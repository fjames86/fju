
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <WinSock2.h>
#include <Windows.h>
#else
#include <arpa/inet.h>
#endif

#include "fvm-private.h"
#include <stdarg.h>
#include <stdio.h>
#include <fju/log.h>
#include <fju/programs.h>

int fvm_state_init( struct fvm_s *state, uint32_t progid, uint32_t procid ) {
  struct fvm_module *m;
  uint32_t addr;
  
  m = fvm_module_by_progid( progid );  
  if( !m ) return -1;

  addr = fvm_symbol_by_index( m, procid );
  if( !addr ) return -1;

  memset( state, 0, sizeof(*state) );
  state->module = m;
  state->datasize = m->header.datasize;
  state->textsize = m->header.textsize;
  state->data = m->data;
  state->text = m->text;
  state->reg[FVM_REG_PC] = addr;
  state->reg[FVM_REG_SP] = FVM_ADDR_STACK;
  
  return 0;
}

int fvm_set_args( struct fvm_s *state, char *buf, int len ) {
  if( len > FVM_MAX_STACK ) return -1;
  
  memcpy( state->stack, buf, len );
  state->reg[FVM_REG_SP] = FVM_ADDR_STACK + len;
  state->reg[FVM_REG_R0] = htonl( len );
  state->reg[FVM_REG_R1] = ntohl( FVM_ADDR_STACK + len); /* for consistency with getres */
  
  return 0;
}

int fvm_get_res( struct fvm_s *state, char **buf ) {
  int lenp;
  char *bufp;
  
  if( buf ) *buf = NULL;
  
  lenp = ntohl( state->reg[FVM_REG_R0] );
  bufp = fvm_getaddr( state, ntohl( state->reg[FVM_REG_R1] ) );
  if( !bufp ) lenp = 0;

  if( buf ) *buf = bufp;
  return lenp;
}

int fvm_run( struct fvm_s *state, int nsteps ) {
  int sts = 0;

  while( (nsteps == -1 || state->nsteps < nsteps) && state->frame >= 0 ) {
    sts = fvm_step( state );
    if( sts ) break;
  }

  if( sts ) return sts;

  /* if clustered then send pings etc */
  if( (state->flags & FVM_STATE_DIRTY) && state->module->clusterid ) {
    fvm_cluster_update( state );
  }
  state->flags &= ~FVM_STATE_DIRTY;
  
  return 0;
}

static int debugenabled = 0;
void fvm_debug( int enabled ) {
  debugenabled = enabled;
}

void fvm_printf( char *fmt, ... ) {
  va_list args;

  if( !debugenabled ) return;
  
  va_start( args, fmt );
  vprintf( fmt, args );
  va_end( args );
}

static struct log_s fvmlog;
void fvm_log( int lvl, char *fmt, ... ) {
  static int initialized = 0;
  va_list args;
  
  if( !initialized ) {
    log_open( NULL, NULL, &fvmlog );
    fvmlog.ltag = FVM_RPC_PROG;
    initialized = 1;
  }

  va_start( args, fmt );
  log_writev( &fvmlog, lvl, fmt, args );
  va_end( args );
}
