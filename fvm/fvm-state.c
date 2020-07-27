
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <WinSock2.h>
#include <Windows.h>
#else
#include <arpa/inet.h>
#endif

#include <stdint.h>

#include "fvm-private.h"
#include <stdarg.h>
#include <stdio.h>
#include <inttypes.h>

#include <fju/log.h>
#include <fju/programs.h>
#include <fju/rpc.h>
#include <fju/raft.h>

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

  fvm_set_args( state, NULL, 0 );
  
  return 0;
}

int fvm_set_args( struct fvm_s *state, char *buf, int len ) {
  uint32_t rescountp, resbufp;
  
  if( len > FVM_MAX_STACK ) return -1;

  /* signature: procedure( int argcount, void *argbuf, int *rescount, void **resbuf ); */
  memcpy( state->stack, buf, len );
  state->reg[FVM_REG_SP] = FVM_ADDR_STACK + len;

  /* allocate space for result buf pointer and result count */
  rescountp = state->reg[FVM_REG_SP];
  fvm_write( state, rescountp, 0 );
  state->reg[FVM_REG_SP] += 4;
  resbufp = state->reg[FVM_REG_SP];
  fvm_write( state, resbufp, 0 );  
  state->reg[FVM_REG_SP] += 4;
  
  fvm_write( state, state->reg[FVM_REG_SP], htonl( len ) );
  state->reg[FVM_REG_SP] += 4;
  fvm_write( state, state->reg[FVM_REG_SP], htonl( FVM_ADDR_STACK ) );
  state->reg[FVM_REG_SP] += 4;

  /* reserve space for rescount/resbuf */
  fvm_write( state, state->reg[FVM_REG_SP], htonl( rescountp ) );
  state->reg[FVM_REG_SP] += 4;
  fvm_write( state, state->reg[FVM_REG_SP], htonl( resbufp ) );
  state->reg[FVM_REG_SP] += 4;

  /* dummy var for return address. this is for consistency so that toplevel procs are the same as internal procs */
  fvm_write( state, state->reg[FVM_REG_SP], 0 );
  state->reg[FVM_REG_SP] += 4;
  
  return 0;
}

int fvm_get_res( struct fvm_s *state, char **buf ) {
  int lenp;
  char *bufp;
  uint32_t addr;
  
  if( buf ) *buf = NULL;

  addr = state->reg[FVM_REG_SP] - 28; // rescount
  lenp = ntohl( fvm_read( state, addr ) );
  addr = state->reg[FVM_REG_SP] - 24; // resbufp
  addr = ntohl( fvm_read( state, addr ) );
  bufp = fvm_getaddr( state, addr );
  if( !bufp ) lenp = 0;

  if( buf ) *buf = bufp;
  return lenp;
}

int fvm_run( struct fvm_s *state, int nsteps ) {
  int sts = 0, ns = 0;
  uint64_t start, end;

  if( state->module->clusterid ) {
    struct raft_cluster cl;
    sts = raft_cluster_by_id( state->module->clusterid, &cl );
    if( sts ) return sts;
    if( !cl.leaderid ) return -1;
  }
  
  sts = 0;
  
  if( nsteps == 0 ) nsteps = fvm_max_steps( 0 );

  start = rpc_now();
  while( !(state->flags & FVM_STATE_YIELD) &&
	 (nsteps == -1 || state->nsteps < nsteps) &&
	 (state->frame >= 0) ) {
    sts = fvm_step( state );
    ns++;
    if( sts ) {
      fvm_printf( "fvm_step returned error status\n" );
      break;
    }
  }
  end = rpc_now();
  fvm_log( LOG_LVL_DEBUG, "fvm_run name=%s progid=%u took %"PRIu64"ms in %u steps", state->module->header.name, state->module->header.progid, end - start, ns );
  
  //if( sts ) return sts;

  /* if clustered then send pings etc */
  if( (state->flags & FVM_STATE_DIRTY) && state->module->clusterid ) {
    fvm_cluster_update( state->module );
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

uint32_t fvm_read_uint32( struct fvm_module *m, uint32_t procid ) {
  uint32_t addr, u32;
  
  if( procid >= m->header.symcount ) return -1;
  if( (m->symbols[procid].flags & FVM_SYMBOL_TYPE_MASK) != FVM_SYMBOL_UINT32 ) return -1;

  addr = m->symbols[procid].addr;
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + m->header.datasize - 4) ) {
    memcpy( &u32, &m->data[addr - FVM_ADDR_DATA], 4 );
    return ntohl( u32 );
  } else if( addr >= FVM_ADDR_TEXT && addr < (FVM_ADDR_TEXT + m->header.textsize - 4) ) {
    memcpy( &u32, &m->text[addr - FVM_ADDR_TEXT], 4 );
    return ntohl( u32 );
  }
  
  return -1;
}

int fvm_write_uint32( struct fvm_module *m, uint32_t procid, uint32_t val ) {
  uint32_t addr;
  
  if( procid >= m->header.symcount ) return -1;
  if( (m->symbols[procid].flags & FVM_SYMBOL_TYPE_MASK) != FVM_SYMBOL_UINT32 ) return -1;

  addr = m->symbols[procid].addr;
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + m->header.datasize - 4) ) {
    val = htonl( val );
    memcpy( &m->data[addr - FVM_ADDR_DATA], &val, 4 );
    if( m->clusterid ) fvm_cluster_update( m );
    return 0;
  }

  return -1;
}

uint64_t fvm_read_uint64( struct fvm_module *m, uint32_t procid ) {
  uint32_t addr;
  uint64_t u64;
  struct xdr_s xdr;
  
  if( procid >= m->header.symcount ) return -1;
  if( (m->symbols[procid].flags & FVM_SYMBOL_TYPE_MASK) != FVM_SYMBOL_UINT64 ) return -1;

  addr = m->symbols[procid].addr;
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + m->header.datasize - 8) ) {
    xdr_init( &xdr, &m->data[addr - FVM_ADDR_DATA], 8 );
    xdr_decode_uint64( &xdr, &u64 );
    return u64;
  } else if( addr >= FVM_ADDR_TEXT && addr < (FVM_ADDR_TEXT + m->header.textsize - 8) ) {
    xdr_init( &xdr, &m->text[addr - FVM_ADDR_TEXT], 8 );
    xdr_decode_uint64( &xdr, &u64 );
    return u64;
  }
  
  return -1;
}

int fvm_write_uint64( struct fvm_module *m, uint32_t procid, uint64_t val ) {
  uint32_t addr;
  struct xdr_s xdr;
  
  if( procid >= m->header.symcount ) return -1;
  if( (m->symbols[procid].flags & FVM_SYMBOL_TYPE_MASK) != FVM_SYMBOL_UINT64 ) return -1;

  addr = m->symbols[procid].addr;
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + m->header.datasize - 8) ) {
    xdr_init( &xdr, &m->data[addr - FVM_ADDR_DATA], 8 );
    xdr_encode_uint64( &xdr, val );
    if( m->clusterid ) fvm_cluster_update( m );    
    return 0;
  }
  
  return -1;
}

int fvm_read_string( struct fvm_module *m, uint32_t procid, char *str, int size ) {
  uint32_t addr, ssize;
  struct xdr_s xdr;

  memset( str, 0, size );
  
  if( procid >= m->header.symcount ) return -1;
  if( (m->symbols[procid].flags & FVM_SYMBOL_TYPE_MASK) != FVM_SYMBOL_STRING ) return -1;

  addr = m->symbols[procid].addr;  
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + m->header.datasize - 4) ) {
    xdr_init( &xdr, &m->data[addr - FVM_ADDR_DATA], 4 );
    xdr_decode_uint32( &xdr, &ssize );
    
    if( addr > (FVM_ADDR_DATA + m->header.datasize - 4 - ssize) ) return -1;

    xdr_init( &xdr, &m->data[addr - FVM_ADDR_DATA], 4 + ssize + (ssize % 4 ? (4 - (ssize % 4)) : 0) );
    xdr_decode_string( &xdr, str, size );    
    return 0;
  } else if( addr >= FVM_ADDR_TEXT && addr < (FVM_ADDR_TEXT + m->header.textsize - 4) ) {
    xdr_init( &xdr, &m->text[addr - FVM_ADDR_TEXT], 4 );
    xdr_decode_uint32( &xdr, &ssize );
    
    if( addr > (FVM_ADDR_TEXT + m->header.textsize - 4 - ssize) ) return -1;

    xdr_init( &xdr, &m->text[addr - FVM_ADDR_TEXT], 4 + ssize + (ssize % 4 ? (4 - (ssize % 4)) : 0) );
    xdr_decode_string( &xdr, str, size );
    return 0;
  }
  
  return -1;
}

int fvm_write_string( struct fvm_module *m, uint32_t procid, char *str ) {
  uint32_t addr, ssize, xsize;
  struct xdr_s xdr;

  ssize = strlen( str );
  xsize = 4 + ssize;
  if( xsize % 4 ) xsize += 4 - (xsize % 4);
  
  if( procid >= m->header.symcount ) return -1;
  if( (m->symbols[procid].flags & FVM_SYMBOL_TYPE_MASK) != FVM_SYMBOL_STRING ) return -1;
  if( (m->symbols[procid].flags & FVM_SYMBOL_SIZE_MASK) < xsize ) return -1;
  
  addr = m->symbols[procid].addr;  
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + m->header.datasize - xsize) ) {
    xdr_init( &xdr, &m->data[addr - FVM_ADDR_DATA], xsize );
    xdr_encode_string( &xdr, str );
    if( m->clusterid ) fvm_cluster_update( m );    
    return 0;
  }
  
  return -1;
}
