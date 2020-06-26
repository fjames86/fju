
#include "fvm-private.h"
#include <stdarg.h>
#include <stdio.h>

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

int fvm_run( struct fvm_s *state, int nsteps ) {
  int sts = 0;

  while( (nsteps == -1 || state->nsteps < nsteps) && state->frame >= 0 ) {
    sts = fvm_step( state );
    if( sts ) break;
  }

  return sts;
}

int fvm_results( struct fvm_s *state, char *results, int size, int *rsize ) {
  memcpy( results, &state->stack[state->reg[FVM_REG_SP] - size], size );
  return 0;
}

int fvm_read_var( char *module, char *vname, char *buf, int size ) {
  struct fvm_module *m;
  uint32_t addr;
  
  m = fvm_module_by_name( module );
  if( !m ) return -1;
  addr = fvm_symbol_addr( m, vname );
  if( !addr ) return -1;

  if( addr < FVM_ADDR_DATA || (addr + size) >= (FVM_ADDR_DATA + m->header.datasize) ) return -1;

  memcpy( buf, &m->data + (addr - FVM_ADDR_DATA), size );
  return 0;  
}

int fvm_write_var( char *module, char *vname, char *buf, int size ) {
  struct fvm_module *m;
  uint32_t addr;
  
  m = fvm_module_by_name( module );
  if( !m ) return -1;
  addr = fvm_symbol_addr( m, vname );
  if( !addr ) return -1;

  if( addr < FVM_ADDR_DATA || (addr + size) >= (FVM_ADDR_DATA + m->header.datasize) ) return -1;

  memcpy( (char *)&m->data + (addr - FVM_ADDR_DATA), buf, size );
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
