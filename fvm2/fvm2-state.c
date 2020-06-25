
#include "fvm2-private.h"
#include <stdarg.h>
#include <stdio.h>

int fvm2_state_init( struct fvm2_s *state, uint32_t progid, uint32_t procid ) {
  struct fvm2_module *m;
  uint32_t addr;
  
  m = fvm2_module_by_progid( progid );  
  if( !m ) return -1;

  addr = fvm2_symbol_by_index( m, procid );
  if( !addr ) return -1;

  memset( state, 0, sizeof(*state) );
  state->module = m;
  state->datasize = m->header.datasize;
  state->textsize = m->header.textsize;
  state->data = m->data;
  state->text = m->text;
  state->reg[FVM2_REG_PC] = addr;
  state->reg[FVM2_REG_SP] = FVM2_ADDR_STACK;
  
  return 0;
}

int fvm2_run( struct fvm2_s *state, int nsteps ) {
  int i, sts = 0;

  i = 0;
  while( (nsteps == -1 || i < nsteps) && state->frame >= 0 ) {
    sts = fvm2_step( state );
    if( sts ) break;
    i++;
  }

  return sts;
}

int fvm2_results( struct fvm2_s *state, char *results, int size, int *rsize ) {
  memcpy( results, &state->stack[state->reg[FVM2_REG_SP] - size], size );
  return 0;
}

int fvm2_read_var( char *module, char *vname, char *buf, int size ) {
  struct fvm2_module *m;
  uint32_t addr;
  
  m = fvm2_module_by_name( module );
  if( !m ) return -1;
  addr = fvm2_symbol_addr( m, vname );
  if( !addr ) return -1;

  if( addr < FVM2_ADDR_DATA || (addr + size) >= (FVM2_ADDR_DATA + m->header.datasize) ) return -1;

  memcpy( buf, &m->data + (addr - FVM2_ADDR_DATA), size );
  return 0;  
}

int fvm2_write_var( char *module, char *vname, char *buf, int size ) {
  struct fvm2_module *m;
  uint32_t addr;
  
  m = fvm2_module_by_name( module );
  if( !m ) return -1;
  addr = fvm2_symbol_addr( m, vname );
  if( !addr ) return -1;

  if( addr < FVM2_ADDR_DATA || (addr + size) >= (FVM2_ADDR_DATA + m->header.datasize) ) return -1;

  memcpy( (char *)&m->data + (addr - FVM2_ADDR_DATA), buf, size );
  return 0;
}

static int debugenabled = 0;
void fvm2_debug( int enabled ) {
  debugenabled = enabled;
}

void fvm2_printf( char *fmt, ... ) {
  va_list args;

  if( !debugenabled ) return;
  
  va_start( args, fmt );
  vprintf( fmt, args );
  va_end( args );
}
