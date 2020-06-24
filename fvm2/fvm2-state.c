
#include "fvm2-private.h"

int fvm2_state_init( char *module, char *fname, char *args, int argsize, struct fvm2_s *state ) {
  struct fvm2_module *m;
  uint32_t addr;
  
  m = fvm2_module_by_name( module );
  if( !m ) return -1;

  addr = fvm2_symbol_addr( m, fname );
  if( !addr ) return -1;

  memset( state, 0, sizeof(*state) );
  state->module = m;
  state->reg[FVM2_REG_PC] = addr;
  state->reg[FVM2_REG_SP] = FVM2_ADDR_STACK;
  
  return 0;
}

int fvm2_run( struct fvm2_s *state, int nsteps ) {
  int i, sts;

  i = 0;
  while( i < nsteps && state->frame >= 0 ) {
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

