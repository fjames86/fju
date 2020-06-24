
#include "fvm2-private.h"


static uint32_t mem_read( struct fvm2_s *state, uint32_t addr ) {
  uint32_t u;
  int n;
  
  if( addr < FVM2_ADDR_DATA ) return 0;
  if( addr >= FVM2_ADDR_DATA && addr < (FVM2_ADDR_DATA + state->datasize) ) {
    n = 4;
    if( (state->datasize - FVM2_ADDR_DATA - addr) < n ) n = (state->datasize - FVM2_ADDR_DATA - addr);
    memcpy( &u, state->data + addr - FVM2_ADDR_DATA, n );
    return u;
  } else if( addr >= FVM2_ADDR_TEXT && addr < (FVM2_ADDR_TEXT + state->textsize) ) {
    n = 4;
    if( (state->textsize - FVM2_ADDR_TEXT - addr) < n ) n = (state->textsize - FVM2_ADDR_TEXT - addr);
    memcpy( &u, state->text + addr - FVM2_ADDR_TEXT, n );
    return u;
  } else if( addr >= FVM2_ADDR_STACK && addr < (FVM2_ADDR_STACK + FVM2_MAX_STACK) ) {
    n = 4;
    if( (FVM2_MAX_STACK - FVM2_ADDR_STACK - addr) < n ) n = (FVM2_MAX_STACK - FVM2_ADDR_STACK - addr);
    memcpy( &u, state->stack + addr - FVM2_ADDR_STACK, n );
    return u;
  }
  
  return 0;
}

static void mem_write( struct fvm2_s *state, uint32_t addr, uint32_t val ) {
  int n;
  
  if( addr >= FVM2_ADDR_DATA && addr < (FVM2_ADDR_DATA + state->datasize) ) {
    n = 4;
    if( (state->datasize - FVM2_ADDR_DATA - addr) < n ) n = (state->datasize - FVM2_ADDR_DATA - addr);
    memcpy( state->data + addr - FVM2_ADDR_DATA, &val, n );
  } else if( addr >= FVM2_ADDR_STACK && addr < (FVM2_ADDR_STACK + FVM2_MAX_STACK) ) {
    n = 4;
    if( (FVM2_MAX_STACK - FVM2_ADDR_DATA - addr) < n ) n = (FVM2_MAX_STACK - FVM2_ADDR_DATA - addr);
    memcpy( state->stack + addr - FVM2_ADDR_STACK, &val, n );
  }
}

static uint32_t sign_extend( uint32_t x ) {
  if( (x >> 15) & 1 ) {
    x |= (0xFFFFFFFF << 16);
  }
  return x;
}

static void setflags( struct fvm2_s *state, uint32_t val ) {
  state->reg[FVM2_REG_FLAGS] = 0;
  if( val > 0) state->reg[FVM2_REG_FLAGS] |= FVM2_FLAG_POS;
  if( val & 0x80000000 ) state->reg[FVM2_REG_FLAGS] |= FVM2_FLAG_NEG;
  if( val == 0 ) state->reg[FVM2_REG_FLAGS] |= FVM2_FLAG_ZERO;
}


typedef int (*fvm2_opcode_fn)( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data );

static int opcode_nop( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return 0;
}

static int opcode_ldreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LD RX RY */
  state->reg[reg] = mem_read( state, state->reg[data & 0x7] );
  return 0;
}

static int opcode_ldconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LD RX const */
  state->reg[reg] = mem_read( state, data & 0xffff );
  return 0;
}

static int opcode_streg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ST RX RY */
  mem_write( state, state->reg[reg], state->reg[data & 0x7] );
  return 0;
}

static int opcode_stconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ST RX const */
  mem_write( state, state->reg[reg], data & 0xffff );
  return 0;
}

static int opcode_ldi( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LDI RX const */
  state->reg[reg] = sign_extend( data );
  return 0;
}

static int opcode_lea( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LEA RX const */
  state->reg[reg] = state->reg[FVM2_REG_PC] + sign_extend( data );
  return 0;
}

static int opcode_pushreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* PUSH RX */
  mem_write( state, state->reg[FVM2_REG_SP], state->reg[reg] );
  state->reg[FVM2_REG_SP] += 4;
  return 0;
}

static int opcode_pushconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* PUSH const */
  mem_write( state, state->reg[FVM2_REG_SP], sign_extend( data ) );
  state->reg[FVM2_REG_SP] += 4;
  return 0;
}

static int opcode_popreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* POP RX */
  state->reg[FVM2_REG_SP] -= 4;
  state->reg[reg] = mem_read( state, state->reg[FVM2_REG_SP] );
  return 0;
}

static int opcode_ret( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  uint32_t addr;
  /* RET */
  state->reg[FVM2_REG_SP] -= 4;
  addr = mem_read( state, state->reg[FVM2_REG_SP] );
  state->reg[FVM2_REG_PC] = addr;
  state->frame--;
  return 0;
}

static int opcode_callreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  mem_write( state, state->reg[FVM2_REG_SP], state->reg[FVM2_REG_PC] );
  state->reg[FVM2_REG_SP] += 4;
  state->reg[FVM2_REG_PC] = state->reg[reg];
  state->frame++;
  return 0;
}

static int opcode_callconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  mem_write( state, state->reg[FVM2_REG_SP], state->reg[FVM2_REG_PC] );
  state->reg[FVM2_REG_SP] += 4;
  state->reg[FVM2_REG_PC] = data & 0xffff;
  state->frame++;
  return 0;  
}


static int opcode_jmpreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[FVM2_REG_PC] = state->reg[reg];
  return 0;
}
static int opcode_jmpconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[FVM2_REG_PC] = data;
  return 0;
}


static int opcode_jzreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & FVM2_FLAG_ZERO ) {
    state->reg[FVM2_REG_PC] = state->reg[reg];
  }  
  return 0;
}
static int opcode_jzconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & FVM2_FLAG_ZERO ) {
    state->reg[FVM2_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jpreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & FVM2_FLAG_POS ) {
    state->reg[FVM2_REG_PC] = state->reg[reg];
  }  
  return 0;
}
static int opcode_jpconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & FVM2_FLAG_POS ) {
    state->reg[FVM2_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jnreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & FVM2_FLAG_NEG ) {
    state->reg[FVM2_REG_PC] = state->reg[reg];
  }  
  return 0;
}

static int opcode_jnconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & FVM2_FLAG_NEG ) {
    state->reg[FVM2_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jpzreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & (FVM2_FLAG_POS|FVM2_FLAG_ZERO) ) {
    state->reg[FVM2_REG_PC] = state->reg[reg];
  }  
  return 0;
}
static int opcode_jpzconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & (FVM2_FLAG_POS|FVM2_FLAG_ZERO) ) {
    state->reg[FVM2_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jpnreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & (FVM2_FLAG_POS|FVM2_FLAG_NEG) ) {
    state->reg[FVM2_REG_PC] = state->reg[reg];
  }  
  return 0;
}
static int opcode_jpnconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & (FVM2_FLAG_POS|FVM2_FLAG_NEG) ) {
    state->reg[FVM2_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jnzreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & (FVM2_FLAG_NEG|FVM2_FLAG_ZERO) ) {
    state->reg[FVM2_REG_PC] = state->reg[reg];
  }  
  return 0;
}
static int opcode_jnzconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & (FVM2_FLAG_NEG|FVM2_FLAG_ZERO) ) {
    state->reg[FVM2_REG_PC] = data;
  }    
  return 0;
}


static int opcode_addreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ADD RX RY */
  state->reg[reg] += state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_addconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ADD RX const */
  state->reg[reg] += sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_subreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SUB RX RY */
  state->reg[reg] -= state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_subconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SUB RX const */
  state->reg[reg] -= sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_mulreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MUL RX RY */
  state->reg[reg] *= state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_mulconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MUL RX const */
  state->reg[reg] *= sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_divreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* DIV RX RY */
  if( state->reg[data & 0x7] == 0 ) return -1;
  state->reg[reg] /= state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_divconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* DIV RX const */
  if( data == 0 ) return -1;
  state->reg[reg] /= sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_modreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MOD RX RY */
  if( state->reg[data & 0x7] == 0 ) return -1;
  state->reg[reg] = state->reg[reg] % state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_modconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MOD RX const */
  if( data == 0 ) return -1;
  state->reg[reg] = state->reg[reg] % sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}

static int opcode_andreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* AND RX RY */
  state->reg[reg] &= state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_andconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* AND RX const */
  state->reg[reg] &= sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_orreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* OR RX RY */
  state->reg[reg] |= state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_orconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* OR RX const */
  state->reg[reg] |= sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_xorreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* XOR RX RY */
  state->reg[reg] ^= state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_xorconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* XOR RX const */
  state->reg[reg] ^= sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_notreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[reg] = ~state->reg[reg];
  setflags( state, state->reg[reg] );
  return 0;
}

static int opcode_shlreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHL RX RY */
  state->reg[reg] <<= state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_shlconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHL RX const */
  state->reg[reg] <<= sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_shrreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHR RX RY */
  state->reg[reg] >>= state->reg[data & 0x7];
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_shrconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHR RX const */
  state->reg[reg] >>= sign_extend( data );
  setflags( state, state->reg[reg] );
  return 0;
}

static int opcode_rolreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX RY */
  uint32_t bit = (state->reg[reg] & 0x8000000) ? 1 : 0;
  state->reg[reg] <<= state->reg[data & 0x7];
  state->reg[reg] |= bit;
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_rolconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX const */
  uint32_t bit = (state->reg[reg] & 0x8000000) ? 1 : 0;
  state->reg[reg] <<= sign_extend( data );
  state->reg[reg] |= bit;
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_rorreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX RY */
  uint32_t bit = (state->reg[reg] & 0x0000001) ? 0x8000000 : 0;
  state->reg[reg] >>= state->reg[data & 0x7];
  state->reg[reg] |= bit;
  setflags( state, state->reg[reg] );
  return 0;
}
static int opcode_rorconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX const */
  uint32_t bit = (state->reg[reg] & 0x0000001) ? 0x8000000 : 0;
  state->reg[reg] >>= sign_extend( data );
  state->reg[reg] |= bit;
  setflags( state, state->reg[reg] );
  return 0;
}


static int opcode_callvirt( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return 0;
}

static int opcode_ldvirt( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return 0;
}

static int opcode_stvirt( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return 0;
}

static int opcode_leasp( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return 0;
}

static int opcode_allocareg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return 0;
}

static int opcode_allocaconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return 0;
}



static fvm2_opcode_fn opcodes[FVM2_MAX_OPCODE] =
  {
   opcode_nop,
   opcode_ldreg,
   opcode_ldconst,
   opcode_streg,
   opcode_stconst,
   opcode_ldi,
   opcode_lea,
   opcode_pushreg,
   opcode_pushconst,
   opcode_popreg,
   opcode_ret,
   opcode_callreg,
   opcode_callconst,
   opcode_jmpreg,
   opcode_jmpconst,
   opcode_jzreg,
   opcode_jzconst,
   opcode_jpreg,
   opcode_jpconst,
   opcode_jnreg,
   opcode_jnconst,
   opcode_jpzreg,
   opcode_jpzconst,
   opcode_jpnreg,
   opcode_jpnconst,
   opcode_jnzreg,
   opcode_jnzconst,
   opcode_addreg,
   opcode_addconst,
   opcode_subreg,
   opcode_subconst,
   opcode_mulreg,
   opcode_mulconst,
   opcode_divreg,
   opcode_divconst,
   opcode_modreg,
   opcode_modconst,
   opcode_andreg,
   opcode_andconst,
   opcode_orreg,
   opcode_orconst,
   opcode_xorreg,
   opcode_xorconst,
   opcode_notreg,
   opcode_shlreg,
   opcode_shlconst,
   opcode_shrreg,
   opcode_shrconst,
   opcode_rolreg,
   opcode_rolconst,
   opcode_rorreg,
   opcode_rorconst,
   opcode_callvirt,
   opcode_ldvirt,
   opcode_stvirt,
   opcode_leasp,
   opcode_allocareg,
   opcode_allocaconst,


  };



int fvm2_step( struct fvm2_s *state ) {
  /* fetch next instruction */
  fvm2_opcode_fn fn;
  uint32_t opcode;
  
  opcode = mem_read( state, state->reg[FVM2_REG_PC] );
  fn = opcodes[(opcode >> 24) & 0xff];
  if( !fn ) return -1;
  
  state->reg[FVM2_REG_PC]++;  
  return fn( state, (opcode & 0x00f00000) >> 20, (opcode & 0x000f0000) >> 16, opcode & 0x0000ffff );
}




