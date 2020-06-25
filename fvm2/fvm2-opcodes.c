
#include "fvm2-private.h"

#include <arpa/inet.h>

static uint32_t default_maxsteps = -1;

static uint32_t mem_read( struct fvm2_s *state, uint32_t addr ) {
  uint32_t u;
  int n;
  
  if( addr < FVM2_ADDR_DATA ) return 0;
  if( addr >= FVM2_ADDR_DATA && addr < (FVM2_ADDR_DATA + state->datasize) ) {
    n = 4;
    if( (state->datasize - FVM2_ADDR_DATA - addr) < n ) n = (state->datasize - FVM2_ADDR_DATA - addr);
    memcpy( &u, state->data + addr - FVM2_ADDR_DATA, n );
    return u;
  } else if( (addr >= FVM2_ADDR_TEXT) && (addr < (FVM2_ADDR_TEXT + state->textsize)) ) {
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

uint32_t fvm2_read( struct fvm2_s *state, uint32_t addr ) {
  return mem_read( state, addr );
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

void fvm2_write( struct fvm2_s *state, uint32_t addr, uint32_t val ) {
  mem_write( state, addr, val );
}

static uint32_t sign_extend( uint32_t x ) {
  if( (x >> 15) & 1 ) {
    x |= (0xFFFFFFFF << 16);
  }
  return x;
}

static void setflags( struct fvm2_s *state, uint32_t val ) {
  int32_t i32 = (int32_t)val;
  state->reg[FVM2_REG_FLAGS] = 0;
  if( i32 > 0) state->reg[FVM2_REG_FLAGS] |= FVM2_FLAG_POS;
  if( i32 < 0 ) state->reg[FVM2_REG_FLAGS] |= FVM2_FLAG_NEG;
  if( i32 == 0 ) state->reg[FVM2_REG_FLAGS] |= FVM2_FLAG_ZERO;
}

char *fvm2_getaddr( struct fvm2_s *state, uint32_t addr ) {
  if( addr >= FVM2_ADDR_DATA && addr < (FVM2_ADDR_DATA + state->datasize) ) {
    return (char *)&state->data[addr - FVM2_ADDR_DATA];
  }
  if( addr >= FVM2_ADDR_TEXT && addr < (FVM2_ADDR_TEXT + state->textsize) ) {
    return (char *)&state->text[addr - FVM2_ADDR_TEXT];
  }
  if( addr >= FVM2_ADDR_STACK && addr < (FVM2_ADDR_STACK + FVM2_MAX_STACK) ) {
    return (char *)&state->stack[addr - FVM2_ADDR_STACK];
  }
  return NULL;
}


typedef int (*fvm2_opcode_fn)( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data );

static int opcode_nop( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return 0;
}

static int opcode_ldreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LD RX RY */
  state->reg[reg] = mem_read( state, ntohl( state->reg[data & 0x7] ) );
  return 0;
}

static int opcode_ldconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LD RX const */
  state->reg[reg] = mem_read( state, data & 0xffff );
  return 0;
}

static int opcode_streg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ST RX RY */
  mem_write( state, state->reg[reg], ntohl( state->reg[data & 0x7] ) );
  return 0;
}

static int opcode_stconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ST RX const */
  mem_write( state, ntohl( state->reg[reg] ), htonl(sign_extend( data & 0xffff) ) );
  return 0;
}

static int opcode_ldi( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LDI RX const */
  state->reg[reg] = htonl(sign_extend( data ));
  return 0;
}

static int opcode_lea( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LEA RX const */
  state->reg[reg] = htonl(ntohl(state->reg[FVM2_REG_PC]) + sign_extend( data ));
  return 0;
}

void fvm2_push( struct fvm2_s *state, uint32_t val ) {
  /* PUSH RX */
  if( state->reg[FVM2_REG_SP] >= (FVM2_ADDR_STACK + FVM2_MAX_STACK) ) return;
  mem_write( state, state->reg[FVM2_REG_SP], val );
  state->reg[FVM2_REG_SP] += 4;  
}

uint32_t fvm2_pop( struct fvm2_s *state ) {
  if( state->reg[FVM2_REG_SP] <= FVM2_ADDR_STACK ) return -1;
  state->reg[FVM2_REG_SP] -= 4;
  return mem_read( state, state->reg[FVM2_REG_SP] );  
}

static int opcode_pushreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* PUSH RX */
  if( state->reg[FVM2_REG_SP] >= (FVM2_ADDR_STACK + FVM2_MAX_STACK) ) return -1;
  
  mem_write( state, state->reg[FVM2_REG_SP], state->reg[reg] );
  state->reg[FVM2_REG_SP] += 4;
  return 0;
}

static int opcode_pushconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* PUSH const */
  if( state->reg[FVM2_REG_SP] >= (FVM2_ADDR_STACK + FVM2_MAX_STACK) ) return -1;
  
  mem_write( state, state->reg[FVM2_REG_SP], htonl(sign_extend( data )) );
  state->reg[FVM2_REG_SP] += 4;
  return 0;
}

static int opcode_popreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* POP RX */
  if( state->reg[FVM2_REG_SP] <= FVM2_ADDR_STACK ) return -1;
  state->reg[FVM2_REG_SP] -= 4;
  state->reg[reg] = mem_read( state, state->reg[FVM2_REG_SP] );
  return 0;
}

static int opcode_ret( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* RET */
  if( state->reg[FVM2_REG_SP] >= (FVM2_ADDR_STACK + 4) ) {
    state->reg[FVM2_REG_SP] -= 4;
    state->reg[FVM2_REG_PC] = ntohl(mem_read( state, state->reg[FVM2_REG_SP] ));
  }
  state->frame--;
  return 0;
}

static int opcode_callreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  mem_write( state, state->reg[FVM2_REG_SP], htonl(state->reg[FVM2_REG_PC]) );
  state->reg[FVM2_REG_SP] += 4;
  state->reg[FVM2_REG_PC] = ntohl(state->reg[reg]);
  state->frame++;
  return 0;
}

static int opcode_callconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  mem_write( state, state->reg[FVM2_REG_SP], htonl(state->reg[FVM2_REG_PC]) );
  state->reg[FVM2_REG_SP] += 4;
  state->reg[FVM2_REG_PC] = data & 0xffff;
  state->frame++;
  return 0;  
}


static int opcode_jmpreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[FVM2_REG_PC] = ntohl(state->reg[reg]);
  return 0;
}
static int opcode_jmpconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[FVM2_REG_PC] = data;
  return 0;
}


static int opcode_jzreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM2_REG_FLAGS] & FVM2_FLAG_ZERO ) {
    state->reg[FVM2_REG_PC] = ntohl(state->reg[reg]);
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
    state->reg[FVM2_REG_PC] = ntohl(state->reg[reg]);
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
    state->reg[FVM2_REG_PC] = ntohl(state->reg[reg]);
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
    state->reg[FVM2_REG_PC] = ntohl(state->reg[reg]);
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
    state->reg[FVM2_REG_PC] = ntohl(state->reg[reg]);
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
    state->reg[FVM2_REG_PC] = ntohl(state->reg[reg]);
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
  state->reg[reg] = htonl(ntohl(state->reg[reg]) + ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_addconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ADD RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) + sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_subreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SUB RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) - ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_subconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SUB RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) - sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_mulreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MUL RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) * ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_mulconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MUL RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) * sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_divreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* DIV RX RY */
  if( htonl(state->reg[data & 0x7]) == 0 ) return -1;
  state->reg[reg] = htonl(ntohl(state->reg[reg]) / ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_divconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* DIV RX const */
  if( data == 0 ) return -1;
  state->reg[reg] = htonl(ntohl(state->reg[reg]) / sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_modreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MOD RX RY */
  if( ntohl(state->reg[data & 0x7]) == 0 ) return -1;
  state->reg[reg] = htonl(ntohl(state->reg[reg]) % ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_modconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MOD RX const */
  if( data == 0 ) return -1;
  state->reg[reg] = htonl(ntohl(state->reg[reg]) % sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}

static int opcode_andreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* AND RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) & ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_andconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* AND RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) & sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_orreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* OR RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) | ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_orconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* OR RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) | sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_xorreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* XOR RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) ^ ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_xorconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* XOR RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) ^ sign_extend( data ));
  setflags( state, ntohl(state->reg[reg] ));
  return 0;
}
static int opcode_notreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[reg] = ~state->reg[reg];
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}

static int opcode_shlreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHL RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) << ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_shlconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHL RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) << sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_shrreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHR RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) >> ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_shrconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHR RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) >> sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}

static int opcode_rolreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX RY */
  uint32_t r, bit;
  r = ntohl(state->reg[reg]);
  bit = (r & 0x8000000) ? 1 : 0;
  state->reg[reg] = htonl((r << ntohl(state->reg[data & 0x7])) | bit);
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_rolconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX const */
  uint32_t r, bit;
  r = ntohl(state->reg[reg]);
  bit = (r & 0x8000000) ? 1 : 0;
  state->reg[reg] = htonl((r << data) | bit);
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_rorreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX RY */
  uint32_t r, bit;
  r = ntohl(state->reg[reg]);
  bit = (r & 0x0000001) ? 0x8000000 : 0;
  state->reg[reg] = htonl((r >> ntohl(state->reg[data & 0x7])) | bit);
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_rorconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX const */
  uint32_t r, bit;
  r = ntohl(state->reg[reg]);
  bit = (r & 0x0000001) ? 0x8000000 : 0;
  state->reg[reg] = htonl((r >> data) | bit);
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}

static int opcode_callvirt( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* CALLVIRT RX RY RZ */
  /* RX contains progid. RY contains proc index (index into symbol table).
   * RZ contains arg length, receives result length. arg/res on stack 
   */
  uint32_t addr; 
  int sts;
  struct fvm2_module *m;
  uint32_t rx, ry, rz;
  struct fvm2_s state2;
  char *args, *res;
  int argsize, ressize;
  uint32_t sp;
  
  rx = reg;
  ry = data & 0x7;
  rz = (data >> 4) & 0x7;

  argsize = ntohl( state->reg[rz] );
  if( (argsize > (state->reg[FVM2_REG_SP] - FVM2_ADDR_STACK)) ) {
    fvm2_printf( "args too large %d > %u \n", argsize, state->reg[FVM2_REG_SP] - FVM2_ADDR_STACK );
    return -1;
  }
  
  sp = state->reg[FVM2_REG_SP] - argsize;
  args = (char *)&state->stack[sp - FVM2_ADDR_STACK];
  res = (char *)&state->stack[sp - FVM2_ADDR_STACK];
  ressize = FVM2_MAX_STACK - (sp - FVM2_ADDR_STACK);

  if( ntohl( state->reg[rx] ) == 0 ) {
    /* special code for standard libary call */
    fvm2_printf( "callvirt native %u\n", ntohl( state->reg[ry] ) );
    sts = fvm2_native_call( state, ntohl( state->reg[ry] ) );
    if( sts ) {
      state->reg[rz] = htonl( -1 );
      return 0;
    }
    state->reg[rz] = 0;
    state->reg[FVM2_REG_SP] = sp;
    return 0;
  }
  
  m = fvm2_module_by_progid( ntohl( state->reg[rx] ) );
  if( !m ) {
    fvm2_printf( "callvirt unknown progid %u\n", ntohl( state->reg[rx] ) );
    state->reg[rz] = htonl( -1 );
    return 0;
  }

  addr = fvm2_symbol_by_index( m, ntohl( state->reg[ry] ) );
  if( addr == 0 ) {
    fvm2_printf( "callvirt unknown procid %u\n", ntohl( state->reg[ry] ) );
    state->reg[rz] = htonl( -1 );
    return 0;
  }

  memset( &state2, 0, sizeof(state2) );
  state2.module = m;
  state2.datasize = m->header.datasize;
  state2.textsize = m->header.textsize;
  state2.data = m->data;
  state2.text = m->text;
  state2.reg[FVM2_REG_PC] = addr;
  /* copy args onto stack */
  memcpy( state2.stack, args, argsize );
  state2.reg[FVM2_REG_R0] = htonl( argsize );
  state2.reg[FVM2_REG_SP] = FVM2_ADDR_STACK + argsize;
  sts = fvm2_run( &state2, default_maxsteps ); 
  if( sts ) {
    fvm2_printf( "callvirt run failed\n" );
    state->reg[rz] = htonl( -1 );
    return 0;
  }

  /* extract result data. Calling convention is R0 contains result length.  */
  ressize = ntohl( state2.reg[FVM2_REG_R0] );
  if( ressize == -1 ) {
    fvm2_printf( "callvirt return failure\n" );
    state->reg[rz] = htonl( -1 );
    return 0;
  }

  memcpy( res, state2.stack, ressize );
  state->reg[rz] = htonl( ressize );
  fvm2_printf( "callvirt success\n" );
  return 0;
}

static int opcode_ldvirt( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  uint32_t rx, ry, rz;
  struct fvm2_s state2;
  int sts;
  
  rx = reg;
  ry = data & 0x7;
  rz = (data >> 4) & 0x7;

  sts = fvm2_state_init2( &state2, ntohl( state->reg[rx] ), ntohl( state->reg[ry] ) );
  if( sts ) {
    fvm2_printf( "ldvirt failed to init\n" );
  }
  
  state->reg[rz] = fvm2_read( &state2, state2.reg[FVM2_REG_PC] );
  return 0;
}

static int opcode_stvirt( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  uint32_t rx, ry, rz;
  struct fvm2_s state2;
  int sts;
  
  rx = reg;
  ry = data & 0x7;
  rz = (data >> 4) & 0x7;

  sts = fvm2_state_init2( &state2, ntohl( state->reg[rx] ), ntohl( state->reg[ry] ) );
  if( sts ) {
    fvm2_printf( "ldvirt failed to init\n" );
  }

  fvm2_write( &state2, state2.reg[FVM2_REG_PC], state->reg[rz] );
  return 0;
}

static int opcode_leasp( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LEASP RX const */
  state->reg[reg] = htonl( state->reg[FVM2_REG_SP] + sign_extend( data ) );
  return 0;
}

static int opcode_allocareg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ALLOCA RX */
  state->reg[FVM2_REG_SP] += ntohl( state->reg[reg] );
  return 0;
}

static int opcode_allocaconst( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ALLOCA const */
  state->reg[FVM2_REG_SP] += sign_extend( data );
  return 0;
}

static int opcode_movreg( struct fvm2_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[reg] = state->reg[data & 0x7];
  return 0;
}


struct opcode_def {
  fvm2_opcode_fn fn;
  char *name;
};
 
static struct opcode_def opcodes[FVM2_MAX_OPCODE] =
  {
   { opcode_nop, "NOP" },
   { opcode_ldreg, "LD" },
   { opcode_ldconst, "LD" },
   { opcode_streg, "ST" },
   { opcode_stconst, "ST" },
   { opcode_ldi, "LDI" },
   { opcode_lea, "LEA" },
   { opcode_pushreg, "PUSH" },
   { opcode_pushconst, "PUSH" },
   { opcode_popreg, "POP" },
   { opcode_ret, "RET" },
   { opcode_callreg, "CALL" },
   { opcode_callconst, "CALL" },
   { opcode_jmpreg, "JMP" },
   { opcode_jmpconst, "JMP" },
   { opcode_jzreg, "JZ" },
   { opcode_jzconst, "JZ" },
   { opcode_jpreg, "JP" },
   { opcode_jpconst, "JP" },
   { opcode_jnreg, "JN" },
   { opcode_jnconst, "JN" },
   { opcode_jpzreg, "JPZ" },
   { opcode_jpzconst, "JPZ" },
   { opcode_jpnreg, "JPN" },
   { opcode_jpnconst, "JPN" },
   { opcode_jnzreg, "JNZ" },
   { opcode_jnzconst, "JNZ" },
   { opcode_addreg, "ADD" },
   { opcode_addconst, "ADD" },
   { opcode_subreg, "SUB" },
   { opcode_subconst, "SUB" },
   { opcode_mulreg, "MUL" },
   { opcode_mulconst, "MUL" },
   { opcode_divreg, "DIV" },
   { opcode_divconst, "DIV" },
   { opcode_modreg, "MOD" },
   { opcode_modconst, "MOD" },
   { opcode_andreg, "AND" },
   { opcode_andconst, "AND" },
   { opcode_orreg, "OR" },
   { opcode_orconst, "OR" },
   { opcode_xorreg, "XOR" },
   { opcode_xorconst, "XOR" },
   { opcode_notreg, "NOT" },
   { opcode_shlreg, "SHL" },
   { opcode_shlconst, "SHL" },
   { opcode_shrreg, "SHR" },
   { opcode_shrconst, "SHR" },
   { opcode_rolreg, "ROL" },
   { opcode_rolconst, "ROL" },
   { opcode_rorreg, "ROR" },
   { opcode_rorconst, "ROR" },
   { opcode_callvirt, "CALLVIRT" },
   { opcode_ldvirt, "LDVIRT" },
   { opcode_stvirt, "STVIRT" },
   { opcode_leasp, "LEASP" },
   { opcode_allocareg, "ALLOCA" },
   { opcode_allocaconst, "ALLOCA" },
   { opcode_movreg, "MOV" },

  };



int fvm2_step( struct fvm2_s *state ) {
  /* fetch next instruction */
  struct opcode_def *def;
  uint32_t opcode, pc;

  pc = state->reg[FVM2_REG_PC];
  if( !(pc >= FVM2_ADDR_TEXT && pc < (FVM2_ADDR_TEXT + FVM2_MAX_TEXT)) ) return -1;
  
  opcode = ntohl(mem_read( state, state->reg[FVM2_REG_PC] ));
  def = &opcodes[(opcode >> 24) & 0xff];
  if( !def->fn ) return -1;

  fvm2_printf( "%-16s Frame %-2u R0 %-4x R1 %-4x R2 %-4x R3 %-4x R4 %-4x R5 %-4x R6 %-4x R7 %-4x SP %-4x PC %-4x : %08x %-6s R%u 0x%04x (%d)\n",
	  state->module->header.name,
	  state->frame,
	  ntohl(state->reg[FVM2_REG_R0]),
	  ntohl(state->reg[FVM2_REG_R1]),
	  ntohl(state->reg[FVM2_REG_R2]),
	  ntohl(state->reg[FVM2_REG_R3]),
	  ntohl(state->reg[FVM2_REG_R4]),
	  ntohl(state->reg[FVM2_REG_R5]),
	  ntohl(state->reg[FVM2_REG_R6]),
	  ntohl(state->reg[FVM2_REG_R7]),
	  state->reg[FVM2_REG_SP],
    	  state->reg[FVM2_REG_PC],
	  opcode,
	  def->name,
	  (opcode & 0x00700000) >> 20,
	  opcode & 0xffff,
	  sign_extend( opcode & 0xffff ) );
  
  state->reg[FVM2_REG_PC] += 4;  
  return def->fn( state, (opcode & 0x000f0000) >> 16, (opcode & 0x00700000) >> 20, opcode & 0x0000ffff );
}





uint32_t fvm2_max_steps( uint32_t n ) {
  uint32_t old = default_maxsteps;
  if( n ) default_maxsteps = n;
  return old;
}
