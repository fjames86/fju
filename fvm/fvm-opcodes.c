
#ifdef WIN32
#include <WinSock2.h>
#include <Windows.h>
#else
#include <arpa/inet.h>
#endif

#include <stdint.h>

#include "fvm-private.h"

static uint32_t default_maxsteps = -1;

static uint32_t mem_read( struct fvm_s *state, uint32_t addr ) {
  uint32_t u;
  int n;
  
  if( addr < FVM_ADDR_DATA ) return 0;
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + state->datasize) ) {
    n = 4;
    if( (state->datasize - FVM_ADDR_DATA - addr) < n ) n = (state->datasize - FVM_ADDR_DATA - addr);
    memcpy( &u, state->data + addr - FVM_ADDR_DATA, n );
    return u;
  } else if( (addr >= FVM_ADDR_TEXT) && (addr < (FVM_ADDR_TEXT + state->textsize)) ) {
    n = 4;
    if( (state->textsize - FVM_ADDR_TEXT - addr) < n ) n = (state->textsize - FVM_ADDR_TEXT - addr);
    memcpy( &u, state->text + addr - FVM_ADDR_TEXT, n );
    return u;
  } else if( addr >= FVM_ADDR_STACK && addr < (FVM_ADDR_STACK + FVM_MAX_STACK) ) {
    n = 4;
    if( (FVM_MAX_STACK - FVM_ADDR_STACK - addr) < n ) n = (FVM_MAX_STACK - FVM_ADDR_STACK - addr);
    memcpy( &u, state->stack + addr - FVM_ADDR_STACK, n );
    return u;
  }
  
  return 0;
}

uint32_t fvm_read( struct fvm_s *state, uint32_t addr ) {
  return mem_read( state, addr );
}

static void mem_write( struct fvm_s *state, uint32_t addr, uint32_t val ) {
  int n;
  
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + state->datasize) ) {
    n = 4;
    if( (state->datasize - FVM_ADDR_DATA - addr) < n ) n = (state->datasize - FVM_ADDR_DATA - addr);
    memcpy( state->data + addr - FVM_ADDR_DATA, &val, n );
    state->flags |= FVM_STATE_DIRTY;
  } else if( addr >= FVM_ADDR_STACK && addr < (FVM_ADDR_STACK + FVM_MAX_STACK) ) {
    n = 4;
    if( (FVM_MAX_STACK - FVM_ADDR_DATA - addr) < n ) n = (FVM_MAX_STACK - FVM_ADDR_DATA - addr);
    memcpy( state->stack + addr - FVM_ADDR_STACK, &val, n );
  }
}

void fvm_write( struct fvm_s *state, uint32_t addr, uint32_t val ) {
  mem_write( state, addr, val );
}

static uint32_t sign_extend( uint32_t x ) {
  if( (x >> 15) & 1 ) {
    x |= (0xFFFFFFFF << 16);
  }
  return x;
}

static void setflags( struct fvm_s *state, uint32_t val ) {
  int32_t i32 = (int32_t)val;
  state->reg[FVM_REG_FLAGS] = 0;
  if( i32 > 0) state->reg[FVM_REG_FLAGS] |= FVM_FLAG_POS;
  if( i32 < 0 ) state->reg[FVM_REG_FLAGS] |= FVM_FLAG_NEG;
  if( i32 == 0 ) state->reg[FVM_REG_FLAGS] |= FVM_FLAG_ZERO;
}

char *fvm_getaddr( struct fvm_s *state, uint32_t addr ) {
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + state->datasize) ) {
    return (char *)&state->data[addr - FVM_ADDR_DATA];
  }
  if( addr >= FVM_ADDR_TEXT && addr < (FVM_ADDR_TEXT + state->textsize) ) {
    return (char *)&state->text[addr - FVM_ADDR_TEXT];
  }
  if( addr >= FVM_ADDR_STACK && addr < (FVM_ADDR_STACK + FVM_MAX_STACK) ) {
    return (char *)&state->stack[addr - FVM_ADDR_STACK];
  }
  return NULL;
}


typedef int (*fvm_opcode_fn)( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data );

static int opcode_nop( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return 0;
}

static int opcode_ldreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LD RX RY */
  state->reg[reg] = mem_read( state, ntohl( state->reg[data & 0x7] ) );
  setflags( state, ntohl( state->reg[reg] ) );
  return 0;
}

static int opcode_ldconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LD RX const */
  state->reg[reg] = mem_read( state, data & 0xffff );
  setflags( state, ntohl( state->reg[reg] ) );
  return 0;
}

static int opcode_streg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ST RX RY */
  mem_write( state, ntohl( state->reg[reg] ), state->reg[data & 0x7] );
  return 0;
}

static int opcode_stconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ST RX const */
  mem_write( state, ntohl( state->reg[reg] ), htonl(sign_extend( data & 0xffff) ) );
  return 0;
}

static int opcode_ldi( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LDI RX const */
  state->reg[reg] = htonl(sign_extend( data ));
  setflags( state, ntohl( state->reg[reg] ) );
  return 0;
}

static int opcode_lea( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LEA RX const */
  state->reg[reg] = htonl(state->reg[FVM_REG_PC] + sign_extend( data ));
  return 0;
}

void fvm_push( struct fvm_s *state, uint32_t val ) {
  /* PUSH RX */
  if( state->reg[FVM_REG_SP] >= (FVM_ADDR_STACK + FVM_MAX_STACK) ) return;
  mem_write( state, state->reg[FVM_REG_SP], val );
  state->reg[FVM_REG_SP] += 4;  
}

uint32_t fvm_pop( struct fvm_s *state ) {
  if( state->reg[FVM_REG_SP] <= FVM_ADDR_STACK ) return -1;
  state->reg[FVM_REG_SP] -= 4;
  return mem_read( state, state->reg[FVM_REG_SP] );  
}

static int opcode_pushreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* PUSH RX */
  if( state->reg[FVM_REG_SP] >= (FVM_ADDR_STACK + FVM_MAX_STACK) ) return -1;
  
  mem_write( state, state->reg[FVM_REG_SP], state->reg[reg] );
  state->reg[FVM_REG_SP] += 4;
  return 0;
}

static int opcode_pushconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* PUSH const */
  if( state->reg[FVM_REG_SP] >= (FVM_ADDR_STACK + FVM_MAX_STACK) ) return -1;
  
  mem_write( state, state->reg[FVM_REG_SP], htonl(sign_extend( data )) );
  state->reg[FVM_REG_SP] += 4;
  return 0;
}

static int opcode_popreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* POP RX */
  if( state->reg[FVM_REG_SP] >= (FVM_ADDR_STACK + 4) ) {
    state->reg[FVM_REG_SP] -= 4;
    state->reg[reg] = mem_read( state, state->reg[FVM_REG_SP] );
  } else {
    state->reg[reg] = 0;
  }
  setflags( state, ntohl( state->reg[reg] ) );
  
  return 0;
}

static int opcode_ret( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* RET */
  if( state->reg[FVM_REG_SP] >= (FVM_ADDR_STACK + 4) ) {
    state->reg[FVM_REG_SP] -= 4;
    state->reg[FVM_REG_PC] = ntohl(mem_read( state, state->reg[FVM_REG_SP] ));
  }
  state->frame--;
  return 0;
}

static int opcode_callreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  mem_write( state, state->reg[FVM_REG_SP], htonl(state->reg[FVM_REG_PC]) );
  state->reg[FVM_REG_SP] += 4;
  state->reg[FVM_REG_PC] = ntohl(state->reg[reg]);
  state->frame++;
  return 0;
}

static int opcode_callconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  mem_write( state, state->reg[FVM_REG_SP], htonl(state->reg[FVM_REG_PC]) );
  state->reg[FVM_REG_SP] += 4;
  state->reg[FVM_REG_PC] = data & 0xffff;
  state->frame++;
  return 0;  
}


static int opcode_jmpreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[FVM_REG_PC] = ntohl(state->reg[reg]);
  return 0;
}
static int opcode_jmpconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[FVM_REG_PC] = data;
  return 0;
}


static int opcode_jzreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & FVM_FLAG_ZERO ) {
    state->reg[FVM_REG_PC] = ntohl(state->reg[reg]);
  }  
  return 0;
}
static int opcode_jzconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & FVM_FLAG_ZERO ) {
    state->reg[FVM_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jpreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & FVM_FLAG_POS ) {
    state->reg[FVM_REG_PC] = ntohl(state->reg[reg]);
  }  
  return 0;
}
static int opcode_jpconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & FVM_FLAG_POS ) {
    state->reg[FVM_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jnreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & FVM_FLAG_NEG ) {
    state->reg[FVM_REG_PC] = ntohl(state->reg[reg]);
  }  
  return 0;
}

static int opcode_jnconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & FVM_FLAG_NEG ) {
    state->reg[FVM_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jpzreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & (FVM_FLAG_POS|FVM_FLAG_ZERO) ) {
    state->reg[FVM_REG_PC] = ntohl(state->reg[reg]);
  }  
  return 0;
}
static int opcode_jpzconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & (FVM_FLAG_POS|FVM_FLAG_ZERO) ) {
    state->reg[FVM_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jpnreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & (FVM_FLAG_POS|FVM_FLAG_NEG) ) {
    state->reg[FVM_REG_PC] = ntohl(state->reg[reg]);
  }  
  return 0;
}
static int opcode_jpnconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & (FVM_FLAG_POS|FVM_FLAG_NEG) ) {
    state->reg[FVM_REG_PC] = data;
  }    
  return 0;
}

static int opcode_jnzreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & (FVM_FLAG_NEG|FVM_FLAG_ZERO) ) {
    state->reg[FVM_REG_PC] = ntohl(state->reg[reg]);
  }  
  return 0;
}
static int opcode_jnzconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  if( state->reg[FVM_REG_FLAGS] & (FVM_FLAG_NEG|FVM_FLAG_ZERO) ) {
    state->reg[FVM_REG_PC] = data;
  }    
  return 0;
}


static int opcode_addreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ADD RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) + ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_addconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ADD RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) + sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_subreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SUB RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) - ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_subconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SUB RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) - sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_mulreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MUL RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) * ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_mulconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MUL RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) * sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_divreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* DIV RX RY */
  if( ntohl(state->reg[data & 0x7]) == 0 ) return -1;
  state->reg[reg] = htonl(ntohl(state->reg[reg]) / ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_divconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* DIV RX const */
  if( data == 0 ) return -1;
  state->reg[reg] = htonl(ntohl(state->reg[reg]) / sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_modreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MOD RX RY */
  if( htonl(state->reg[data & 0x7]) == 0 ) return -1;
  state->reg[reg] = htonl(ntohl(state->reg[reg]) % ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_modconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* MOD RX const */
  if( data == 0 ) return -1;
  state->reg[reg] = htonl(ntohl(state->reg[reg]) % sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}

static int opcode_andreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* AND RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) & ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_andconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* AND RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) & sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_orreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* OR RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) | ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_orconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* OR RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) | sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_xorreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* XOR RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) ^ ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_xorconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* XOR RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) ^ sign_extend( data ));
  setflags( state, ntohl(state->reg[reg] ));
  return 0;
}
static int opcode_notreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[reg] = ~state->reg[reg];
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}

static int opcode_shlreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHL RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) << ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_shlconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHL RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) << sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_shrreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHR RX RY */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) >> ntohl(state->reg[data & 0x7]));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_shrconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SHR RX const */
  state->reg[reg] = htonl(ntohl(state->reg[reg]) >> sign_extend( data ));
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}

static int opcode_rolreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX RY */
  uint32_t r, bit;
  r = ntohl(state->reg[reg]);
  bit = (r & 0x8000000) ? 1 : 0;
  state->reg[reg] = htonl((r << ntohl(state->reg[data & 0x7])) | bit);
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_rolconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX const */
  uint32_t r, bit;
  r = ntohl(state->reg[reg]);
  bit = (r & 0x8000000) ? 1 : 0;
  state->reg[reg] = htonl((r << data) | bit);
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_rorreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX RY */
  uint32_t r, bit;
  r = ntohl(state->reg[reg]);
  bit = (r & 0x0000001) ? 0x8000000 : 0;
  state->reg[reg] = htonl((r >> ntohl(state->reg[data & 0x7])) | bit);
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}
static int opcode_rorconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ROL RX const */
  uint32_t r, bit;
  r = ntohl(state->reg[reg]);
  bit = (r & 0x0000001) ? 0x8000000 : 0;
  state->reg[reg] = htonl((r >> data) | bit);
  setflags( state, ntohl(state->reg[reg]) );
  return 0;
}

static int opcode_callvirt( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* CALLVIRT RX RY RZ */
  /* RX contains progid. RY contains proc index (index into symbol table).
   * RZ contains arg length, receives result length. arg/res on stack 
   */
  uint32_t addr; 
  int sts;
  struct fvm_module *m;
  uint32_t rx, ry, rz;
  struct fvm_s state2;
  char *args, *res, *resp;
  int argsize, ressize;
  uint32_t sp;
  
  rx = reg;
  ry = data & 0x7;
  rz = (data >> 4) & 0x7;

  argsize = ntohl( state->reg[rz] );
  if( (argsize > (state->reg[FVM_REG_SP] - FVM_ADDR_STACK)) ) {
    fvm_printf( "args too large %d > %u \n", argsize, state->reg[FVM_REG_SP] - FVM_ADDR_STACK );
    return -1;
  }
  
  sp = state->reg[FVM_REG_SP] - argsize;
  args = (char *)&state->stack[sp - FVM_ADDR_STACK];
  res = (char *)&state->stack[sp - FVM_ADDR_STACK];
  ressize = FVM_MAX_STACK - (sp - FVM_ADDR_STACK);

  m = fvm_module_by_progid( ntohl( state->reg[rx] ) );
  if( !m ) {
    fvm_printf( "callvirt unknown progid %u\n", ntohl( state->reg[rx] ) );
    state->reg[rz] = htonl( -1 );
    return 0;
  }

  addr = fvm_symbol_by_index( m, ntohl( state->reg[ry] ) );
  if( addr == 0 ) {
    fvm_printf( "callvirt unknown procid %u\n", ntohl( state->reg[ry] ) );
    state->reg[rz] = htonl( -1 );
    return 0;
  }

  memset( &state2, 0, sizeof(state2) );
  state2.nsteps = state->nsteps;
  state2.module = m;
  state2.datasize = m->header.datasize;
  state2.textsize = m->header.textsize;
  state2.data = m->data;
  state2.text = m->text;
  state2.reg[FVM_REG_PC] = addr;
  /* copy args onto stack */
  fvm_set_args( &state2, args, argsize );
  sts = fvm_run( &state2, default_maxsteps );
  state->nsteps = state2.nsteps;
  if( sts ) {
    fvm_printf( "callvirt run failed\n" );
    state->reg[rz] = htonl( -1 );
    return 0;
  }

  /* extract result data. Calling convention is R0 contains result length, R1 contains address of results, typically SP - R0  */
  ressize = fvm_get_res( &state2, &resp );
  if( ressize == -1 ) {
    fvm_printf( "callvirt return failure\n" );
    state->reg[rz] = htonl( -1 );
    return 0;
  }

  memcpy( res, resp, ressize );
  state->reg[rz] = htonl( ressize );
  fvm_printf( "callvirt success\n" );
  return 0;
}

static int opcode_ldvirt( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  uint32_t rx, ry, rz;
  struct fvm_s state2;
  int sts;
  
  rx = reg;
  ry = data & 0x7;
  rz = (data >> 4) & 0x7;

  sts = fvm_state_init( &state2, ntohl( state->reg[rx] ), ntohl( state->reg[ry] ) );
  if( sts ) {
    fvm_printf( "ldvirt failed to init\n" );
  }
  
  state->reg[rz] = fvm_read( &state2, state2.reg[FVM_REG_PC] );
  return 0;
}

static int opcode_stvirt( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  uint32_t rx, ry, rz;
  struct fvm_s state2;
  int sts;
  
  rx = reg;
  ry = data & 0x7;
  rz = (data >> 4) & 0x7;

  sts = fvm_state_init( &state2, ntohl( state->reg[rx] ), ntohl( state->reg[ry] ) );
  if( sts ) {
    fvm_printf( "ldvirt failed to init\n" );
  }

  fvm_write( &state2, state2.reg[FVM_REG_PC], state->reg[rz] );
  return 0;
}

static int opcode_leaspconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LEASP RX const */
  state->reg[reg] = htonl( state->reg[FVM_REG_SP] + sign_extend( data ) );
  return 0;
}

static int opcode_leaspreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LEASP RX RY */
  state->reg[reg] = htonl( state->reg[FVM_REG_SP] + ntohl( state->reg[data & 0x7] ) );
  return 0;
}

static int opcode_addspreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ADDSP RX */
  state->reg[FVM_REG_SP] += ntohl( state->reg[reg] );
  return 0;
}

static int opcode_addspconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* ADDSP const */
  state->reg[FVM_REG_SP] += sign_extend( data );
  return 0;
}

static int opcode_movreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  state->reg[reg] = state->reg[data & 0x7];
  return 0;
}

static int opcode_cmpreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  uint32_t x;
  x = ntohl( state->reg[reg] )  - ntohl( state->reg[data & 0x7] );
  setflags( state, x );
  return 0;
}

static int opcode_cmpconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  uint32_t x;
  x = ntohl( state->reg[reg] )  - sign_extend( data );
  setflags( state, x );
  return 0;
}

static int opcode_ldincreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LDINC RX RY */
  state->reg[reg] = mem_read( state, ntohl( state->reg[data & 0x7] ) );
  setflags( state, ntohl( state->reg[reg] ) );
  state->reg[data & 0x7] = htonl( ntohl( state->reg[data & 0x7] ) + 4 );
  return 0;
}

static int opcode_stincreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {

  /* STINC RX RY */
  mem_write( state, ntohl( state->reg[reg] ), state->reg[data & 0x7] );
  setflags( state, ntohl( state->reg[data & 0x7] ) );
  state->reg[reg] = htonl( ntohl( state->reg[reg] ) + 4 );
  
  return 0;
}

static int opcode_ldspreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LDSP RX RY */
  state->reg[reg] = mem_read( state, state->reg[FVM_REG_SP] + ntohl( state->reg[data & 0x7] ) );
  setflags( state, ntohl( state->reg[reg] ) );
  return 0;
}

static int opcode_ldspconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* LDSP RX RY */
  state->reg[reg] = mem_read( state, state->reg[FVM_REG_SP] + sign_extend( data ) );
  setflags( state, ntohl( state->reg[reg] ) );
  return 0;
}

static int opcode_subspreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SUBSP RX */
  state->reg[FVM_REG_SP] -= ntohl( state->reg[reg] );
  return 0;
}

static int opcode_subspconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  /* SUBSP const */
  state->reg[FVM_REG_SP] -= sign_extend( data );
  return 0;
}

static int opcode_callnatreg( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  int sts;
  
  /* CALLNAT RX RY */ 
  sts = fvm_native_call( state, ntohl( state->reg[data & 0x7] ) );
  fvm_printf( "callnative %u result %d\n", ntohl( state->reg[data & 0x7] ), sts );
  state->reg[reg] = htonl( sts );
  return 0;
}

static int opcode_callnatconst( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  int sts;
  
  /* CALLNAT RX const */ 
  sts = fvm_native_call( state, data );
  fvm_printf( "callnative %u result %d\n", data, sts );
  state->reg[reg] = htonl( sts );
  return 0;
}

static int opcode_halt( struct fvm_s *state, uint32_t flags, uint32_t reg, uint32_t data ) {
  return -1;
}

struct opcode_def {
  fvm_opcode_fn fn;
  char *name;
};
 
static struct opcode_def opcodes[FVM_MAX_OPCODE] =
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
   { opcode_leaspconst, "LEASP" },
   { opcode_addspreg, "ADDSP" },
   { opcode_addspconst, "ADDSP" },
   { opcode_movreg, "MOV" },
   { opcode_cmpreg, "CMP"},
   { opcode_cmpconst, "CMP" },
   { opcode_ldincreg, "LDINC" },
   { opcode_stincreg, "STINC" },
   { opcode_ldspreg, "LDSP" },
   { opcode_ldspconst, "LDSP" },
   { opcode_subspreg, "SUBSP" },
   { opcode_subspconst, "SUBSP" },
   { opcode_leaspreg, "LEASP" },
   { opcode_callnatreg, "CALLNAT" },
   { opcode_callnatconst, "CALLNAT" },
   { opcode_halt, "HALT" },
  };



int fvm_step( struct fvm_s *state ) {
  /* fetch next instruction */
  struct opcode_def *def;
  uint32_t opcode, pc;
  int sts;
  
  pc = state->reg[FVM_REG_PC];
  if( !(pc >= FVM_ADDR_TEXT && pc < (FVM_ADDR_TEXT + FVM_MAX_TEXT)) ) return -1;
  
  opcode = ntohl(mem_read( state, state->reg[FVM_REG_PC] ));
  def = &opcodes[(opcode >> 24) & 0xff];
  if( !def->fn ) return -1;

  fvm_printf( "%-16s Frame %-2u R0 %-4x R1 %-4x R2 %-4x R3 %-4x R4 %-4x R5 %-4x R6 %-4x R7 %-4x SP %-4x PC %-4x : %08x %-6s R%u 0x%04x (%d)\n",
	  state->module->header.name,
	  state->frame,
	  ntohl(state->reg[FVM_REG_R0]),
	  ntohl(state->reg[FVM_REG_R1]),
	  ntohl(state->reg[FVM_REG_R2]),
	  ntohl(state->reg[FVM_REG_R3]),
	  ntohl(state->reg[FVM_REG_R4]),
	  ntohl(state->reg[FVM_REG_R5]),
	  ntohl(state->reg[FVM_REG_R6]),
	  ntohl(state->reg[FVM_REG_R7]),
	  state->reg[FVM_REG_SP],
    	  state->reg[FVM_REG_PC],
	  opcode,
	  def->name,
	  (opcode & 0x00700000) >> 20,
	  opcode & 0xffff,
	  sign_extend( opcode & 0xffff ) );
  
  state->reg[FVM_REG_PC] += 4;  
  sts = def->fn( state, (opcode & 0x000f0000) >> 16, (opcode & 0x00700000) >> 20, opcode & 0x0000ffff );
  state->nsteps++;
  return sts;	      
}





uint32_t fvm_max_steps( uint32_t n ) {
  uint32_t old = default_maxsteps;
  if( n ) default_maxsteps = n;
  return old;
}
