
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

#include <fju/fvm.h>
#include <fju/mmf.h>
#include <fju/rpc.h>
#include <fju/log.h>

#include "fvmc.h"

struct fvm_state {
  struct fvm_module *module;
  uint32_t nsteps;
  uint32_t timeout;
  uint32_t pc;
  uint32_t sp;
  uint32_t frame;
  char stack[FVM_MAX_STACK];
};

static struct {
  struct fvm_module *modules;
  uint32_t max_steps;
  uint32_t max_runtime;
  uint32_t debug;
} glob = { NULL, 1000000, 5000 };

int fvm_module_load( char *buf, int size, struct fvm_module **modulep ) {
  /* parse header, load data and text segments */
  struct fvm_headerinfo *hdr;
  struct fvm_module *module;
  int i;
  
  if( size < sizeof(*hdr) ) return -1;
  
  hdr = (struct fvm_headerinfo *)buf;
  if( hdr->magic != FVM_MAGIC ) return -1;
  if( hdr->version != FVM_VERSION ) return -1;  
  if( size != sizeof(*hdr) + hdr->textsize ) return -1;
  if( hdr->nprocs > FVM_MAX_PROC ) return -1;
  for( i = 0; i < hdr->nprocs; i++ ) {
    if( (hdr->procs[i].address < FVM_ADDR_TEXT) ||
	(hdr->procs[i].address >= (FVM_ADDR_TEXT + hdr->textsize)) ) return -1;

    /* opaque params must be preceeded by a u32 param that receives the length */
    if( ((hdr->procs[i].siginfo >> (3*i)) & 0x3) == VAR_TYPE_OPAQUE ) {
      if( i == 0 ) return -1;
      if( ((hdr->procs[i].siginfo >> (3*(i - 1))) & 0x3) != VAR_TYPE_U32 ) return -1;
      if( ((hdr->procs[i].siginfo >> (3*i)) & 0x4) && !(hdr->procs[i].siginfo >> (3*(i - 1)) & 0x4) ) return -1;
      if( !((hdr->procs[i].siginfo >> (3*i)) & 0x4) && (hdr->procs[i].siginfo >> (3*(i - 1)) & 0x4) ) return -1;
    }
  }
  
  module = malloc( sizeof(*module) + hdr->datasize + hdr->textsize );
  memset( module, 0, sizeof(*module) );
  strcpy( module->name, hdr->name );
  module->progid = hdr->progid;
  module->versid = hdr->versid;
  module->nprocs = hdr->nprocs;
  for( i = 0; i < module->nprocs; i++ ) {
    strcpy( module->procs[i].name, hdr->procs[i].name );
    module->procs[i].address = hdr->procs[i].address;
    module->procs[i].siginfo = hdr->procs[i].siginfo;
  }
  module->textsize = hdr->textsize;
  module->datasize = hdr->datasize;
  module->data = (char *)module + sizeof(*module);
  module->text = (char *)module + sizeof(*module) + hdr->datasize;
  memset( module->data, 0, hdr->datasize );
  memcpy( module->text, (char *)hdr + sizeof(*hdr), hdr->textsize );

  module->next = glob.modules;
  glob.modules = module;

  if( modulep ) *modulep = module;
  
  return 0;
}

int fvm_module_load_file( char *filename, struct fvm_module **modulep ) {
  struct mmf_s mmf;
  int sts;
  sts = mmf_open2( filename, &mmf, MMF_OPEN_EXISTING );
  if( sts ) return sts;
  mmf_remap( &mmf, mmf.fsize );
  sts = fvm_module_load( mmf.file, mmf.fsize, modulep );
  mmf_close( &mmf );
  return sts;
}

int fvm_module_unload( char *modname ) {
  struct fvm_module *m, *prev;
  m = glob.modules;
  prev = NULL;
  while( m ) {
    if( strcasecmp( m->name, modname ) == 0 ) {
      if( prev ) prev->next = m->next;
      else glob.modules = m->next;
      free( m );
      return 0;
    }
    m = m->next;
  }
  return -1;
}

struct fvm_module *fvm_module_by_name( char *name ) {
  struct fvm_module *m;
  m = glob.modules;
  while( m ) {
    if( strcasecmp( m->name, name ) == 0 ) return m;
    m = m->next;
  }
  return NULL;
}

struct fvm_module *fvm_module_by_progid( uint32_t progid, uint32_t versid ) {
  struct fvm_module *m;
  m = glob.modules;
  while( m ) {
    if( m->progid == progid && m->versid == versid ) return m;
    m = m->next;
  }
  return NULL;
}
    
int fvm_procid_by_name( struct fvm_module *module, char *procname ) {
  int i;
  for( i = 0; i < module->nprocs; i++ ) {
    if( strcasecmp( module->procs[i].name, procname ) == 0 ) return i;
  }
  return -1;
}

/* --------------- runtime ------------------- */

static int fvm_push( struct fvm_state *state, uint32_t u32 ) {
  if( state->sp >= FVM_MAX_STACK ) return -1;
  memcpy( &state->stack[state->sp], &u32, 4 );
  state->sp += 4;
  return 0;
}

static uint32_t fvm_pop( struct fvm_state *state ) {
  uint32_t u32;
  if( state->sp < 4 ) return -1;
  state->sp -= 4;  
  memcpy( &u32, &state->stack[state->sp], 4 );
  return u32;
}

static char *fvm_getptr( struct fvm_state *state, uint32_t addr ) {
  if( (addr >= FVM_ADDR_DATA) && (addr < (FVM_ADDR_DATA + state->module->datasize)) ) {
    return &state->module->data[addr - FVM_ADDR_DATA];
  }
  if( (addr >= FVM_ADDR_TEXT) && (addr < (FVM_ADDR_TEXT + state->module->textsize)) ) {
    return &state->module->text[addr - FVM_ADDR_TEXT];
  }
  if( (addr >= FVM_ADDR_STACK) && (addr < (FVM_ADDR_STACK + FVM_MAX_STACK)) ) {
    return &state->stack[addr - FVM_ADDR_STACK];
  }
  return NULL;  
}

static uint32_t fvm_read_u32( struct fvm_state *state, uint32_t addr ) {
  uint32_t u;
  if( (addr >= FVM_ADDR_DATA) && (addr < (FVM_ADDR_DATA + state->module->datasize)) ) {
    memcpy( &u, &state->module->data[addr - FVM_ADDR_DATA], 4 );
    return u;
  }
  if( (addr >= FVM_ADDR_TEXT) && (addr < (FVM_ADDR_TEXT + state->module->textsize)) ) {
    memcpy( &u, &state->module->text[addr - FVM_ADDR_TEXT], 4 );
    return u;
  }
  if( (addr >= FVM_ADDR_STACK) && (addr < (FVM_ADDR_STACK + FVM_MAX_STACK)) ) {
    memcpy( &u, &state->stack[addr - FVM_ADDR_STACK], 4 );
    return u;
  }
  
  return 0;  
}
static uint16_t fvm_read_pcu16( struct fvm_state *state ) {
  uint16_t u;
  uint32_t addr = state->pc;
  u = 0;
  if( (addr >= FVM_ADDR_TEXT) && (addr < (FVM_ADDR_TEXT + state->module->textsize)) ) {
    memcpy( &u, &state->module->text[addr - FVM_ADDR_TEXT], 2 );
  }
  state->pc += 2;  
  return u;
}

static int fvm_write_u32( struct fvm_state *state, uint32_t addr, uint32_t u ) {

  if( (addr >= FVM_ADDR_DATA) && (addr < (FVM_ADDR_DATA + state->module->datasize - 4)) ) {
    memcpy( &state->module->data[addr - FVM_ADDR_DATA], &u, 4 );
    return 0;
  }
  if( (addr >= FVM_ADDR_STACK) && (addr < (FVM_ADDR_STACK + FVM_MAX_STACK - 4)) ) {
    memcpy( &state->stack[addr - FVM_ADDR_STACK], &u, 4 );
    return 0;
  }
  
  return -1;  
}

static uint32_t fvm_stack_read( struct fvm_state *state, uint32_t depth ) {
  return fvm_read_u32( state, FVM_ADDR_STACK + state->sp - depth );
}


struct opinfo {  
  op_t op;
  char *name;
  uint32_t pcdata;
  int32_t stackadjust;
};

static struct opinfo opcodeinfo[] =
  {
   { OP_NOP, "NOP", 0, 0 },
   { OP_LDI32, "LDI32", 4, 4 },
   { OP_LEA, "LEA", 2, 4 },
   { OP_ADDSP, "ADDSP", 2, 0 }, /* opcode adjusts sp directly */
   { OP_SUBSP, "SUBSP", 2, 0 }, /* ditto */
   { OP_CALL, "CALL", 2, 0 }, /* ( -- retaddr ) */
   { OP_RET, "RET", 0, 0 }, /* ( retaddr -- ) */
   { OP_LEASP, "LEASP", 2, 4 }, /* ( -- address ) */
   { OP_LDSP, "LDSP", 2, 4 }, /* ( -- value ) */
   { OP_STSP, "STSP", 2, -4 }, /* (value -- )*/
   { OP_BR, "BR", 2, -4 },  /* (test --) */
   { OP_EQ, "EQ", 0, -4 }, /* (a b -- test) */
   { OP_NEQ, "NEQ", 0, -4 },
   { OP_GT, "GT", 0, -4 },
   { OP_GTE, "GTE", 0, -4 },
   { OP_LT, "LT", 0, -4 },
   { OP_LTE, "LTE", 0, -4 },
   { OP_JMP, "JMP", 2, 0 },
   { OP_ADD, "ADD", 0, -4 },
   { OP_SUB, "SUB", 0, -4 },
   { OP_MUL, "MUL", 0, -4 },
   { OP_DIV, "DIV", 0, -4 },      
   { OP_MOD, "MOD", 0, -4 },      
   { OP_AND, "AND", 0, -4 },
   { OP_OR, "OR", 0, -4 },
   { OP_XOR, "XOR", 0, -4 }, /* (a b -- a^b )*/
   { OP_NOT, "NOT", 0, 0 },
   { OP_SHL, "SHL", 0, -4 }, /* (value shift -- value) */
   { OP_SHR, "SHR", 0, -4 }, /* (value shift -- value) */
   { OP_LD, "LD", 0, 0 },  /* ( address -- value ) */
   { OP_ST, "ST", 0, -8 },  /* (address value -- ) */
   { OP_SYSCALL, "SYSCALL", 2, 0 },
   { 0, NULL, 0, 0 }
  };
static struct opinfo *getopinfo( op_t op ) {
  int i;
  for( i = 0; opcodeinfo[i].name; i++ ) {
    if( opcodeinfo[i].op == op ) return &opcodeinfo[i];
  }
  return NULL;
}

static int fvm_step( struct fvm_state *state ) {
  op_t op;  
  uint8_t u8;
  uint16_t u16;
  int16_t i16;
  uint32_t u32, addr;  
  struct opinfo *oinfo;
  int i;
  
  if( (state->pc < FVM_ADDR_TEXT) || (state->pc >= (FVM_ADDR_TEXT + state->module->textsize)) ) {
    printf( "bad pc %04x\n", state->pc );
    return -1;
  }

  u8 = state->module->text[state->pc - FVM_ADDR_TEXT];
  op = u8;

  if( glob.debug ) {
    oinfo = getopinfo( op );
    printf( "PC=%04x SP=%04x %s Stack: ", state->pc, state->sp, oinfo ? oinfo->name : "unknown" );
    u32 = (state->sp > 64) ? state->sp - 64 : 0;
    printf( "%04x: ", u32 );
    for( i = u32; i < state->sp; i += 4 ) {
      printf( "%x ", *((uint32_t *)&state->stack[i]) );
    }
    printf( "\n" );
  }
  
  state->pc++;
  switch( op ) {
  case OP_NOP:
    break;
  case OP_LDI32:
    u32 = fvm_read_u32( state, state->pc );
    state->pc += 4;
    fvm_push( state, u32 );
    break;
  case OP_LEA:
    i16 = (int16_t)fvm_read_pcu16( state );
    fvm_push( state, ((int)state->pc) + i16 );
    break;
  case OP_ADDSP:
    u16 = fvm_read_pcu16( state );
    state->sp += u16;
    break;
  case OP_SUBSP:
    u16 = fvm_read_pcu16( state );
    state->sp -= u16;
    break;
  case OP_CALL:
    u16 = fvm_read_pcu16( state );
    fvm_push( state, state->pc );
    state->pc = u16;
    state->frame++;
    break;
  case OP_RET:
    u32 = fvm_pop( state );
    if( u32 < FVM_ADDR_TEXT || (u32 >= (FVM_ADDR_TEXT + state->module->textsize)) ) {
      if( (u32 == 0) && (state->frame == 1) ) {
	//printf( "Returning from entry point routine\n" );
      } else {
	printf( "Attempt to return to invalid address %04x\n", u32 );
	return -1;
      }
    }
    state->pc = u32;
    state->frame--;
    break;
  case OP_LEASP:
    u16 = fvm_read_pcu16( state );
    fvm_push( state, FVM_ADDR_STACK + state->sp - u16 );    
    break;
  case OP_LDSP:
    u16 = fvm_read_pcu16( state );
    u32 = fvm_read_u32( state, FVM_ADDR_STACK + state->sp - u16 );
    fvm_push( state, u32 );
    break;
  case OP_STSP:
    u16 = fvm_read_pcu16( state );
    addr = FVM_ADDR_STACK + state->sp - u16;
    u32 = fvm_pop( state );
    fvm_write_u32( state, addr, u32 );
    break;
  case OP_BR:
    u16 = fvm_read_pcu16( state );
    u32 = fvm_pop( state );
    if( u32 ) {
      state->pc = u16;
    }
    break;
  case OP_EQ:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, u32 == addr ? 1 : 0 );
    break;
  case OP_GT:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr > u32 ? 1 : 0 );
    break;
  case OP_GTE:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr >= u32 ? 1 : 0 );
    break;    
  case OP_LT:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr < u32 ? 1 : 0 );
    break;    
  case OP_LTE:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr <= u32 ? 1 : 0 );
    break;
  case OP_JMP:
    u16 = fvm_read_pcu16( state );
    state->pc = u16;
    break;
  case OP_ADD:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, u32 + addr );
    break;
  case OP_SUB:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr - u32 );
    break;
  case OP_MUL:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, u32 * addr );
    break;
  case OP_DIV:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, u32 ? addr / u32 : 0 );
    break;
  case OP_MOD:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, u32 ? addr % u32 : 0 );
    break;
  case OP_AND:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr & u32 );
    break;
  case OP_OR:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr | u32 );
    break;
  case OP_XOR:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr ^ u32 );
    break;
  case OP_NOT:
    u32 = fvm_pop( state );
    fvm_push( state, ~u32 );
    break;
  case OP_SHL:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr << u32 );
    break;
  case OP_SHR:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, addr >> u32 );
    break;
  case OP_LD:
    u32 = fvm_pop( state );
    u32 = fvm_read_u32( state, u32 );
    fvm_push( state, u32 );
    break;
  case OP_ST:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_write_u32( state, addr, u32 );
    break;
  case OP_SYSCALL:
    u16 = fvm_read_pcu16( state );
    switch( u16 ) {
    case 1:
      /* LogWrite(flags,len,buf) */
      {
	char *buf;
	uint32_t len, flags;
	struct log_entry entry;
	struct log_iov iov[1];
	
	addr = fvm_stack_read( state, 4 ); /* bufadd */
	buf = fvm_getptr( state, addr );
	len = fvm_stack_read( state, 8 ); /* buflen */
	flags = fvm_stack_read( state, 12 ); 
	memset( &entry, 0, sizeof(entry) );
	iov[0].buf = buf;
	iov[0].len = buf ? len : 0;
	entry.iov = iov;
	entry.niov = 1;
	entry.flags = flags;
	log_write( NULL, &entry );
      }
      break;
    default:
      printf( "INvalid syscall %u\n", (uint32_t)u16 );
      return -1;
    }
    break;
  default:
    printf( "INvalid opcode %u\n", op );
    return -1;
  }

  return 0;
}


int fvm_run( struct fvm_module *module, uint32_t procid, struct xdr_s *argbuf , struct xdr_s *resbuf ) {
  struct fvm_state state;
  uint64_t start;
  int sts;
  uint32_t isvar[FVM_MAX_PARAM], vartype[FVM_MAX_PARAM], u32[FVM_MAX_PARAM];
  uint32_t siginfo, u;
  int i, nargs, len;
  char *str, *buf;
  
  if( procid > module->nprocs ) return -1;
  
  memset( &state, 0, sizeof(state) );
  state.module = module;
  state.sp = 0;
  state.pc = module->procs[procid].address;

  /* 
   * prepare args on stack:
   * <string/opaque buffers><u32 args and pointers to the string/opaque args><dummy return address> 
   */

  siginfo = module->procs[procid].siginfo;
  nargs = (siginfo >> 24) & 0x1f;
  
  for( i = 0; i < nargs; i++ ) {
    isvar[i] = (siginfo >> (i*3)) & 0x4 ? 1 : 0;
    vartype[i] = (siginfo >> (i*3)) & 0x3;

    if( isvar[i] ) {
      /* output arg: reserve space for result pointer */
      u32[i] = FVM_ADDR_STACK + state.sp; /* address of result value */
      state.sp += 4;
    } else {
      /* input arg */
      switch( vartype[i] ) {
      case VAR_TYPE_U32:
	if( (i < (nargs - 1)) && (vartype[i + 1] == VAR_TYPE_OPAQUE) ) {
	  /* don't decode the u32 if the next param is opaque. that's because this will receive the length */
	} else {
	  sts = xdr_decode_uint32( argbuf, &u32[i] );
	  if( sts ) return sts;
	}
	break;
      case VAR_TYPE_STRING:
	sts = xdr_decode_string( argbuf, state.stack + state.sp, FVM_MAX_STACK - state.sp );
	if( sts ) return sts;
	u32[i] = FVM_ADDR_STACK + state.sp;
	len = strlen( state.stack + state.sp ) + 1;
	if( len % 4 ) len += 4 - (len % 4);
	state.sp += len;
	break;
      case VAR_TYPE_OPAQUE:
	len = FVM_MAX_STACK - state.sp;
	sts = xdr_decode_opaque( argbuf, (uint8_t *)state.stack + state.sp, &len );
	if( sts ) return sts;
	u32[i - 1] = len;
	u32[i] = FVM_ADDR_STACK + state.sp;
	if( len % 4 ) len += 4 - (len % 4);
	state.sp += len;
	break;
      }      
    }
  }

  /* push args values */
  for( i = 0; i < nargs; i++ ) {
    fvm_push( &state, u32[i] ); 
  }
  fvm_push( &state, 0 ); /* push dummy return address */
    
  start = rpc_now();
  state.frame = 1;
  while( state.frame && (state.nsteps < glob.max_steps) ) {
    sts = fvm_step( &state );
    if( sts ) { printf( "step failed\n"); return -1; }

    if( (state.nsteps % 1000) == 0 ) {
      if( (rpc_now() - start) > glob.max_runtime ) {
	//return -1;
      }
    }
  }
  
  /* decode results */
  if( !resbuf ) return 0;

  for( i = 0; i < nargs; i++ ) {
    if( isvar[i] ) {
      switch( vartype[i] ) {
      case VAR_TYPE_U32:
	if( (i < (nargs - 1)) && (vartype[i + 1] == VAR_TYPE_OPAQUE) ) {
	  u = u32[i];
	  u32[i] = fvm_read_u32( &state, u );
	} else {
	  /* get result */
	  u = fvm_read_u32( &state, u32[i] );
	  printf( "Extracted %u\n", u );
	  sts = xdr_encode_uint32( resbuf, u );
	  if( sts ) return sts;
	}
	break;
      case VAR_TYPE_STRING:
	u = fvm_read_u32( &state, u32[i] );
	str = fvm_getptr( &state, u );
	sts = xdr_encode_string( resbuf, str ? str : "" );
	if( sts ) return sts;
	break;
      case VAR_TYPE_OPAQUE:
	u = fvm_read_u32( &state, u32[i] );
	buf = fvm_getptr( &state, u );
	len = u32[i - 1];
	sts = xdr_encode_opaque( resbuf, (uint8_t *)(buf ? buf : NULL), buf ? len : 0 );
	if( sts ) return sts;
	break;
      }
    }
  }

  resbuf->count = resbuf->offset;
  resbuf->offset = 0;
  
  return 0;
  
}

/* ------------------- rpc interface ---------------- */

void fvm_rpc_register( void ) {
}
