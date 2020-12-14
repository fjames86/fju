
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#define strcasecmp _stricmp
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

#include <fju/fvm.h>
#include <fju/mmf.h>
#include <fju/rpc.h>
#include <fju/log.h>
#include <fju/raft.h>
#include <fju/hostreg.h>
#include <fju/rpcd.h>
#include <fju/programs.h>
#include <fju/freg.h>
#include <fju/sec.h>

#include "fvm-private.h"

log_deflogger(fvm_log,FVM_RPC_PROG)

static int fvm_unregister_program( char *modname );

static struct {
  struct fvm_module *modules;
  uint32_t max_steps;
  uint32_t max_runtime;
  uint32_t debug;
} glob = { NULL, 1000000, 5000 };

static int fvmc_decode_header( struct xdr_s *xdr, struct fvm_headerinfo *x ) {
  int i, sts;
  sts = xdr_decode_uint32( xdr, &x->magic );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &x->version );
  if( sts ) return sts;  
  sts = xdr_decode_string( xdr, x->name, sizeof(x->name) );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->progid );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->versid );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->datasize );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->textsize );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->nprocs );
  if( sts ) return sts;
  if( x->nprocs > FVM_MAX_PROC ) return -1;
  for( i = 0; i < x->nprocs; i++ ) {
    sts = xdr_decode_string( xdr, x->procs[i].name, sizeof(x->procs[i].name) );
    if( sts ) return sts;    
    sts = xdr_decode_uint32( xdr, &x->procs[i].address );
    if( sts ) return sts;    
    sts = xdr_decode_uint64( xdr, &x->procs[i].siginfo );
    if( sts ) return sts;    
  }
  return 0;
}

static int get_init_proc( struct fvm_module *m, char *procname ) {
  int sts;
  char path[256];
  sprintf( path, "/fju/fvm/modules/%s/init", m->name );
  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_STRING, procname, FVM_MAX_NAME, NULL );
  if( !sts ) return 0;
  if( fvm_procid_by_name( m, "init" ) >= 0 ) {
    strcpy( procname, "init" );
    return 0;
  }
  return -1;
}

int fvm_module_load( char *buf, int size, struct fvm_module **modulep ) {
  /* parse header, load data and text segments */
  struct fvm_headerinfo hdr;
  struct fvm_module *module;
  int i, sts;
  struct xdr_s xdr;
  char procname[FVM_MAX_NAME];
  
  xdr_init( &xdr, (uint8_t *)buf, size );
  sts = fvmc_decode_header( &xdr, &hdr );
	      
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "Failed to decode header" );
    return -1;
  }
  
  if( hdr.magic != FVM_MAGIC ) {
    fvm_log( LOG_LVL_ERROR, "Bad magic" );
    return -1;
  }
  
  if( hdr.version != FVM_VERSION ) {
    fvm_log( LOG_LVL_ERROR, "Bad version" );
    return -1;
  }
  
  if( xdr.count != (xdr.offset + hdr.textsize) ) {
    fvm_log( LOG_LVL_ERROR, "Bad size buffer size" );
    return -1;
  }
  
  for( i = 0; i < hdr.nprocs; i++ ) {
    if( (hdr.procs[i].address < FVM_ADDR_TEXT) ||
	(hdr.procs[i].address >= (FVM_ADDR_TEXT + hdr.textsize)) ) {
      fvm_log( LOG_LVL_ERROR, "Proc address outsize text" );
      return -1;
    }

    /* opaque params must be preceeded by a u32 param that receives the length */
    if( FVM_SIGINFO_VARTYPE(hdr.procs[i].siginfo, i) == VAR_TYPE_OPAQUE ) {
      if( i == 0 ) {
	fvm_log( LOG_LVL_ERROR, "Bad parameter" );
	return -1;
      }
      
      if( FVM_SIGINFO_VARTYPE(hdr.procs[i].siginfo, i - 1) != VAR_TYPE_U32 ) {
	fvm_log( LOG_LVL_ERROR, "Bad parameter" );
	return -1;
      }
      
      if( FVM_SIGINFO_ISVAR(hdr.procs[i].siginfo, i) && !FVM_SIGINFO_ISVAR(hdr.procs[i].siginfo, i - 1) ) {
	fvm_log( LOG_LVL_ERROR, "Bad parameter" );	
	return -1;
      }
      
      if( !FVM_SIGINFO_ISVAR(hdr.procs[i].siginfo, i) && FVM_SIGINFO_ISVAR(hdr.procs[i].siginfo, i - 1) ) {
	fvm_log( LOG_LVL_ERROR, "Bad parameter" );	
	return -1;
      }
    }
  }
  if( fvm_module_by_name( hdr.name ) ) {
    fvm_log( LOG_LVL_ERROR, "Module already registered" );
    return -1;
  }
  
  module = malloc( sizeof(*module) + hdr.datasize + hdr.textsize );
  memset( module, 0, sizeof(*module) );
  strcpy( module->name, hdr.name );
  module->progid = hdr.progid;
  module->versid = hdr.versid;
  module->nprocs = hdr.nprocs;
  for( i = 0; i < module->nprocs; i++ ) {
    strcpy( module->procs[i].name, hdr.procs[i].name );
    module->procs[i].address = hdr.procs[i].address;
    module->procs[i].siginfo = hdr.procs[i].siginfo;
  }
  module->textsize = hdr.textsize;
  module->datasize = hdr.datasize;
  module->data = (char *)module + sizeof(*module);
  module->text = (char *)module + sizeof(*module) + hdr.datasize;
  memset( module->data, 0, hdr.datasize );
  memcpy( module->text, xdr.buf + xdr.offset, hdr.textsize );

  module->next = glob.modules;
  glob.modules = module;

  if( modulep ) *modulep = module;

  sts = get_init_proc( module, procname );
  if( !sts ) {
    fvm_run( module, fvm_procid_by_name( module, procname ), NULL, NULL );
  }
  
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
      /* unload any rpc program, if any */
      fvm_unregister_program( modname );
      
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

char *fvm_getptr( struct fvm_state *state, uint32_t addr, int len, int writeable ) {
  if( (addr >= FVM_ADDR_DATA) && (addr < (FVM_ADDR_DATA + state->module->datasize - len)) ) {
    return &state->module->data[addr - FVM_ADDR_DATA];
  }

  if( !writeable ) {
    if( (addr >= FVM_ADDR_TEXT) && (addr < (FVM_ADDR_TEXT + state->module->textsize - len)) ) {
      return &state->module->text[addr - FVM_ADDR_TEXT];
    }
  }
  
  if( (addr >= FVM_ADDR_STACK) && (addr < (FVM_ADDR_STACK + FVM_MAX_STACK - len)) ) {
    return &state->stack[addr - FVM_ADDR_STACK];
  }
  return NULL;  
}

uint32_t fvm_read_u32( struct fvm_state *state, uint32_t addr ) {
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

int fvm_write_u32( struct fvm_state *state, uint32_t addr, uint32_t u ) {

  if( (addr >= FVM_ADDR_DATA) && (addr < (FVM_ADDR_DATA + state->module->datasize - 4)) ) {
    memcpy( &state->module->data[addr - FVM_ADDR_DATA], &u, 4 );
    return 0;
  }
  if( (addr >= FVM_ADDR_STACK) && (addr < (FVM_ADDR_STACK + FVM_MAX_STACK - 4)) ) {
    memcpy( &state->stack[addr - FVM_ADDR_STACK], &u, 4 );
    return 0;
  }

  printf( " WARNING attempt to set invalid address %x\n", addr );
  return -1;  
}

uint32_t fvm_stack_read( struct fvm_state *state, uint32_t depth ) {
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
   { OP_BRZ, "BRZ", 2, 0 }, /* branch if zero */
   { OP_LD8, "LD8", 0, 0 },
   { OP_ST8, "ST8", 0, -8 },
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
  int i, sts;
  
  if( (state->pc < FVM_ADDR_TEXT) || (state->pc >= (FVM_ADDR_TEXT + state->module->textsize)) ) {
    printf( "bad pc %04x\n", state->pc );
    return -1;
  }

  u8 = state->module->text[state->pc - FVM_ADDR_TEXT];
  op = u8;

  if( glob.debug ) {
    oinfo = getopinfo( op );
    if( oinfo->pcdata == 0 ) {
      printf( "PC=%04x SP=%04x %s Stack: ", state->pc, state->sp, oinfo ? oinfo->name : "unknown" );
    } else if( oinfo->pcdata == 2 ) {
      memcpy( &u16, &state->module->text[state->pc - FVM_ADDR_TEXT + 1], 2 );
      printf( "PC=%04x SP=%04x %s %d|%x Stack: ", state->pc, state->sp, oinfo ? oinfo->name : "unknown", (int32_t)(int16_t)u16, (uint32_t)u16 );
    } else if( oinfo->pcdata == 4 ) {
      memcpy( &u32, &state->module->text[state->pc - FVM_ADDR_TEXT + 1], 4 );      
      printf( "PC=%04x SP=%04x %s %d|%x Stack: ", state->pc, state->sp, oinfo ? oinfo->name : "unknown", u32, u32 );
    }
    
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
  case OP_BRZ:
    u16 = fvm_read_pcu16( state );
    u32 = fvm_pop( state );
    if( !u32 ) {
      state->pc = u16;
    }
    break;
  case OP_EQ:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, u32 == addr ? 1 : 0 );
    break;
  case OP_NEQ:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    fvm_push( state, u32 != addr ? 1 : 0 );
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
  case OP_LD8:
    {
      uint8_t *ptr;
      addr = fvm_pop( state );
      ptr = (uint8_t *)fvm_getptr( state, addr, 1, 0 );
      u32 = ptr ? *ptr : 0;
      fvm_push( state, u32 );
    }
    break;
  case OP_ST8:
    {
      uint8_t *ptr;      
      u32 = fvm_pop( state ) & 0xff;
      addr = fvm_pop( state );
      ptr = (uint8_t *)fvm_getptr( state, addr, 1, 1 );
      if( ptr ) *ptr = u32;
      else printf( ";; WARNING ST8 attempt to set invalid address %x\n", addr );
    }
    break;    
  case OP_SYSCALL:
    u16 = fvm_read_pcu16( state );
    sts = fvm_syscall( state, u16 );
    if( sts ) return -1;
    break;
  default:
    printf( "Invalid opcode %u\n", op );
    return -1;
  }

  return 0;
}


int fvm_run( struct fvm_module *module, uint32_t procid, struct xdr_s *argbuf , struct xdr_s *resbuf ) {
  struct fvm_state state;
  uint64_t start, now;
  int sts;
  uint32_t isvar[FVM_MAX_PARAM], vartype[FVM_MAX_PARAM], u32[FVM_MAX_PARAM];
  uint64_t siginfo;
  uint32_t u;
  int i, nargs, len;
  char *str, *buf;
  
  if( (procid < 0) || (procid >= module->nprocs) ) return -1;
  
  memset( &state, 0, sizeof(state) );
  state.module = module;
  state.sp = 0;
  state.pc = module->procs[procid].address;

  /* 
   * prepare args on stack:
   * <string/opaque buffers><u32 args and pointers to the string/opaque args><dummy return address> 
   */

  siginfo = module->procs[procid].siginfo;
  nargs = FVM_SIGINFO_NARGS(siginfo);

  for( i = 0; i < nargs; i++ ) {
    isvar[i] = FVM_SIGINFO_ISVAR(siginfo,i);
    vartype[i] = FVM_SIGINFO_VARTYPE(siginfo,i);

    if( isvar[i] ) {
      /* output arg: reserve space for result pointer */
      u32[i] = FVM_ADDR_STACK + state.sp; /* address of result value */
      state.sp += 4;
    } else {
      if( !argbuf ) return -1;
      
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
  while( state.frame ) {
    sts = fvm_step( &state );
    if( sts ) { printf( "step failed\n"); return -1; }

    if( (state.nsteps % 1000) == 0 ) {
      if( state.nsteps > glob.max_steps ) {
	fvm_log( LOG_LVL_WARN, "fvm_run exited due to max steps %u", state.nsteps, glob.max_steps );
	return -1;
      }
      
      now = rpc_now();
      if( (now - start) > glob.max_runtime ) {
	fvm_log( LOG_LVL_WARN, "fvm_run exited due to timeout %u", (now - start), glob.max_runtime );
	return -1;
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
	str = fvm_getptr( &state, u, 0, 0 );
	sts = xdr_encode_string( resbuf, str ? str : "" );
	if( sts ) return sts;
	break;
      case VAR_TYPE_OPAQUE:
	u = fvm_read_u32( &state, u32[i] );
	buf = fvm_getptr( &state, u, 0, 0 );
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

/* -------------------- clustering ---------------- */

struct fvm_command_s {
  char modname[FVM_MAX_NAME];
  uint32_t mode;
#define FVM_MODE_UPDATESTATE 0   /* args are the updated state to apply */
#define FVM_MODE_RUN         1   /* run a given proc with specified args */
  union {
    struct {
      char *buf;
      int len;
    } updatestate;
    struct {
      uint64_t hostid;           /* if hostid=0 the given proc is run on all nodes, otherwise it is run only on the given node */
      char procname[FVM_MAX_NAME];
      char *args;                /* proc args */
      int len;
    } run;
  } u;
};

static int fvm_decode_command( struct xdr_s *xdr, struct fvm_command_s *x ) {
  int sts;
  sts = xdr_decode_string( xdr, x->modname, sizeof(x->modname) );
  if( !sts ) sts = xdr_decode_uint32( xdr, &x->mode );
  if( sts ) return sts;
  
  switch( x->mode ) {
  case FVM_MODE_UPDATESTATE:
    sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&x->u.updatestate.buf, &x->u.updatestate.len );
    break;
  case FVM_MODE_RUN:
    sts = xdr_decode_uint64( xdr, &x->u.run.hostid );
    if( !sts ) sts = xdr_decode_string( xdr, x->u.run.procname, sizeof(x->u.run.procname) );
    if( !sts ) sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&x->u.run.args, &x->u.run.len );
    break;
  }
  
  return sts;
}



static void fvm_command( struct raft_app *app, struct raft_cluster *cl, uint64_t seq, char *buf, int len ) {
  int sts;
  struct fvm_command_s cmd;
  struct xdr_s xdr;
  struct fvm_module *m;
    
  xdr_init( &xdr, (uint8_t *)buf, len );
  sts = fvm_decode_command( &xdr, &cmd );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "XDR error decoding command" );
    return;
  }

  m = fvm_module_by_name( cmd.modname );
  if( !m ) {
    fvm_log( LOG_LVL_ERROR, "Unknown module %s", cmd.modname );
    return;
  }
  
  switch( cmd.mode ) {
  case FVM_MODE_UPDATESTATE:
    if( cmd.u.updatestate.len != m->datasize ) {
      fvm_log( LOG_LVL_ERROR, "Bad datasize" );
      return;
    }
    fvm_log( LOG_LVL_TRACE, "fvm update data segment modname=%s", cmd.modname );
    memcpy( m->data, cmd.u.updatestate.buf, m->datasize );
    break;
  case FVM_MODE_RUN:
    {
      uint32_t procid;
      
      fvm_log( LOG_LVL_TRACE, "fvm run mod=%s proc=%s arglen=%u", cmd.modname, cmd.u.run.procname, cmd.u.run.len );
      
      if( (cmd.u.run.hostid == 0) || (cmd.u.run.hostid == hostreg_localid()) ) {
	struct xdr_s args;
	
	procid = fvm_procid_by_name( m, cmd.u.run.procname );
	if( procid < 0 ) {
	  fvm_log( LOG_LVL_ERROR, "Unknown proc %s", cmd.u.run.procname );
	  return;
	}

	xdr_init( &args, (uint8_t *)cmd.u.run.args, cmd.u.run.len );
	fvm_run( m, procid, &args, NULL );
      }
    }
    break;
  }
}


static struct raft_app fvm_app =
  {
   NULL,
   FVM_RPC_PROG,
   fvm_command,
   //   fvm_snapsave,  /* no need for snapshotting so we can ignore these */
   //   fvm_snapload,
  };


int fvm_cluster_run( uint64_t clid, char *modname, char *procname, char *args, int len ) {
  struct xdr_s buf;
  struct rpc_conn *c;
  int sts;

  /* If cluster not specified lookup first cluster with appid=FVM_RPC_PROG */
  if( clid == 0 ) {
    clid = raft_clid_by_appid( FVM_RPC_PROG );
    if( clid == 0 ) return -1;
  }

  c = rpc_conn_acquire();
  if( !c ) return -1;

  xdr_init( &buf, (uint8_t *)c->buf, c->count );
  xdr_encode_string( &buf, modname );
  xdr_encode_uint32( &buf, FVM_MODE_RUN );
  xdr_encode_uint64( &buf, 0 ); /* hostid */
  xdr_encode_string( &buf, procname );
  xdr_encode_opaque( &buf, (uint8_t *)args, len );
  sts = raft_cluster_command( clid, (char *)buf.buf, buf.offset, NULL );
  
  rpc_conn_release( c );
  
  return sts;
}

int fvm_cluster_updatestate( uint64_t clid, char *modname ) {
  struct xdr_s buf;
  struct rpc_conn *c;
  int sts;
  struct fvm_module *m;
  
  m = fvm_module_by_name( modname );
  if( !m ) return -1;
  
  if( clid == 0 ) {
    clid = raft_clid_by_appid( FVM_RPC_PROG );
    if( clid == 0 ) return -1;
  }

  c = rpc_conn_acquire();
  if( !c ) return -1;

  xdr_init( &buf, (uint8_t *)c->buf, c->count );
  xdr_encode_string( &buf, modname );
  xdr_encode_uint32( &buf, FVM_MODE_UPDATESTATE );
  xdr_encode_opaque( &buf, (uint8_t *)m->data, m->datasize );
  sts = raft_cluster_command( clid, (char *)buf.buf, buf.offset, NULL );

  rpc_conn_release( c );
  
  return sts;
}

/* ------------------- rpc interface ---------------- */

static struct rpc_program *alloc_program( uint32_t prog, uint32_t vers, int nprocs, rpc_proc_t proccb ) {
  struct rpc_program *pg;
  struct rpc_version *vs;
  struct rpc_proc *pc;
  int i;
  
  pg = malloc( sizeof(*pg) );
  memset( pg, 0, sizeof(*pg) );
  pg->prog = prog;
  vs = malloc( sizeof(*vs) );
  memset( vs, 0, sizeof(*vs) );
  vs->vers = vers;
  pg->vers = vs;

  pc = malloc( sizeof(*pc) * (nprocs + 1) );
  memset( pc, 0, sizeof(*pc) * (nprocs + 1) );
  for( i = 0; i < nprocs; i++ ) {
    pc[i].proc = i;
    pc[i].fn = proccb;
  }
  vs->procs = pc;
  
  return pg;    
}


/* only export the rpc procedures with names of format Procxxx */
static int get_rpc_procid( struct fvm_module *m, int rpcid ) {
  int i, procid;
  char name[8];
  
  procid = 0;
  for( i = 0; i < m->nprocs; i++ ) {
    memcpy( name, m->procs[i].name, 4 );
    name[5] = '\0';
    if( strcasecmp( name, "proc" ) == 0 ) {
      if( procid == rpcid ) return i;
      procid++;
    }
  }
  
  return -1;
}

static int fvm_rpc_proc( struct rpc_inc *inc ) {
  uint32_t procid;
  int sts, handle;
  struct fvm_module *m;
  struct rpc_conn *conn;
  struct xdr_s argbuf, resbuf;
  
  m = fvm_module_by_progid( inc->msg.u.call.prog, inc->msg.u.call.vers );
  if( !m ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  procid = get_rpc_procid( m, inc->msg.u.call.proc );
  if( (procid < 0) || (procid >= m->nprocs) ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  fvm_log( LOG_LVL_DEBUG, "fvm_rpc_proc %s %s", m->name, m->procs[procid].name );

  xdr_init( &argbuf, inc->xdr.buf + inc->xdr.offset, inc->xdr.count - inc->xdr.offset );

  conn = rpc_conn_acquire();
  xdr_init( &resbuf, conn->buf, conn->count );
  sts = fvm_run( m, procid, &argbuf, &resbuf );
  if( sts ) {
    rpc_conn_release( conn );
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  }
	      
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  sts = xdr_encode_fixed( &inc->xdr, resbuf.buf, resbuf.count );
  rpc_complete_accept_reply( inc, handle );

  rpc_conn_release( conn );
  
  return 0;
}

static int fvm_register_program( char *modname ) {
  struct fvm_module *m;
  struct rpc_program *pg;
  
  m = fvm_module_by_name( modname );
  if( !m ) return -1;

  if( !m->progid || !m->versid ) return -1;
  
  pg = alloc_program( m->progid, m->versid, m->nprocs, fvm_rpc_proc );
  rpc_program_register( pg );
  return 0;
}

static int fvm_unregister_program( char *modname ) {
  struct fvm_module *m;
  struct rpc_program *p;
  struct rpc_version *vs;
  struct rpc_proc *pc;
  
  m = fvm_module_by_name( modname );
  if( !m ) return -1;
  
  rpc_program_find( m->progid, m->versid, 0, &p, &vs, &pc );
  if( !p ) return -1;

  rpc_program_unregister( p );
  free( p->vers->procs );
  free( p->vers );
  free( p );
  return 0;  
}

static int fvm_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm_proc_list( struct rpc_inc *inc ) {
  int handle, i;
  struct fvm_module *m;

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );  
  m = glob.modules;
  while( m ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_string( &inc->xdr, m->name );
    xdr_encode_uint32( &inc->xdr, m->progid );
    xdr_encode_uint32( &inc->xdr, m->versid );
    xdr_encode_uint32( &inc->xdr, m->datasize );
    xdr_encode_uint32( &inc->xdr, m->textsize );    
    xdr_encode_uint32( &inc->xdr, m->nprocs );
    for( i = 0; i < m->nprocs; i++ ) {
      xdr_encode_string( &inc->xdr, m->procs[i].name ); 
      xdr_encode_uint32( &inc->xdr, m->procs[i].address );     
      xdr_encode_uint64( &inc->xdr, m->procs[i].siginfo );
    }
    
    m = m->next;
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_load( struct rpc_inc *inc ) {
  int handle, registerp;
  char *bufp;
  int lenp, sts;
  struct fvm_module *modulep;
  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &registerp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_module_load( bufp, lenp, &modulep );
  if( sts ) goto done;
  
  if( registerp ) {
    /* register as rpc program */
    fvm_unregister_program( modulep->name );
    fvm_register_program( modulep->name );
  }

 done:
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_unload( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_module_unload( name );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm_proc_run( struct rpc_inc *inc ) {
  int handle, sts;
  char modname[FVM_MAX_NAME], procname[FVM_MAX_NAME];
  char *bufp = NULL;
  int lenp;
  struct rpc_conn *conn = NULL;
  struct xdr_s argbuf, resbuf;
  uint32_t procid;
  struct fvm_module *m;
    
  sts = xdr_decode_string( &inc->xdr, modname, sizeof(modname) );
  if( !sts ) sts = xdr_decode_string( &inc->xdr, procname, sizeof(procname) );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  fvm_log( LOG_LVL_DEBUG, "fvm_proc_run mod=%s proc=%s buflen=%u", modname, procname, lenp );

  m = fvm_module_by_name( modname );
  if( !m ) {
    sts = -1;
    goto done;
  }
  
  procid = fvm_procid_by_name( m, procname );
  if( procid < 0 ) {
    sts = -1;
    goto done;
  }

  xdr_init( &argbuf, (uint8_t *)bufp, lenp );
  conn = rpc_conn_acquire();
  xdr_init( &resbuf, conn->buf, conn->count );
  sts = fvm_run( m, procid, &argbuf, &resbuf );

 done:  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
  if( !sts ) xdr_encode_opaque( &inc->xdr, (uint8_t *)resbuf.buf, resbuf.count );
  rpc_complete_accept_reply( inc, handle );

  if( conn ) rpc_conn_release( conn );
    
  return 0;
}

static int fvm_proc_clrun( struct rpc_inc *inc ) {
  int handle, sts;
  char modname[FVM_MAX_NAME], procname[FVM_MAX_NAME];
  char *bufp = NULL;
  int lenp;
  uint64_t clid;

  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_string( &inc->xdr, modname, sizeof(modname) );
  if( !sts ) sts = xdr_decode_string( &inc->xdr, procname, sizeof(procname) );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  fvm_log( LOG_LVL_DEBUG, "fvm_proc_clrun clid=%"PRIx64" mod=%s proc=%s buflen=%u",
	   clid, modname, procname, lenp );

  sts = fvm_cluster_run( clid, modname, procname, bufp, lenp );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}


static struct rpc_proc fvm_procs[] = {
  { 0, fvm_proc_null },
  { 1, fvm_proc_list },
  { 2, fvm_proc_load },
  { 3, fvm_proc_unload },
  { 4, fvm_proc_run },
  { 5, fvm_proc_clrun },
  
  { 0, NULL }
};

static struct rpc_version fvm_vers = {
  NULL, FVM_RPC_VERS, fvm_procs
};

static struct rpc_program fvm_prog = {
  NULL, FVM_RPC_PROG, &fvm_vers
};

struct fvm_iterator {
  struct rpc_iterator iter;
  char modname[FVM_MAX_NAME];
  uint32_t procid;
};

static void fvm_module_iter( struct rpc_iterator *iter ) {
  struct fvm_iterator *fiter;
  struct fvm_module *m;

  fiter = (struct fvm_iterator *)iter;
  m = fvm_module_by_name( fiter->modname );
  if( !m ) return;
  fvm_run( m, fiter->procid, NULL, NULL );
}

void fvm_rpc_register( void ) {
  int sts;
  uint64_t id, key;
  struct freg_entry entry;
  char path[256];
  struct fvm_module *m;
  uint32_t registerp;

  rpc_program_register( &fvm_prog );
  raft_app_register( &fvm_app );  

  /* 
   * load modules registered in /fju/fvm/modules/MODNAME/path str 
   * Run initialization routine /fju/fvm/modules/MODNAME/initproc str 
   * Setup service routines /fju/fvm/modules/MODNAME/service str procname 
   * Register rpc programs /fju/fvm/modules/MODNAME/register u32 
   */

  sts = freg_subkey( NULL, 0, "/fju/fvm/modules", FREG_CREATE, &key );
  if( sts ) return;
  id = 0;
  while( !freg_next( NULL, key, id, &entry ) ) {
    if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
      sts = freg_get_by_name( NULL, entry.id, "path", FREG_TYPE_STRING, path, sizeof(path), NULL );
      if( sts ) goto cont;
      sts = fvm_module_load_file( path, &m );
      if( sts ) goto cont;

      sts = freg_get_by_name( NULL, entry.id, "service", FREG_TYPE_STRING, path, sizeof(path), NULL );
      if( sts && (fvm_procid_by_name( m, "service" ) >= 0) ) {
	strcpy( path, "service" );
	sts = 0;
      }
      
      if( !sts ) {
	struct fvm_iterator *iter = malloc( sizeof(*iter) );
	memset( iter, 0, sizeof(*iter) );
	iter->iter.cb = fvm_module_iter;
	strcpy( iter->modname, m->name );
	iter->procid = fvm_procid_by_name( m, path );
	rpc_iterator_register( &iter->iter );
      }

      registerp = (m->progid ? 1 : 0);
      sts = freg_get_by_name( NULL, entry.id, "register", FREG_TYPE_UINT32, (char *)&registerp, sizeof(registerp), NULL );
      if( registerp ) {
	fvm_register_program( m->name );
      }

    }
  cont:
    id = entry.id;
  }

}

void fvm_setdebug( int debugmode ) {
  glob.debug = debugmode;
}
