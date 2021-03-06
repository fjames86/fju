
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
#include <fju/hostreg.h>
#include <fju/rpcd.h>
#include <fju/programs.h>
#include <fju/freg.h>
#include <fju/sec.h>

#include "fvm-private.h"

log_deflogger(fvm_log,"FVM");

static int fvm_unregister_program( char *modname );
static void fvm_unregister_iterator( char *modname );
static int fvm_init_module( char *modname );
static void fvm_register_iterator( char *modname, int procid, int period );
static int fvm_register_program( char *modname );

struct fvm_iterator {
  struct rpc_iterator iter;
  uint32_t prochandle;
  struct fvm_iterator *next;
};

static struct {
  struct fvm_module *modules;
  uint32_t max_steps;
  uint32_t max_runtime;
  uint32_t debug;
  struct fvm_iterator *iterators;
  uint32_t moduletag;
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
  sts = xdr_decode_uint64( xdr, &x->timestamp );
  if( sts ) return sts;
  return 0;
}

static int get_init_proc( struct fvm_module *m, char *procname ) {
  int sts, procid;
  char path[256];
  sprintf( path, "/fju/fvm/modules/%s/init", m->name );
  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_STRING, procname, FVM_MAX_NAME, NULL );
  if( !sts ) return 0;

  procid = fvm_procid_by_name( m, "init" );
  if( procid >= 0 ) {
    strcpy( procname, m->procs[procid].name );
    return 0;
  }
  
  return -1;
}

static int get_exit_proc( struct fvm_module *m, char *procname ) {
  int sts, procid;
  char path[256];
  sprintf( path, "/fju/fvm/modules/%s/exit", m->name );
  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_STRING, procname, FVM_MAX_NAME, NULL );
  if( !sts ) return 0;

  procid = fvm_procid_by_name( m, "exit" );
  if( procid >= 0 ) {
    strcpy( procname, m->procs[procid].name );
    return 0;
  }
  
  return -1;
}

static int get_service_proc( struct fvm_module *m, char *procname, int *service_period ) {
  int sts, procid;
  char path[256];

  sprintf( path, "/fju/fvm/modules/%s/service-period", m->name );  
  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_UINT32, (char *)service_period, 4, NULL );
  if( sts ) *service_period = 1000;

  sprintf( path, "/fju/fvm/modules/%s/service", m->name );  
  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_STRING, procname, FVM_MAX_NAME, NULL );
  if( !sts ) return 0;

  procid = fvm_procid_by_name( m, "service" );
  if( procid >= 0 ) {
    strcpy( procname, m->procs[procid].name );
    return 0;
  }
  
  return -1;
}

static void fvm_module_postload( struct fvm_module *module ) {
  int sts;
  char procname[FVM_MAX_NAME];

  /* possibly load initial data segment */
  fvm_module_loaddata( module->name, 0 );
  
  /* Run init proc */
  sts = get_init_proc( module, procname );
  if( !sts ) {
    fvm_log( LOG_LVL_TRACE, "fvm_module_postload: %s running init proc %s", module->name, procname );
    sts = fvm_run( module, fvm_procid_by_name( module, procname ), NULL, NULL );
    if( sts ) fvm_log( LOG_LVL_TRACE, "fvm_module_postload: init routine failed" );
  }

  /* register service routine if rpcdp=true and procedure named "Service" exists */
  if( rpcdp() ) {
    int service_period;
    sts = get_service_proc( module, procname, &service_period );
    if( !sts ) {
      fvm_log( LOG_LVL_TRACE, "fvm_module_postload %s registering service proc %s", module->name, procname );
      fvm_register_iterator( module->name, fvm_procid_by_name( module, procname ), service_period );      
    }

    if( module->progid ) {
      fvm_log( LOG_LVL_TRACE, "fvm_module_postload %s register as rpc program", module->name );
      fvm_register_program( module->name );
    }
  }
}

static int fvm_parse_module( char *buf, int size, struct fvm_headerinfo *hdrp, char **textp ) {
  /* parse header, load data and text segments */
  struct fvm_headerinfo hdr;
  int i, sts;
  struct xdr_s xdr;
  
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

  *hdrp = hdr;
  *textp = (char *)xdr.buf + xdr.offset;
  return 0;
}

int fvm_module_load( char *buf, int size, uint32_t flags, struct fvm_module **modulep ) {
  /* parse header, load data and text segments */
  struct fvm_headerinfo hdr;
  struct fvm_module *module;
  int i, sts;
  char *textp;
  
  sts = fvm_parse_module( buf, size, &hdr, &textp );
  if( sts ) return sts;
  
  if( fvm_module_by_name( hdr.name ) ) {
    if( flags & FVM_RELOAD ) {
      sts = fvm_module_unload( hdr.name );
      if( sts ) {
	fvm_log( LOG_LVL_ERROR, "Failed to unload existing module %s", hdr.name );
	return -1;
      }
    } else {
      fvm_log( LOG_LVL_ERROR, "Module %s already registered", hdr.name );
      return -1;
    }
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
  module->timestamp = hdr.timestamp;
  module->flags = FVM_MODULE_DYNAMIC;
  memset( module->data, 0, hdr.datasize );
  memcpy( module->text, textp, hdr.textsize );

  {
    char timestr[64];
    fvm_log( LOG_LVL_INFO, "fvm_module_load %s timestamp=%s", module->name, sec_timestr( hdr.timestamp, timestr ) );
  }

  /* register module with system */
  sts = fvm_module_register( module );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "fvm_module_load failed to register %s", module->name );
    free( module );    
    return -1;
  }
  
  if( modulep ) *modulep = module;

  return 0;
}

int fvm_module_register( struct fvm_module *mod ) {

  fvm_log( LOG_LVL_INFO, "fvm_module_register %s", mod->name );
  
  /* forbid if name conflict */
  if( fvm_module_by_name( mod->name ) ) {
    fvm_log( LOG_LVL_ERROR, "fvm_module_register %s exists", mod->name );
    return -1;
  }

  /* 
   * Choose a tag not already taken.
   * Note that this would cause an infinite loop if we hit 64k modules but that isn't a realistic scenario.
   */  
  do {
    mod->tag = (glob.moduletag + 1) % 0x10000;
  } while( fvm_module_by_tag( mod->tag ) );
  glob.moduletag = mod->tag;

  /* register module */
  mod->next = glob.modules;
  glob.modules = mod;

  fvm_module_postload( mod );
  
  return 0;
}

int fvm_module_load_file( char *filename, uint32_t flags, struct fvm_module **modulep ) {
  struct mmf_s mmf;
  int sts;
  sts = mmf_open2( filename, &mmf, MMF_OPEN_EXISTING );
  if( sts ) return sts;
  mmf_remap( &mmf, mmf.fsize );
  sts = fvm_module_load( mmf.file, mmf.fsize, flags, modulep );
  mmf_close( &mmf );
  return sts;
}

int fvm_module_unregister( struct fvm_module *module ) {
  struct fvm_module *m, *prev;
  int sts;
  char procname[FVM_MAX_NAME];
  
  prev = NULL;
  m = glob.modules;
  while( m ) {
    if( m == module ) {
      /* Run exit proc */
      sts = get_exit_proc( m, procname );
      if( !sts ) {
	fvm_log( LOG_LVL_TRACE, "fvm_module_unregister: %s running exit proc %s", m->name, procname );
	sts = fvm_run( m, fvm_procid_by_name( m, procname ), NULL, NULL );
	if( sts ) fvm_log( LOG_LVL_TRACE, "fvm_module_unregister: exit routine failed" );
      }
      
      /* unload any rpc program, if any */
      fvm_unregister_program( module->name );

      /* unregister any iterator */
      fvm_unregister_iterator( module->name );
      
      if( prev ) prev->next = m->next;
      else glob.modules = m->next;
      return 0;
    }
    prev = m;
    m = m->next;
  }
  return -1;
}

int fvm_module_unload( char *modname ) {
  struct fvm_module *m, *prev;
  
  m = glob.modules;
  prev = NULL;
  while( m ) {
    if( strcasecmp( m->name, modname ) == 0 ) {
      fvm_log( LOG_LVL_INFO, "fvm_module_unload %s", modname );

      fvm_module_unregister( m );

      if( m->flags & FVM_MODULE_DYNAMIC ) {
	free( m );
      }
      
      return 0;
    }
    prev = m;
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
    
struct fvm_module *fvm_module_by_tag( int tag ) {
  struct fvm_module *m;
  m = glob.modules;
  while( m ) {
    if( m->tag == tag ) return m;
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

int fvm_procid_by_addr( struct fvm_module *module, int address ) {
  int i;
  for( i = 0; i < module->nprocs; i++ ) {
    if( module->procs[i].address == address ) return i;
  }
  return -1;
}

int fvm_handle_by_name( char *modname, char *procname, uint32_t *phandle ) {
  struct fvm_module *m;
  int procid;

  m = fvm_module_by_name( modname );
  if( !m ) return -1;

  procid = fvm_procid_by_name( m, procname );
  if( procid < 0 ) return -1;

  *phandle = (m->tag << 16) | procid;
  return 0;
}

int fvm_handle_by_procid( char *modname, int procid, uint32_t *phandle ) {
  struct fvm_module *m;

  m = fvm_module_by_name( modname );
  if( !m ) return -1;

  if( (procid < 0) || (procid >= m->nprocs) ) return -1;

  *phandle = (m->tag << 16) | procid;
  return 0;
}

int fvm_proc_by_handle( uint32_t phandle, struct fvm_module **m, int *procid ) {
  uint32_t mtag, pid;
  struct fvm_module *mp;
  
  mtag = (phandle >> 16) & 0xffff;
  pid = (phandle & 0xffff);

  mp = fvm_module_by_tag( mtag );
  if( !mp ) return -1;

  if( pid >= mp->nprocs ) return -1;

  *m = mp;
  *procid = pid;
  return 0;
}



/* --------------- runtime ------------------- */

static int fvm_push( struct fvm_state *state, uint32_t u32 ) {
  if( state->sp >= FVM_MAX_STACK - 4 ) return -1;
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
  if( len < 0 ) return NULL;
  
  if( (addr >= FVM_ADDR_DATA) && ((addr + len) <= (FVM_ADDR_DATA + state->module->datasize)) ) {
    return &state->module->data[addr - FVM_ADDR_DATA];
  }

  if( !writeable ) {
    if( (addr >= FVM_ADDR_TEXT) && ((addr + len) <= (FVM_ADDR_TEXT + state->module->textsize)) ) {
      return &state->module->text[addr - FVM_ADDR_TEXT];
    }
  }
  
  if( (addr >= FVM_ADDR_STACK) && ((addr + len) <= (FVM_ADDR_STACK + FVM_MAX_STACK)) ) {
    return &state->stack[addr - FVM_ADDR_STACK];
  }
  return NULL;  
}

char *fvm_getstr( struct fvm_state *state, uint32_t addr ) {
  char *ptr, *p;
  
  ptr = fvm_getptr( state, addr, 1, 0 );
  if( !ptr ) return NULL;

  /* check string is null terminated within memory bounds */
  p = ptr;
  while( 1 ) {
    if( !*p ) break;
    
    addr++;
    if( !fvm_getptr( state, addr, 1, 0 ) ) return NULL;    
    p++;
  }
  
  return ptr;
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
static uint32_t fvm_read_pcu32( struct fvm_state *state ) {
  uint32_t u;
  uint32_t addr = state->pc;
  u = 0;
  if( (addr >= FVM_ADDR_TEXT) && (addr < (FVM_ADDR_TEXT + state->module->textsize)) ) {
    memcpy( &u, &state->module->text[addr - FVM_ADDR_TEXT], 4 );
  }
  state->pc += 4;  
  return u;
}

int fvm_write_u32( struct fvm_state *state, uint32_t addr, uint32_t u ) {

  if( (addr >= FVM_ADDR_DATA) && (addr <= (FVM_ADDR_DATA + state->module->datasize - 4)) ) {
    memcpy( &state->module->data[addr - FVM_ADDR_DATA], &u, 4 );
    return 0;
  }
  if( (addr >= FVM_ADDR_STACK) && (addr <= (FVM_ADDR_STACK + FVM_MAX_STACK - 4)) ) {
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
   { OP_LDIZ, "LDIZ", 0, 4 }, /* load zero */
   { OP_LDI16, "LDI16", 2, 4 }, /* load 16 bit immediate */
   { OP_INC, "INC", 0, 0 }, /* increment by 1 */
   { OP_DEC, "DEC", 0, 0 }, /* decrement by 1 */
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
    fvm_log( LOG_LVL_ERROR, "fvm_step bad pc" );
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
    u32 = fvm_read_pcu32( state );
    if( fvm_push( state, u32 ) ) return -1;
    break;
  case OP_LDI16:
    i16 = (int16_t)fvm_read_pcu16( state );
    if( fvm_push( state, (uint32_t)(int32_t)i16 ) ) return -1;
    break;    
  case OP_LDIZ:
    if( fvm_push( state, 0 ) ) return -1;
    break;    
  case OP_LEA:
    i16 = (int16_t)fvm_read_pcu16( state );
    if( fvm_push( state, ((int)state->pc) + i16 ) ) return -1;
    break;
  case OP_ADDSP:
    u16 = fvm_read_pcu16( state );
    if( state->sp + u16 > FVM_MAX_STACK ) return -1;
    state->sp += u16;
    break;
  case OP_SUBSP:
    u16 = fvm_read_pcu16( state );
    if( state->sp < u16 ) return -1;
    state->sp -= u16;
    break;
  case OP_CALL:
    u16 = fvm_read_pcu16( state );
    if( fvm_push( state, state->pc ) ) return -1;
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
	fvm_log( LOG_LVL_ERROR, "fvm_step return to invalid address %x frame %u", u32, state->frame );
	return -1;
      }
    }
    state->pc = u32;
    state->frame--;
    break;
  case OP_LEASP:
    u16 = fvm_read_pcu16( state );
    if( fvm_push( state, FVM_ADDR_STACK + state->sp - u16 ) ) return -1;
    break;
  case OP_LDSP:
    u16 = fvm_read_pcu16( state );
    u32 = fvm_read_u32( state, FVM_ADDR_STACK + state->sp - u16 );
    if( fvm_push( state, u32 ) ) return -1;
    break;
  case OP_STSP:
    u16 = fvm_read_pcu16( state );
    addr = FVM_ADDR_STACK + state->sp - u16;
    u32 = fvm_pop( state );
    if( fvm_write_u32( state, addr, u32 ) < 0 ) return -1;
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
  case OP_INC:
    u32 = fvm_pop( state );
    fvm_push( state, u32 + 1 );      
    break;
  case OP_DEC:
    u32 = fvm_pop( state );
    fvm_push( state, u32 - 1 );      
    break;        
  case OP_LD:
    u32 = fvm_pop( state );
    u32 = fvm_read_u32( state, u32 );
    fvm_push( state, u32 );
    break;
  case OP_ST:
    u32 = fvm_pop( state );
    addr = fvm_pop( state );
    if( fvm_write_u32( state, addr, u32 ) < 0 ) return -1;
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
    if( sts ) {
      fvm_log( LOG_LVL_ERROR, "fvm_step syscall %u failed", u16 );
      return -1;
    }
    break;
  default:
    printf( "Invalid opcode %u\n", op );
    fvm_log( LOG_LVL_ERROR, "fvm_step invalid opcode" );
    return -1;
  }

  return 0;
}


static int fvm_run_proc( struct fvm_module *module, uint32_t procaddr, uint64_t siginfo, struct xdr_s *argbuf , struct xdr_s *resbuf, uint32_t *nsteps ) {
  struct fvm_state state;
  uint64_t start, now;
  int sts;
  uint32_t isvar[FVM_MAX_PARAM], vartype[FVM_MAX_PARAM], u32[FVM_MAX_PARAM];
  uint32_t u;
  int i, nargs, len;
  char *str, *buf;

  if( (procaddr < FVM_ADDR_TEXT) || (procaddr >= (FVM_ADDR_TEXT + module->textsize)) ) {
    fvm_log( LOG_LVL_ERROR, "fvm_run bad procaddr %x", procaddr );
    return -1;
  }

  memset( &state, 0, sizeof(state) );
  state.module = module;
  state.sp = 0;
  state.pc = procaddr;

  /* 
   * prepare args on stack:
   * <string/opaque buffers><u32 args and pointers to the string/opaque args><dummy return address> 
   */

  nargs = FVM_SIGINFO_NARGS(siginfo);

  for( i = 0; i < nargs; i++ ) {
    isvar[i] = FVM_SIGINFO_ISVAR(siginfo,i);
    vartype[i] = FVM_SIGINFO_VARTYPE(siginfo,i);

    if( isvar[i] ) {
      /* output arg: reserve space for result pointer */
      u32[i] = FVM_ADDR_STACK + state.sp; /* address of result value */
      state.sp += 4;
    } else {
      if( !argbuf ) {
	fvm_log( LOG_LVL_ERROR, "fvm_run need args" );
	return -1;
      }
      
      /* input arg */
      switch( vartype[i] ) {
      case VAR_TYPE_U32:
	if( (i < (nargs - 1)) && (FVM_SIGINFO_VARTYPE(siginfo, i + 1) == VAR_TYPE_OPAQUE) ) {
	  /* don't decode the u32 if the next param is opaque. that's because this will receive the length */
	} else {
	  sts = xdr_decode_uint32( argbuf, &u32[i] );
	  if( sts ) {
	    fvm_log( LOG_LVL_ERROR, "fvm_run xdr error u32 i=%d", i );
	    return sts;
	  }
	}
	break;
      case VAR_TYPE_STRING:
	sts = xdr_decode_string( argbuf, state.stack + state.sp, FVM_MAX_STACK - state.sp );
	if( sts ) {
	  fvm_log( LOG_LVL_ERROR, "fvm_run xdr error string i=%d argbuf=%d/%d", i, argbuf->offset, argbuf->count );
	  return sts;
	}
	u32[i] = FVM_ADDR_STACK + state.sp;
	len = strlen( state.stack + state.sp ) + 1;
	if( len % 4 ) len += 4 - (len % 4);
	state.sp += len;
	break;	
      case VAR_TYPE_OPAQUE:
	len = FVM_MAX_STACK - state.sp;
	sts = xdr_decode_opaque( argbuf, (uint8_t *)state.stack + state.sp, &len );
	if( sts ) {
	  fvm_log( LOG_LVL_ERROR, "fvm_run xdr error opaque i=%d argbuf=%d/%d", i, argbuf->offset, argbuf->count );
	  return sts;
	}
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
    if( sts ) {
      fvm_log( LOG_LVL_ERROR, "fvm_run step failed" );
      return -1;
    }
    state.nsteps++;
    
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

  if( nsteps ) *nsteps = state.nsteps;
  
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
	  sts = xdr_encode_uint32( resbuf, u );
	  if( sts ) {
	    fvm_log( LOG_LVL_ERROR, "fvm_run xdr error decoding result" );	    
	    return sts;
	  }
	}
	break;
      case VAR_TYPE_STRING:
	u = fvm_read_u32( &state, u32[i] );
	str = fvm_getstr( &state, u );
	sts = xdr_encode_string( resbuf, str ? str : "" );
	if( sts ) {
	  fvm_log( LOG_LVL_ERROR, "fvm_run xdr error decoding result" );	    	  
	  return sts;
	}
	break;
      case VAR_TYPE_OPAQUE:
	u = fvm_read_u32( &state, u32[i] );
	len = u32[i - 1];	
	buf = fvm_getptr( &state, u, len, 0 );
	if( !buf ) {
	  fvm_log( LOG_LVL_ERROR, "fvm_run failed to get opaque pointer" );
	}
	
	sts = xdr_encode_opaque( resbuf, (uint8_t *)buf, buf ? len : 0 );
	if( sts ) {
	  fvm_log( LOG_LVL_ERROR, "fvm_run xdr error decoding result" );	    	  
	  return sts;
	}
	break;
      }
    }
  }

  resbuf->count = resbuf->offset;
  resbuf->offset = 0;
  
  return 0;
  
}

int fvm_run( struct fvm_module *module, uint32_t procid, struct xdr_s *argbuf , struct xdr_s *resbuf ) {
  uint32_t nsteps;
  int sts;

  if( module->flags & FVM_MODULE_DISABLED ) {
    fvm_log( LOG_LVL_WARN, "fvm_run module %s disabled", module->name );
    return -1;
  }
  
  if( (procid < 0) || (procid >= module->nprocs) ) {
    fvm_log( LOG_LVL_ERROR, "fvm_run bad procid" );
    return -1;
  }

  if( module->flags & FVM_MODULE_NATIVE ) {
    if( !module->procs[procid].nativeproc ) return -1;
    sts = module->procs[procid].nativeproc( argbuf, resbuf );
    module->procs[procid].perfdata.rcount++;
    return sts;
  }
  
  sts = fvm_run_proc( module, module->procs[procid].address, module->procs[procid].siginfo, argbuf, resbuf, &nsteps );
  module->procs[procid].perfdata.rcount++;
  module->procs[procid].perfdata.nsteps += nsteps;

  return sts;
}

int fvm_run_by_name( char *modname, char *procname, struct xdr_s *args, struct xdr_s *res ) {
  struct fvm_module *m;
  int procid;
  m = fvm_module_by_name( modname );
  if( !m ) return -1;
  procid = fvm_procid_by_name( m, procname );
  if( procid < 0 ) return -1;
  return fvm_run( m, procid, args, res );
}

int fvm_run_by_handle( int handle, struct xdr_s *args, struct xdr_s *res ) {
  struct fvm_module *m;
  int procid;
  int sts;
  sts = fvm_proc_by_handle( handle, &m, &procid );
  if( sts ) return -1;
  return fvm_run( m, procid, args, res );
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


#define RPCPROC_SIGINFO 0x800000000000d10L
/* only export the rpc procedures with names of format Procxxx that have correct signature */
static int get_rpc_procid( struct fvm_module *m, int rpcid ) {
  int i, procid;
  char name[8];
  
  procid = 0;
  for( i = 0; i < m->nprocs; i++ ) {    
    memcpy( name, m->procs[i].name, 4 );
    name[4] = '\0';
    if( (strcasecmp( name, "proc" ) == 0) && (m->procs[i].siginfo == RPCPROC_SIGINFO) ) {
      if( procid == rpcid ) {
	return i;
      }
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
  if( !m ) {
    fvm_log( LOG_LVL_TRACE, "Unknown module progid=%u", inc->msg.u.call.prog );
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_PROG_UNAVAIL, NULL, NULL );
  }

  procid = get_rpc_procid( m, inc->msg.u.call.proc );
  if( (procid < 0) || (procid >= m->nprocs) ) {
    fvm_log( LOG_LVL_TRACE, "Unknown proc %u", inc->msg.u.call.proc );
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_PROC_UNAVAIL, NULL, NULL );
  }

  fvm_log( LOG_LVL_DEBUG, "fvm_rpc_proc %s %s", m->name, m->procs[procid].name );

  if( (inc->xdr.offset + 4) > inc->xdr.buf_size ) {
    fvm_log( LOG_LVL_ERROR, "fvm_rpc_proc out of buffer space" );
    return -1;
  }
  
  memmove( inc->xdr.buf + inc->xdr.offset + 4, inc->xdr.buf + inc->xdr.offset, inc->xdr.count - inc->xdr.offset );
  xdr_init( &argbuf, inc->xdr.buf + inc->xdr.offset, inc->xdr.count - inc->xdr.offset + 4 );
  xdr_encode_uint32( &argbuf, inc->xdr.count - inc->xdr.offset );
  argbuf.offset = 0;
  
  conn = rpc_conn_acquire();
  xdr_init( &resbuf, conn->buf, conn->count );
  sts = fvm_run( m, procid, &argbuf, &resbuf );
  if( sts ) {
    rpc_conn_release( conn );
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  }
	      
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  sts = xdr_encode_fixed( &inc->xdr, resbuf.buf + 4, resbuf.count - 4 );
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
    xdr_encode_uint64( &inc->xdr, m->timestamp );
    xdr_encode_uint32( &inc->xdr, m->flags );
    xdr_encode_uint32( &inc->xdr, m->nprocs );
    for( i = 0; i < m->nprocs; i++ ) {
      xdr_encode_string( &inc->xdr, m->procs[i].name ); 
      xdr_encode_uint32( &inc->xdr, m->procs[i].address );     
      xdr_encode_uint64( &inc->xdr, m->procs[i].siginfo );
      xdr_encode_uint64( &inc->xdr, m->procs[i].perfdata.nsteps );
      xdr_encode_uint64( &inc->xdr, m->procs[i].perfdata.rcount );
    }
    
    m = m->next;
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_load( struct rpc_inc *inc ) {
  int handle;
  char *bufp;
  int lenp, sts;
  struct fvm_module *modulep;
  uint32_t flags;
  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_module_load( bufp, lenp, flags, &modulep );
  if( sts ) goto done;
  
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

static int fvm_proc_reload( struct rpc_inc *inc ) {
  int handle, sts;
  char modname[FVM_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, modname, sizeof(modname) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_init_module( modname );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_data( struct rpc_inc *inc ) {
  int handle, sts;
  struct fvm_module *m;
  char modname[FVM_MAX_NAME];
  
  sts = xdr_decode_string( &inc->xdr, modname, sizeof(modname) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  m = fvm_module_by_name( modname );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, m ? 1 : 0 );
  if( m ) xdr_encode_opaque( &inc->xdr, (uint8_t *)m->data, m->datasize );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_enable( struct rpc_inc *inc ) {
  int handle, sts;
  int enable, prev;
  char modname[FVM_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, modname, sizeof(modname) );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &enable );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_module_enable( modname, enable, &prev );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
  if( !sts ) xdr_encode_boolean( &inc->xdr, prev );
  rpc_complete_accept_reply( inc, handle );

  return 0;
}

static int fvm_proc_getprocinfo( struct rpc_inc *inc ) {
  int handle, sts, procid;
  struct fvm_module *m;
  char modname[FVM_MAX_NAME], procname[FVM_MAX_NAME];
  
  sts = xdr_decode_string( &inc->xdr, modname, sizeof(modname) );
  if( !sts ) sts = xdr_decode_string( &inc->xdr, procname, sizeof(procname) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  m = fvm_module_by_name( modname );
  if( m ) {
    procid = fvm_procid_by_name( m, procname );
    if( procid >= 0 ) {
      xdr_encode_boolean( &inc->xdr, 1 );
      xdr_encode_uint32( &inc->xdr, m->procs[procid].address );     
      xdr_encode_uint64( &inc->xdr, m->procs[procid].siginfo );
      xdr_encode_uint64( &inc->xdr, m->procs[procid].perfdata.nsteps );
      xdr_encode_uint64( &inc->xdr, m->procs[procid].perfdata.rcount );      
    } else {
      xdr_encode_boolean( &inc->xdr, 0 );
    }
  } else {
    xdr_encode_boolean( &inc->xdr, 0 );
  }
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static struct rpc_proc fvm_procs[] = {
  { 0, fvm_proc_null },
  { 1, fvm_proc_list },
  { 2, fvm_proc_load },
  { 3, fvm_proc_unload },
  { 4, fvm_proc_run },
  // { 5, NULL }, // unused 
  { 6, fvm_proc_reload },
  { 7, fvm_proc_data },
  { 8, fvm_proc_enable },
  { 9, fvm_proc_getprocinfo },
  
  { 0, NULL }
};

static struct rpc_version fvm_vers = {
  NULL, FVM_RPC_VERS, fvm_procs
};

static struct rpc_program fvm_prog = {
  NULL, FVM_RPC_PROG, &fvm_vers
};


static void fvm_module_iter( struct rpc_iterator *iter ) {
  struct fvm_iterator *fiter;
  struct fvm_module *m;
  int sts, procid;
  
  fiter = (struct fvm_iterator *)iter;
  sts = fvm_proc_by_handle( fiter->prochandle, &m, &procid );
  if( sts ) return;

  sts = fvm_run( m, procid, NULL, NULL );
  if( sts ) fvm_log( LOG_LVL_ERROR, "fvm iter failed" );
}

static void fvm_register_iterator( char *modname, int procid, int period ) {
  struct fvm_iterator *iter;
  int sts;
  
  fvm_log( LOG_LVL_INFO, "fvm register iterator %s %u", modname, procid );

  iter = malloc( sizeof(*iter) );
  memset( iter, 0, sizeof(*iter) );
  iter->iter.cb = fvm_module_iter;
  iter->iter.period = period;

  sts = fvm_handle_by_procid( modname, procid, &iter->prochandle );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "fvm_register_iterator bad proc %s/%u", modname, procid );
    free( iter );
    return;
  }
  
  iter->next = glob.iterators;
  glob.iterators = iter;
  
  rpc_iterator_register( &iter->iter );
}

static void fvm_unregister_iterator( char *modname ) {
  struct fvm_iterator *it, *prev;
  struct fvm_module *m;

  m = fvm_module_by_name( modname );
  if( !m ) return;
  
  it = glob.iterators;
  prev = NULL;
  while( it ) {
    if( ((it->prochandle & 0xffff0000) >> 16) == m->tag ) {
      fvm_log( LOG_LVL_TRACE, "fvm unregister iterator %s %u", modname, it->prochandle & 0xffff );
      
      if( prev ) prev->next = it->next;
      else glob.iterators = it->next;
      rpc_iterator_unregister( &it->iter );
      free( it );
      return;
    }
    prev = it;
    it = it->next;
  }
}


static int fvm_init_module( char *modname ) {
  char path[256];
  int sts;
  struct freg_entry entry;
  struct fvm_module *m;
  char *textp;
  int lenp;
  
  /* unload if already existing */
  if( fvm_module_by_name( modname) ) {
    sts = fvm_module_unload( modname );
    if( sts ) {
      fvm_log( LOG_LVL_ERROR, "fvm_init_module failed to unload %s", modname );
      return sts;
    }
  }
  
  sprintf( path, "/fju/fvm/modules/%s", modname );
  sts = freg_entry_by_name( NULL, 0, path, &entry, NULL );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "Failed to load registry entry for module %s", modname );
    return -1;
  }

  sts = freg_get_by_name( NULL, entry.id, "path", FREG_TYPE_STRING, path, sizeof(path), NULL );
  if( !sts ) {
    sts = fvm_module_load_file( path, 0, &m );
    if( sts ) {
      fvm_log( LOG_LVL_ERROR, "Failed to load module file %s", path );
      return -1;
    }
  } else {
    sts = freg_get_by_name( NULL, entry.id, "text", FREG_TYPE_OPAQUE, NULL, 0, &lenp );
    if( !sts ) {
      textp = malloc( lenp );
      if( !textp ) return -1;
      sts = freg_get_by_name( NULL, entry.id, "text", FREG_TYPE_OPAQUE, textp, lenp, &lenp );
      if( sts ) {
	fvm_log( LOG_LVL_ERROR, "Unexpected failure to load text segment from freg" );
	free( textp );
	return -1;
      }
      
      sts = fvm_module_load( textp, lenp, 0, &m );
      
      free( textp );
      
      if( sts ) {
	fvm_log( LOG_LVL_ERROR, "Failed to load module from freg" );
	return -1;
      }
    } else {
      fvm_log( LOG_LVL_ERROR, "Failed to find path or text entry for module %s", modname );
      return -1;
    }
  }
    
  return 0;
}

int fvm_module_savedata( char *modname, int id, int *saveidp ) {
  struct fvm_module *m;
  int sts;
  char path[256];
  uint64_t parentid;
  
  m = fvm_module_by_name( modname );
  if( !m ) return -1;

  sprintf( path, "/fju/fvm/modules/%s", m->name );
  sts = freg_subkey( NULL, 0, path, FREG_CREATE, &parentid );
  if( sts ) return -1;

  /* select an unused id if id=-1 */
  if( id < 0 ) {
    id = 0;
    if( freg_entry_by_name( NULL, parentid, "data", NULL, NULL ) == 0 ) {
      do {
	id++;
	sprintf( path, "data%d", id );
      } while( freg_entry_by_name( NULL, parentid, path, NULL, NULL ) == 0 );
    }
  }
  
  if( id > 0 ) sprintf( path, "data%d", id );
  else sprintf( path, "data" );
  sts = freg_put( NULL, parentid, path, FREG_TYPE_OPAQUE, m->data, m->datasize, NULL );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "Failed to save datasegment for module %s", modname );
    return -1;
  }

  if( saveidp ) *saveidp = id;
  
  return 0;
}

int fvm_module_loaddata( char *modname, int id ) {
  struct fvm_module *m;
  int sts, lenp;
  char path[256];
  
  m = fvm_module_by_name( modname );
  if( !m ) return -1;

  sprintf( path, "/fju/fvm/modules/%s/data", modname );
  if( id > 0 ) sprintf( path + strlen( path ), "%d", id );
  
  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_OPAQUE, NULL, 0, &lenp );
  if( sts ) {
    /* if no entry found for id=0 then pretend it is all zero */
    if( id == 0 ) {
      memset( m->data, 0, m->datasize );
      return 0;
    }
    
    return -1;
  }

  if( lenp != m->datasize ) return -1;
  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_OPAQUE, m->data, m->datasize, &lenp );
  if( sts ) return -1;

  return 0;
}

int fvm_module_remdata( char *modname, int id ) {
  int sts;
  char path[256];
  uint64_t idp;
  
  sprintf( path, "/fju/fvm/modules/%s/data", modname );
  if( id > 0 ) sprintf( path + strlen( path ), "%d", id );

  idp = freg_id_by_name( NULL, path, NULL );
  if( !idp ) return -1;
  sts = freg_rem( NULL, idp );
  
  return sts;
}

int fvm_module_getdata( char *modname, int id, char *buf, int len, int *lenp ) {
  int sts, lp;
  char path[256];
  
  sprintf( path, "/fju/fvm/modules/%s/data", modname );
  if( id > 0 ) sprintf( path + strlen( path ), "%d", id );
  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_OPAQUE, buf, len, &lp );
  if( sts ) return -1;
  if( lenp ) *lenp = lp;
  
  return 0;
}

int fvm_module_savetext( char *modname ) {
  int sts;
  char path[256];
  struct fvm_module *m;
  uint64_t parentid;
  
  m = fvm_module_by_name( modname );
  if( !m  ) return -1;
  
  sprintf( path, "/fju/fvm/modules/%s", m->name );
  sts = freg_subkey( NULL, 0, path, FREG_CREATE, &parentid );
  if( sts ) return -1;

  sts = freg_put( NULL, parentid, "text", FREG_TYPE_OPAQUE, m->text, m->textsize, NULL );
  if( sts ) return -1;
  
  return 0;
}

void fvm_rpc_register( void ) {
  int sts;
  uint64_t id, key;
  struct freg_entry entry;
  
  rpc_program_register( &fvm_prog );

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
      fvm_log( LOG_LVL_INFO, "fvm load registry module %s", entry.name );
      fvm_init_module( entry.name );
    }
    id = entry.id;
  }

  sts = freg_get_by_name( NULL, 0, "/fju/fvm/maxruntime", FREG_TYPE_UINT32, (char *)&glob.max_runtime, 4, NULL );
  sts = freg_get_by_name( NULL, 0, "/fju/fvm/maxsteps", FREG_TYPE_UINT32, (char *)&glob.max_steps, 4, NULL );

}

void fvm_setdebug( int debugmode ) {
  glob.debug = debugmode;
}

int fvm_module_enable( char *modname, int enable, int *prev ) {
  struct fvm_module *m;
  
  m = fvm_module_by_name( modname );
  if( !m ) return -1;

  /* return previous enabled state */
  if( prev ) {
    *prev = (m->flags & FVM_MODULE_DISABLED) ? 0 : 1;
  }

  /* set/clear disabled flag */
  m->flags = (m->flags & ~FVM_MODULE_DISABLED) | (enable ? 0 : FVM_MODULE_DISABLED);
  return 0;
}

