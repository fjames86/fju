
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <WinSock2.h>
#include <Windows.h>
#else
#include <arpa/inet.h>
#endif

#include <inttypes.h>

#include "fvm-private.h"
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/hrauth.h>
#include <fju/hostreg.h>

#include <fju/programs.h>
#include <fju/log.h>
#include <fju/freg.h>
#include <fju/raft.h>


/* rpc interface */

/*
 *Load module
 * Unload module
 * List modules 
 * 
 */

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

static int fvm_rpc_proc( struct rpc_inc *inc ) {
  /* get prog, vers, proc */
  uint32_t progid, procid, type;
  struct fvm_s state;
  int sts, handle, arglength, count;
  struct fvm_module *m;
  char *resp;
  
  progid = inc->msg.u.call.prog;
  m = fvm_module_by_progid( progid );
  if( !m ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  procid = inc->msg.u.call.proc;
  if( procid >= m->header.symcount ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  type = m->symbols[procid].flags & FVM_SYMBOL_TYPE_MASK;
  fvm_log( LOG_LVL_DEBUG, "fvm_rpc_proc progid=%u procid=%u type=%s", progid, procid,
	   type == FVM_SYMBOL_PROC ? "PROC" : type == FVM_SYMBOL_UINT32 ? "UINT32" : type == FVM_SYMBOL_UINT64 ? "UINT64" : type == FVM_SYMBOL_STRING ? "STRING" : "UNKNOWN" );  
  switch( type ) {
  case FVM_SYMBOL_PROC:
    /* initialize state */
    sts = fvm_state_init( &state, progid, procid );
    if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
    
    /* copy args onto fvm stack and set r0 to length */
    arglength = inc->xdr.count - inc->xdr.offset;
    fvm_log( LOG_LVL_TRACE, "fvm_rpc_proc run offset=%u count=%u arglength=%u", inc->xdr.offset, inc->xdr.count, arglength );
    
    fvm_set_args( &state, (char *)(inc->xdr.buf + inc->xdr.offset), arglength );
    sts = fvm_run( &state, 0 );
    if( sts ) {
      fvm_log( LOG_LVL_ERROR, "fvm_rpc_proc fvm_run failed" );
      return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
    }
    
    fvm_log( LOG_LVL_TRACE, "success reply length %d", ntohl( state.reg[FVM_REG_R0] ) );
    rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
    
    count = fvm_get_res( &state, &resp );
    if( count > 0 && count < FVM_MAX_STACK ) {
      sts = xdr_encode_fixed( &inc->xdr, (uint8_t *)resp, count );
      if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
    }
    rpc_complete_accept_reply( inc, handle );
    break;
  case FVM_SYMBOL_UINT32:
    {
      uint32_t u1, u2;
      int b;
      sts = xdr_decode_boolean( &inc->xdr, &b );
      if( !sts && b ) sts = xdr_decode_uint32( &inc->xdr, &u1 );
      if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

      fvm_log( LOG_LVL_TRACE, "getvar uint32 %u:%u set %u", progid, procid, b ? u1 : 0 );
      
      u2 = fvm_read_uint32( m, procid );
      if( b ) fvm_write_uint32( m, procid, u1 );
    
      rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
      xdr_encode_uint32( &inc->xdr, u2 );
      rpc_complete_accept_reply( inc, handle );
    }
    break;
  case FVM_SYMBOL_UINT64:
    {
      uint64_t u1, u2;
      int b;
      sts = xdr_decode_boolean( &inc->xdr, &b );
      if( !sts && b ) sts = xdr_decode_uint64( &inc->xdr, &u1 );
      if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

      fvm_log( LOG_LVL_TRACE, "getvar uint64 %u:%u set %"PRIu64"", progid, procid, b ? u1 : 0 );
      
      u2 = fvm_read_uint64( m, procid );
      if( b ) fvm_write_uint64( m, procid, u1 );
      
      rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
      xdr_encode_uint64( &inc->xdr, u2 );
      rpc_complete_accept_reply( inc, handle );
    }
    break;
  case FVM_SYMBOL_STRING:
    {
      char str1[256], str2[256];
      int b;
      
      sts = xdr_decode_boolean( &inc->xdr, &b );
      if( !sts && b ) sts = xdr_decode_string( &inc->xdr, str1, sizeof(str1) );
      if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

      fvm_log( LOG_LVL_TRACE, "getvar string %u:%u %s%s", progid, procid, b ? "set " : "get ", b ? str1 : "" );
      
      sts = fvm_read_string( m, procid, str2, sizeof(str2) );
      if( sts ) strcpy( str2, "" );
      if( b ) {
	sts = fvm_write_string( m, procid, str1 );
	if( sts ) fvm_log( LOG_LVL_ERROR, "fvm_write_string failed" );
      }
      
      rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
      xdr_encode_string( &inc->xdr, str2 );
      rpc_complete_accept_reply( inc, handle );
    }    
    break;
  default:
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
    break;
  }

  
  return 0;
}

int fvm_register_program( uint32_t progid ) {
  struct fvm_module *m;
  struct rpc_program *pg;
  
  m = fvm_module_by_progid( progid );
  if( !m ) return -1;

  pg = alloc_program( m->header.progid, m->header.versid, m->header.symcount, fvm_rpc_proc );
  rpc_program_register( pg );
  return 0;
}

int fvm_unregister_program( uint32_t progid ) {
  struct fvm_module *m;
  struct rpc_program *p;
  struct rpc_version *vs;
  struct rpc_proc *pc;
  
  m = fvm_module_by_progid( progid );
  if( !m ) return -1;
  
  rpc_program_find( m->header.progid, 0, 0, &p, &vs, &pc );
  if( !p ) return -1;

  rpc_program_unregister( p );
  free( p->vers->procs );
  free( p->vers );
  free( p );
  return 0;  
}

/* ------------------- */

static int fvm_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm_proc_list( struct rpc_inc *inc ) {
  int handle;
  int i, j, n, m;
  struct fvm_module_info *minfo = NULL;
  struct fvm_symbol *sym = NULL;
  int nsym = 0;
  
  /* no args */
  n = fvm_module_list( NULL, 0 );
  if( n > 0 ) {
    minfo = malloc( sizeof(*minfo) * n );
    m = fvm_module_list( minfo, n );
    if( m < n ) n = m;
  }

  nsym = 32;
  sym = malloc( sizeof(*sym) * nsym );
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  for( i = 0; i < n; i++ ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_string( &inc->xdr, minfo[i].name );
    xdr_encode_uint32( &inc->xdr, minfo[i].progid );
    xdr_encode_uint32( &inc->xdr, minfo[i].versid );
    xdr_encode_uint32( &inc->xdr, minfo[i].datasize );
    xdr_encode_uint32( &inc->xdr, minfo[i].textsize );
    xdr_encode_uint32( &inc->xdr, minfo[i].flags );
    xdr_encode_uint64( &inc->xdr, minfo[i].utime );
    m = fvm_module_symbols( minfo[i].progid, sym, nsym );
    if( m > nsym ) {
      nsym = m;
      sym = realloc( sym, sizeof(*sym) * nsym );
    }
    m = fvm_module_symbols( minfo[i].progid, sym, nsym );
    for( j = 0; j < m; j++ ) {
      xdr_encode_boolean( &inc->xdr, 1 );
      xdr_encode_string( &inc->xdr, sym[j].name );
      xdr_encode_uint32( &inc->xdr, sym[j].flags );
    }
    xdr_encode_boolean( &inc->xdr, 0 );
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  
  rpc_complete_accept_reply( inc, handle );

  if( minfo ) free( minfo );
  if( sym ) free( sym );
  
  return 0;
}

static int fvm_proc_load( struct rpc_inc *inc ) {
  int handle;
  char *bufp;
  int lenp, sts;
  uint32_t progid, flags;
  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_module_load_buffer( bufp, lenp, &progid );
  if( sts ) {
    fvm_module_unload( progid );
    sts = fvm_module_load_buffer( bufp, lenp, &progid );
  }

  if( flags & 0x1 ) {
    /* register as rpc program */
    fvm_unregister_program( progid );
    fvm_register_program( progid );
  } else if( flags & 0x2 ) {
    /* run default method and unregister */
    struct fvm_s state;
    sts = fvm_state_init( &state, progid, 0 );
    if( !sts ) {
      fvm_run( &state, -1 );
    }
    fvm_module_unload( progid );
  }
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_unload( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_module_unload( fvm_progid_by_name( name ) );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm_proc_register( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_register_program( fvm_progid_by_name( name ) );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_unregister( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM_MAX_NAME];
  
  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_unregister_program( fvm_progid_by_name( name ) );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm_proc_run( struct rpc_inc *inc ) {
  int handle, sts;
  uint32_t progid, procid;
  char *bufp = NULL;
  int lenp;
  struct fvm_s state;


  sts = xdr_decode_uint32( &inc->xdr, &progid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &procid );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  fvm_log( LOG_LVL_DEBUG, "fvm_proc_run progid=%u procid=%u buflen=%u", progid, procid, lenp );
  sts = fvm_state_init( &state, progid, procid );
  if( sts ) goto done;
  sts = fvm_set_args( &state, bufp, lenp );
  if( sts ) goto done;

  sts = fvm_run( &state, 0 );

 done:
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts == 0 ? 1 : 0 );

  lenp = 0;
  if( !sts ) lenp = fvm_get_res( &state, &bufp );
  xdr_encode_opaque( &inc->xdr, (uint8_t *)bufp, lenp );
  fvm_log( LOG_LVL_DEBUG, "fvm_proc_run progid=%u procid=%u reslen=%u", progid, procid, lenp );
  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_readvar( struct rpc_inc *inc ) {
  int handle, sts;
  uint32_t progid, procid;
  struct fvm_module *m;
  struct fvm_symbol *sym;
  uint32_t u32, type;
  uint64_t u64;
  char str[256];

  sts = xdr_decode_uint32( &inc->xdr, &progid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &procid );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  fvm_log( LOG_LVL_DEBUG, "fvm_proc_readvar progid=%u procid=%u", progid, procid );

  m = fvm_module_by_progid( progid );
  if( !m ) {
    sts = -1;
    goto done;
  }

  if( procid >= m->header.symcount ) {
    sts = -1;
    goto done;
  }

  sym = &m->symbols[procid];
  type = sym->flags & FVM_SYMBOL_TYPE_MASK;

  switch( type ) {
  case FVM_SYMBOL_UINT32:
    u32 = fvm_read_uint32( m, procid );    
    break;
  case FVM_SYMBOL_UINT64:
    u64 = fvm_read_uint64( m, procid );
    break;
  case FVM_SYMBOL_STRING:
    sts = fvm_read_string( m, procid, str, sizeof(str) );
    break;
  default:
    sts = -1;
  }

 done:
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts == 0 ? 1 : 0 );
  if( sts == 0 ) {
    xdr_encode_uint32( &inc->xdr, type );
    switch( type ) {
    case FVM_SYMBOL_UINT32:
      xdr_encode_uint32( &inc->xdr, u32 );
      break;
    case FVM_SYMBOL_UINT64:
      xdr_encode_uint64( &inc->xdr, u64 );
      break;
    case FVM_SYMBOL_STRING:
      xdr_encode_string( &inc->xdr, str );
      break;      
    }
  }   
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_writevar( struct rpc_inc *inc ) {
  int handle, sts;
  uint32_t progid, procid;
  struct fvm_module *m;
  struct fvm_symbol *sym;
  uint32_t u32, type;
  uint64_t u64;
  char str[256];

  sts = xdr_decode_uint32( &inc->xdr, &progid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &procid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &type );
  if( !sts ) {
    switch( type ) {
    case FVM_SYMBOL_UINT32:
      sts = xdr_decode_uint32( &inc->xdr, &u32 );
      break;
    case FVM_SYMBOL_UINT64:
      sts = xdr_decode_uint64( &inc->xdr, &u64 );
      break;
    case FVM_SYMBOL_STRING:
      sts = xdr_decode_string( &inc->xdr, str, sizeof(str) );
      break;
    default:
      sts = -1;
      break;
    }
  }
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  fvm_log( LOG_LVL_DEBUG, "fvm_proc_writevar progid=%u procid=%u type=%u", progid, procid, type );

  m = fvm_module_by_progid( progid );
  if( !m ) {
    sts = -1;
    goto done;
  }

  if( procid >= m->header.symcount ) {
    sts = -1;
    goto done;
  }

  sym = &m->symbols[procid];
  if( (sym->flags & FVM_SYMBOL_TYPE_MASK) != type ) {
    sts = -1;
    goto done;
  }

  switch( type ) {
  case FVM_SYMBOL_UINT32:
    sts = fvm_write_uint32( m, procid, u32 );
    break;
  case FVM_SYMBOL_UINT64:
    sts = fvm_write_uint64( m, procid, u64 );
    break;
  case FVM_SYMBOL_STRING:
    sts = fvm_write_string( m, procid, str );
    break;
  default:
    sts = -1;
  }

 done:
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts == 0 ? 1 : 0 );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_clusterrun( struct rpc_inc *inc ) {
  int handle, sts;
  uint32_t progid, procid;
  char *bufp = NULL;
  int lenp;
  uint64_t clid;

  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &progid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &procid );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  fvm_log( LOG_LVL_DEBUG, "fvm_proc_clusterrun clid=%"PRIx64" progid=%u procid=%u buflen=%u",
	   clid, progid, procid, lenp );

  sts = fvm_cluster_run( clid, progid, procid, bufp, lenp );

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
  { 4, fvm_proc_register },
  { 5, fvm_proc_unregister },
  { 6, fvm_proc_run },
  { 7, fvm_proc_readvar },
  { 8, fvm_proc_writevar },
  { 9, fvm_proc_clusterrun },
  
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
  uint32_t progid;
  uint32_t procid;
};

static void fvm_iter_cb( struct rpc_iterator *it ) {
  struct fvm_iterator *fvm = (struct fvm_iterator *)it;
  struct fvm_s state;
  int sts, reslen, period;
  struct xdr_s xdr;
  char *resbufp;
  
  sts = fvm_state_init( &state, fvm->progid, fvm->procid );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "Failed to init state %u %u", fvm->progid, fvm->procid );
    return;
  }

  sts = fvm_run( &state, 0 );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "Failed to run %u %u", fvm->progid, fvm->procid );
  }

  reslen = fvm_get_res( &state, &resbufp );
  if( reslen > 0 ) {
    xdr_init( &xdr, (uint8_t *)resbufp, reslen );
    sts = xdr_decode_uint32( &xdr, (uint32_t *)&period );
    if( !sts ) {
      if( period > 0 ) {
	/* positive means update the period */
	fvm->iter.period = period;
	fvm->iter.timeout = 0;
      } else if( period < 0 ) {
	/* returning negative means rerun immediately */
	fvm->iter.timeout = 0;
      } else if( period == 0 ) {
	/* if the iterator routine returns 0 then it means unregister itself */
	rpc_iterator_unregister( &fvm->iter );
	free( fvm );
      }
    }
  }
  
}


struct fvm_event_desc {
  struct fvm_event_desc *next;
  uint32_t eventid;
  uint32_t progid;
  uint32_t procid;
};
static struct fvm_event_desc *fvm_event_descs;

static void fvm_event_cb( uint32_t eventid, struct xdr_s *args, void *cxt ) {
  /* find any prog/procs that were registered on this category. If so then invoke that procedure */
  struct fvm_event_desc *d;
  int sts;
  struct fvm_s state;
  struct xdr_s argbufs[2];
  uint8_t eventxdrbuf[4];
  
  xdr_init( &argbufs[0], eventxdrbuf, 4 );
  sts = xdr_encode_uint32( &argbufs[0], eventid );
  if( args ) {
    xdr_init( &argbufs[1], args->buf, args->offset );
    argbufs[1].offset = args->offset;
  } else xdr_init( &argbufs[1], NULL, 0 );
  
  d = fvm_event_descs;
  while( d ) {
    if( d->eventid == 0 || d->eventid == eventid ) {
      sts = fvm_state_init( &state, d->progid, d->procid );
      if( !sts ) {
	fvm_set_args2( &state, argbufs, 2 );	
	fvm_run( &state, -1 );
      }
    }
    d = d->next;
  }
  
}

#if 0
typedef enum {
    FVM_CMDTYPE_RUN = 0,
    FVM_CMDTYPE_WU32 = 1,
    FVM_CMDTYPE_WU64 = 2,
    FVM_CMDTYPE_WSTR = 3,
} fvm_cmdtype_t;

static void fvm_raft_command( struct raft_app *app, struct raft_cluster *cl, uint64_t seq, char *buf, int len ) {
  int sts;
  uint32_t progid, procid;
  char *argbuf = NULL;
  int arglen;
  fvm_cmdtype_t cmdtype;
  struct xdr_s args;
  struct fvm_s state;
  uint32_t u32;
  uint64_t u64;
  char str[1024];
  struct fvm_module *m;

  xdr_init( &args, (uint8_t *)buf, len );
  sts = xdr_decode_uint32( &args, &progid );
  if( !sts ) sts = xdr_decode_uint32( &args, &procid );
  if( !sts ) sts = xdr_decode_uint32( &args, &cmdtype );
  if( !sts ) {
    switch( cmdtype ) {
    case FVM_CMDTYPE_RUN:
      sts = xdr_decode_opaque_ref( &args, (uint8_t **)&argbuf, &arglen );
      break;
    case FVM_CMDTYPE_WU32:
      sts = xdr_decode_uint32( &args, &u32 );
      break;
    case FVM_CMDTYPE_WU64:
      sts = xdr_decode_uint64( &args, &u64 );
      break;
    case FVM_CMDTYPE_WSTR:
      sts = xdr_decode_string( &args, str, sizeof(str) );
      break;    
    default:
      break;
    }
  }
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "Xdr error decoding command args" );
    return;
  }

  m = fvm_module_by_progid( progid );
  if( !m ) {
    fvm_log( LOG_LVL_ERROR, "Module not found" );
    return;
  }

  switch( cmdtype ) {
  case FVM_CMDTYPE_RUN:    
    fvm_log( LOG_LVL_TRACE, "fvm_raft_command run clid=%"PRIu64" seq=%"PRIu64" progid=%u procid=%u arglen=%u", cl->clid, seq, progid, procid, arglen );
  
    sts = fvm_state_init( &state, progid, procid );
    if( sts ) {
      fvm_log( LOG_LVL_ERROR, "Failed to init state" );
      return;
    }
    
    fvm_set_args( &state, argbuf, arglen );
    sts = fvm_run( &state, 0 );
    if( sts ) {
      fvm_log( LOG_LVL_ERROR, "fvm_run progid=%u procid=%u failed", progid, procid );
    }
    break;
  case FVM_CMDTYPE_WU32:
    fvm_log( LOG_LVL_TRACE, "fvm_raft_command writeu32 clid=%"PRIu64" seq=%"PRIu64" progid=%u procid=%u arg=%u", cl->clid, seq, progid, procid, u32);    
    fvm_write_uint32( m, procid, u32 );
    break;
  case FVM_CMDTYPE_WU64:
    fvm_log( LOG_LVL_TRACE, "fvm_raft_command write u64 clid=%"PRIu64" seq=%"PRIu64" progid=%u procid=%u arg=%"PRIu64"", cl->clid, seq, progid, procid, u64 );    
    fvm_write_uint64( m, procid, u64 );
    break;
  case FVM_CMDTYPE_WSTR:
    fvm_log( LOG_LVL_TRACE, "fvm_raft_command write str clid=%"PRIu64" seq=%"PRIu64" progid=%u procid=%u arg=%s", cl->clid, seq, progid, procid, str ); 
    fvm_write_string( m, procid, str );
    break;
  }
  
}

static void fvm_raft_snapshot( struct raft_app *app, struct raft_cluster *cl, uint64_t term, uint64_t seq ) {
  struct fvm_module *m;
  
  fvm_log( LOG_LVL_INFO, "fvm save snapshot" );

  /* lookup module by cluster id */
  m = fvm_module_by_clid( cl->clid );
  if( !m ) return;
  
  /* save module state */
  raft_snapshot_save( cl->clid, term, seq, 0, (char *)m->data, m->header.datasize );
  raft_snapshot_save( cl->clid, term, seq, m->header.datasize, NULL, 0 );
}

static void fvm_raft_snapload( struct raft_app *app, struct raft_cluster *cl, char *buf, int len ) {
  struct fvm_module *m;
  
  fvm_log( LOG_LVL_INFO, "fvm load snapshot" );

  /* lookup module by clusterid */
  m = fvm_module_by_clid( cl->clid );
  if( !m ) return;
  
  /* set module state from buffer */
  if( m->header.datasize != len ) {
    fvm_log( LOG_LVL_ERROR, "Bad datasize" );
    return;
  }
  
  memcpy( m->data, buf, len );
}

static struct raft_app fvm_app = {
    NULL,
    RAFT_RPC_PROG,
    fvm_raft_command,
    fvm_raft_snapshot,
    fvm_raft_snapload,
};
#endif

void fvm_rpc_register( void ) {
  int sts;
  uint32_t nsteps;
  uint64_t key, clid;
  struct freg_entry entry;
  char path[256];
  char name[FVM_MAX_NAME];
  struct fvm_module *m;
  uint32_t progid, procid, flags;
  struct fvm_s state;
  
  /* load all these modules on startup */
  sts = freg_subkey( NULL, 0, "/fju/fvm/modules", FREG_CREATE, &key );
  if( !sts ) {
    sts = freg_next( NULL, key, 0, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {

	clid = 0;
	sts = freg_get_by_name( NULL, entry.id, "path", FREG_TYPE_STRING, path, sizeof(path), NULL );
	if( !sts ) {
	  fvm_log( LOG_LVL_INFO, "FVM loading module %s", path );
	  sts = fvm_module_load_file( path, &progid );
 	  if( sts ) fvm_log( LOG_LVL_ERROR, "Failed to load module %s", path );
	}
	
	sts = freg_get_by_name( NULL, entry.id, "cluster", FREG_TYPE_UINT64, (char *)&clid, sizeof(clid), NULL );
	if( !sts ) {
	  m = fvm_module_by_progid( progid );
	  if( m ) m->clusterid = clid;
	  else fvm_log( LOG_LVL_ERROR, "Failed to get module %u", progid );
	}

	sts = freg_get_by_name( NULL, entry.id, "flags", FREG_TYPE_UINT32, (char *)&flags, sizeof(flags), NULL );
	if( !sts ) {
	  sts = fvm_module_set_flags( progid, flags, 0xffffffff );
	  if( sts ) fvm_log( LOG_LVL_ERROR, "Failed to get module %u", progid );
	}

      }
      sts = freg_next( NULL, key, entry.id, &entry );
    }
  }

  /* register all these modules as rpc programs on startup */
  sts = freg_subkey( NULL, 0, "/fju/fvm/programs", FREG_CREATE, &key );
  if( !sts ) {
    sts = freg_next( NULL, key, 0, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_STRING ) {
	sts = freg_get( NULL, entry.id, NULL, name, sizeof(name), NULL );
	if( !sts ) {
	  fvm_log( LOG_LVL_INFO, "FVM registering program %s", name );
	  fvm_register_program( fvm_progid_by_name( name ) );
	}
      }
	
      sts = freg_next( NULL, key, entry.id, &entry );
    }    
  }

  /* register service routines */
  sts = freg_subkey( NULL, 0, "/fju/fvm/service", FREG_CREATE, &key );
  if( !sts ) {
    sts = freg_next( NULL, key, 0, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
	char mname[FVM_MAX_NAME], pname[FVM_MAX_NAME];
	uint32_t period = 1000;
	
	sts = freg_get_by_name( NULL, entry.id, "module", FREG_TYPE_STRING, mname, sizeof(mname), NULL );
	if( !sts ) sts = freg_get_by_name( NULL, entry.id, "proc", FREG_TYPE_STRING, pname, sizeof(pname), NULL );
	if( !sts ) {
	  sts = freg_get_by_name( NULL, entry.id, "period", FREG_TYPE_UINT32, (char *)&period, sizeof(period), NULL );
	  if( sts ) {
	    period = 1000;
	    sts = 0;
	  }
	}
	
	
	if( !sts ) {
	  struct fvm_iterator *iter;
	  iter = malloc( sizeof(*iter) + 8 );
	  memset( iter, 0, sizeof(*iter) );
	  iter->iter.period = period;
	  iter->iter.cb = fvm_iter_cb;
	  iter->progid = fvm_progid_by_name( mname );
	  iter->procid = fvm_procid_by_name( iter->progid, pname );
	  if( iter->progid == -1 || iter->procid == -1 ) {
	    fvm_log( LOG_LVL_ERROR, "Failed to register iterator for %s %s", mname, pname );
	    free( iter );
	  } else {
	    fvm_log( LOG_LVL_INFO, "FVM registering iterator %s %s", mname, pname );
	    rpc_iterator_register( &iter->iter );
	  }
	}
      }
	
      sts = freg_next( NULL, key, entry.id, &entry );
    }    
  }

  sts = freg_subkey( NULL, 0, "/fju/fvm/events", FREG_CREATE, &key );
  if( !sts ) {
    sts = freg_next( NULL, key, 0, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
	uint32_t eventid, progid, procid;
	struct fvm_event_desc *d;

	eventid = 0;
	procid = 0;
	progid = fvm_progid_by_name( entry.name );
	sts = freg_get_by_name( NULL, entry.id, "eventid", FREG_TYPE_UINT32, (char *)&eventid, sizeof(eventid), NULL );
	sts = freg_get_by_name( NULL, entry.id, "procid", FREG_TYPE_UINT32, (char *)&procid, sizeof(procid), NULL );	

	d = malloc( sizeof(*d) );
	d->eventid = eventid;
	d->progid = progid;
	d->procid = procid;
	d->next = fvm_event_descs;	
	fvm_event_descs = d;
	
      }
      
      sts = freg_next( NULL, key, entry.id, &entry );
    }    
  }


  
  /* set max steps to limit runaway programs blocking rpcd */
  sts = freg_get_by_name( NULL, 0, "/fju/fvm/maxsteps", FREG_TYPE_UINT32, (char *)&nsteps, sizeof(nsteps), NULL );
  if( !sts ) {
    fvm_log( LOG_LVL_INFO, "Setting max steps to %u", nsteps );
    fvm_max_steps( nsteps );
  }
  sts = freg_get_by_name( NULL, 0, "/fju/fvm/timeout", FREG_TYPE_UINT32, (char *)&nsteps, sizeof(nsteps), NULL );
  if( !sts ) {
    fvm_log( LOG_LVL_INFO, "Setting timeout to %u", nsteps );
    fvm_default_timeout( nsteps );
  }


  /* run init functions on startup */
  sts = freg_subkey( NULL, 0, "/fju/fvm/modules", FREG_CREATE, &key );
  if( !sts ) {
    sts = freg_next( NULL, key, 0, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
	if( !sts ) sts = freg_get_by_name( NULL, entry.id, "init", FREG_TYPE_STRING, name, sizeof(name), NULL );
	if( !sts ) {
	  fvm_log( LOG_LVL_INFO, "FVM initializing module %s proc %s", entry.name, name );
	  progid = fvm_progid_by_name( entry.name );
	  procid = fvm_procid_by_name( progid, name );
	  sts = fvm_state_init( &state, progid, procid );
	  if( sts ) {
	    fvm_log( LOG_LVL_ERROR, "FVM initializing proc failed" );
	  } else {
	    fvm_run( &state, 0 );
	  }
	}


      }
      sts = freg_next( NULL, key, entry.id, &entry );
    }
  }

  rpc_program_register( &fvm_prog );

  rpcd_event_subscribe( 0, fvm_event_cb, NULL );

  fvm_cluster_register();
}



