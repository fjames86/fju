
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <WinSock2.h>
#include <Windows.h>
#else
#include <arpa/inet.h>
#endif

#include "fvm-private.h"
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/raft.h>
#include <fju/hrauth.h>
#include <fju/hostreg.h>

#include <fju/programs.h>
#include <fju/log.h>
#include <fju/freg.h>


/* rpc interface */

/*
 *Load module
 * Unload module
 * List modules 
 * 
 */

/* want to be able to register an rpc program which is dynamically generated (normally it is always a static sturct) */
static int fvm_call_ping( struct raft_cluster *cl, uint64_t hostid, uint32_t progid, char *data, int datasize );
static void fvm_send_pings( struct raft_cluster *cl, struct fvm_module *module );
static int fvm_call_write( struct raft_cluster *cl, uint64_t hostid, struct fvm_module *module, uint32_t retry, int timeout );

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
  
  progid = inc->msg.u.call.prog;
  m = fvm_module_by_progid( progid );
  if( !m ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  procid = inc->msg.u.call.proc;
  if( procid >= m->header.symcount ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  if( m->flags & FVM_MODULE_AUDIT ) fvm_audit_write( progid, procid, (char *)(inc->xdr.buf + inc->xdr.offset), inc->xdr.count - inc->xdr.offset );
  
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
    /* R0 contains length, R1 contains address of buffer */
    count = ntohl( state.reg[FVM_REG_R0] );
    if( count > 0 && count < FVM_MAX_STACK ) {
      char *p = fvm_getaddr( &state, ntohl( state.reg[FVM_REG_R1] ) );
      if( p ) {
	sts = xdr_encode_fixed( &inc->xdr, (uint8_t *)p, count );
	if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
      }
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
    xdr_encode_uint64( &inc->xdr, minfo[i].clusterid );
    xdr_encode_uint32( &inc->xdr, minfo[i].flags );
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
  int lenp, sts, registerp;
  uint32_t progid;
  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &registerp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_module_load_buffer( bufp, lenp, &progid );
  if( sts ) {
    fvm_module_unload( progid );
    sts = fvm_module_load_buffer( bufp, lenp, &progid );
  }

  if( registerp ) {
    fvm_unregister_program( progid );
    fvm_register_program( progid );
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

static int fvm_proc_ping( struct rpc_inc *inc ) {
  /* follower receives updated data from leader */
  int handle, sts, len;
  uint64_t clid, termseq, leaderid, stateseq, stateterm;
  struct raft_cluster cl;
  uint8_t *datap;
  struct fvm_module *m;
  uint32_t progid;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &leaderid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &termseq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &stateseq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &stateterm );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &progid );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  sts = raft_cluster_by_id( clid, &cl );
  if( cl.state == RAFT_STATE_LEADER ) {
    /* we are leader, do not accept */
    sts = -1;
    goto done;
  }
  if( cl.leaderid != leaderid ) {
    /* bad leader */
    sts = -1;
    goto done;
  }  
  if( termseq != cl.termseq ) {
    /* bad term seq */
    sts = -1;
    goto done;
  }
  if( stateseq < cl.stateseq ) {
    /* old state, ignore */
    sts = -1;
    goto done;
  }

  /* all good, accept */
  
  m = fvm_module_by_progid( progid );
  if( !m ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  len = m->header.datasize;  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&datap, &len );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  if( len == m->header.datasize ) {
    memcpy( m->data, datap, len );
    sts = 0;
  } else {
    sts = -1;
  }

  
 done:
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_write( struct rpc_inc *inc ) {
  /* leader receives upated data from follower */
  int handle, sts, len;
  uint64_t clid;
  struct raft_cluster cl;
  char *datap;  
  uint32_t progid;
  struct fvm_module *m;
  int i, n;
  struct raft_member member[32];
    
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &progid );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&datap, &len );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  m = fvm_module_by_progid( progid );
  if( !m ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  if( cl.state != RAFT_STATE_LEADER ) {
    // not leader, don't care 
    return 1;
  }

  // update cluster 
  cl.stateterm = cl.termseq;
  cl.stateseq++;
  cl.commitseq = cl.stateseq;
  fvm_log( LOG_LVL_TRACE, "setting cluster stateseq=%"PRIu64"", cl.stateseq );
  raft_cluster_set( &cl );

  if( len == m->header.datasize ) {
    memcpy( m->data, datap, len );
  }
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );

  // send pings to all followers
  n = raft_member_list( cl.id, member, 32 );
  for( i = 0; i < n; i++ ) {
    fvm_call_ping( &cl, member[i].hostid, progid, (char *)m->data, (int)m->header.datasize );
  }
  
  return 0;
}

static int fvm_proc_cluster( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM_MAX_NAME];
  struct fvm_module *m;
  uint64_t clid;
  
  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  m = fvm_module_by_name( name );
  if( m ) m->clusterid = clid;
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, m ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_run( struct rpc_inc *inc ) {
  int handle, sts;
  uint32_t progid, procid;
  char *bufp;
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

  if( state.module->flags & FVM_MODULE_AUDIT ) fvm_audit_write( progid, procid, bufp, lenp );
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


static struct rpc_proc fvm_procs[] = {
  { 0, fvm_proc_null },
  { 1, fvm_proc_list },
  { 2, fvm_proc_load },
  { 3, fvm_proc_unload },
  { 4, fvm_proc_register },
  { 5, fvm_proc_unregister },
  { 6, fvm_proc_ping },
  { 7, fvm_proc_write },
  { 8, fvm_proc_cluster },
  { 9, fvm_proc_run },
  { 10, fvm_proc_readvar },
  { 11, fvm_proc_writevar },
  
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

  if( state.module->flags & FVM_MODULE_AUDIT ) fvm_audit_write( fvm->progid, fvm->procid, NULL, 0 );
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


static void fvm_cluster_notify( raft_notify_t evt, struct raft_cluster *cl, void *cxt, void *reserved ) {
  struct fvm_module *m;
		  
  switch( evt ) {
  case RAFT_NOTIFY_LEADER:
  case RAFT_NOTIFY_SEND_PING:
    /* resend data whenever we become leader or are sending raft ping messages */
    m = fvm_get_modules();
    while( m ) {
      if( m->clusterid == cl->id ) {
	fvm_send_pings( cl, m );
      }
      m = m->next;
    }
    break;
  default:
    break;
  }
  
}

static struct raft_notify_context fvm_notify_cxt = {
  NULL,
  fvm_cluster_notify,
  NULL
};

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
	  m = fvm_module_by_progid( progid );
	  if( m ) m->flags = flags;
	  else fvm_log( LOG_LVL_ERROR, "Failed to get module %u", progid );
	}
	
      }
      sts = freg_next( NULL, key, entry.id, &entry );
    }
  }

  /* TODO: replay audit logs from cluster leader? */

  /* clear audit log */
  fvm_audit_reset();  

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
	sts = freg_get_by_name( NULL, entry.id, "period", FREG_TYPE_UINT32, (char *)&period, sizeof(period), NULL );
	if( sts ) {
	  period = 1000;
	  sts = 0;
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
  
  /* set max steps to limit runaway programs blocking rpcd */
  sts = freg_get_by_name( NULL, 0, "/fju/fvm/maxsteps", FREG_TYPE_UINT32, (char *)&nsteps, sizeof(nsteps), NULL );
  if( !sts ) {
    fvm_log( LOG_LVL_INFO, "Setting max steps to %u", nsteps );
    fvm_max_steps( nsteps );
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
	    if( state.module->flags & FVM_MODULE_AUDIT ) fvm_audit_write( progid, procid, NULL, 0 );
	    fvm_run( &state, 0 );
	  }
	}


      }
      sts = freg_next( NULL, key, entry.id, &entry );
    }
  }

  rpc_program_register( &fvm_prog );

  raft_notify_register( &fvm_notify_cxt );  
}


struct ping_cxt {
  uint64_t clid;
  uint64_t hostid;
  uint64_t stateseq;
};

static void fvm_call_ping_donecb( struct xdr_s *xdr, void *cxt ) {
  struct ping_cxt *p = (struct ping_cxt *)cxt;
  int sts, b;
  struct raft_cluster cl;
  struct raft_member member;
  
  if( !xdr ) {
    fvm_log( LOG_LVL_TRACE, "fvm ping timeout" );
    goto done;
  }

  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) {
    goto done;
  }

  if( b ) {
    /* ping accepted by follower */
    sts = raft_cluster_by_id( p->clid, &cl );
    if( sts ) goto done;
    if( cl.state != RAFT_STATE_LEADER ) goto done;
    sts = raft_member_by_hostid( p->clid, p->hostid, &member );
    if( sts ) goto done;
    member.nextseq = cl.stateseq;
    member.stateseq = p->stateseq;
    fvm_log( LOG_LVL_TRACE, "Setting member stateseq %"PRIu64"", p->stateseq );
    raft_member_set( &member );
  }

 done:
  free( p );
}

static int fvm_call_ping( struct raft_cluster *cl, uint64_t hostid, uint32_t progid, char *data, int datasize ) {
  /* send data to followers */
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  struct rpc_conn *tmpconn;
  struct ping_cxt *p;

  fvm_log( LOG_LVL_DEBUG, "fvm_call_ping clid=%"PRIx64" Progid=%u Hostid=%"PRIx64" datasize=%u", cl->id, progid, hostid, datasize );
  
  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) return -1;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FVM_RPC_PROG;
  hcall.vers = FVM_RPC_VERS;
  hcall.proc = 6;
  hcall.timeout = 500;
  hcall.service = HRAUTH_SERVICE_PRIV;
  hcall.donecb = fvm_call_ping_donecb;
  p = malloc( sizeof(*p) );
  p->clid = cl->id;
  p->hostid = hostid;
  p->stateseq = cl->stateseq;
  hcall.cxt = p;
  
  xdr_init( &xdr, tmpconn->buf, tmpconn->count );
  xdr_encode_uint64( &xdr, cl->id );
  xdr_encode_uint64( &xdr, hostreg_localid() );
  xdr_encode_uint64( &xdr, cl->termseq );
  xdr_encode_uint64( &xdr, cl->stateseq );
  xdr_encode_uint64( &xdr, cl->stateterm );
  xdr_encode_uint32( &xdr, progid );
  xdr_encode_opaque( &xdr, (uint8_t *)data, datasize );
  
  sts = hrauth_call_udp_async( &hcall, &xdr, NULL );
  rpc_conn_release( tmpconn );
  
  return sts;
}

struct write_cxt {
  uint64_t clid;
  uint32_t progid;
  uint32_t retry;
  uint32_t timeout;
};

static void fvm_call_write_donecb( struct xdr_s *xdr, void *cxt ) {
  struct write_cxt *w = (struct write_cxt *)cxt;
  struct raft_cluster cl;
  struct fvm_module *m;
  int sts;
  int retry;
  
  if( !xdr ) {    
    fvm_log( LOG_LVL_ERROR, "write timeout, retrying" );
    sts = raft_cluster_by_id( w->clid, &cl );
    m = fvm_module_by_progid( w->progid );
    retry = (int)w->retry;
    retry--;
    if( m && sts == 0 && retry > 0 ) {
      fvm_call_write( &cl, cl.leaderid, m, retry, (w->timeout * 3) / 2 );
    }
  }

  free( w );
  
  return;
}

static int fvm_call_write( struct raft_cluster *cl, uint64_t hostid, struct fvm_module *module, uint32_t retry, int timeout ) {
  /* send data segment to leader */
  /* send data to followers */
  int sts;
  struct xdr_s xdr;
  struct rpc_conn *tmpconn;
  struct hrauth_call hcall;
  struct write_cxt *w;

  fvm_log( LOG_LVL_DEBUG, "fvm_call_write clid=%"PRIx64" hostid=%"PRIx64" datasize=%u", cl->id, hostid, module->header.datasize );
  
  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) return -1;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FVM_RPC_PROG;
  hcall.vers = FVM_RPC_VERS;
  hcall.proc = 7;
  hcall.timeout = timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  hcall.donecb = fvm_call_write_donecb;
  w = malloc( sizeof(*w) );
  w->clid = cl->id;
  w->progid = module->header.progid;
  w->retry = retry;
  hcall.cxt = w;
  
  xdr_init( &xdr, tmpconn->buf, tmpconn->count );
  xdr_encode_uint64( &xdr, cl->id );
  xdr_encode_uint32( &xdr, module->header.progid );
  xdr_encode_opaque( &xdr, (uint8_t *)module->data, module->header.datasize );
  
  sts = hrauth_call_udp_async( &hcall, &xdr, NULL );
  rpc_conn_release( tmpconn );
  
  return sts;
}

static void fvm_send_pings( struct raft_cluster *cl, struct fvm_module *module ) {
  /* send ping rpcs to all other members */
  int i, n;
  struct raft_member member[32];
  
  n = raft_member_list( cl->id, member, 32 );
  for( i = 0; i < n; i++ ) {
    fvm_log( LOG_LVL_TRACE, "Comparing cluster stateseq %"PRIu64" to member stateseq %"PRIu64"", cl->stateseq, member[i].stateseq );
    
    if( member[i].stateseq < cl->stateseq ) {
      fvm_log( LOG_LVL_DEBUG, "Sending updated state module=%s clid=%"PRIx64" member=%"PRIx64" cl.stateseq=%"PRIu64" member.stateseq="PRIu64"",
	       module->header.name, cl->id, member[i].hostid, cl->stateseq, member[i].stateseq );

      fvm_call_ping( cl, member[i].hostid, module->header.progid, (char *)module->data, (int)module->header.datasize );
    } 
  }
}

int fvm_cluster_update( struct fvm_module *module ) {
  int sts;
  struct raft_cluster cl;

  fvm_log( LOG_LVL_INFO, "Updating cluster for program %s %u:%u cluster %"PRIx64"",
	   module->header.name,
	   module->header.progid,
	   module->header.versid,
	   module->clusterid );
  
  sts = raft_cluster_by_id( module->clusterid, &cl );
  if( sts ) return sts;

  if( cl.state == RAFT_STATE_LEADER ) {
    /* send ping rpcs to all other members */
    cl.stateterm = cl.termseq;
    cl.stateseq++;
    cl.commitseq = cl.stateseq;
    raft_cluster_set( &cl );
    
    fvm_send_pings( &cl, module );
  } else {
    /* send write to leader */
    if( !cl.leaderid ) return -1;
    sts = fvm_call_write( &cl, cl.leaderid, module, 3, 500 );
    if( sts ) return sts;
  }
  
  return 0;
}

