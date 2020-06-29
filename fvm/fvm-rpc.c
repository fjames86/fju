
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
  uint32_t prog, proc;
  struct fvm_s state;
  int sts, handle, arglength, count;
  
  prog = inc->msg.u.call.prog;
  proc = inc->msg.u.call.proc;

  fvm_log( LOG_LVL_INFO, "fvm_rpc_log progid=%u proc=%u", prog, proc );
  
  /* initialize state */
  sts = fvm_state_init( &state, prog, proc );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  /* copy args onto fvm stack and set r0 to length */
  arglength = inc->xdr.count - inc->xdr.offset;
  fvm_log( LOG_LVL_INFO, "fvm_rpc_log offet=%u count=%u arglength = %u", inc->xdr.offset, inc->xdr.count, arglength );
  
  memcpy( &state.stack, inc->xdr.buf + inc->xdr.offset, arglength );
  state.reg[FVM_REG_R0] = htonl( arglength );
  sts = fvm_run( &state, fvm_max_steps( 0 ) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  fvm_log( LOG_LVL_INFO, "success reply length %d", ntohl( state.reg[FVM_REG_R0] ) );
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
  
  return 0;
}

int fvm_register_program( uint32_t progid ) {
  struct fvm_module *m;
  struct rpc_program *pg;
  int i, nprocs;

  m = fvm_module_by_progid( progid );
  if( !m ) return -1;

  /* collect symbols pointing to text segment, probably functions? */
  nprocs = 0;
  for( i = 0; i < m->header.symcount; i++ ) {
    if( m->symbols[i].addr >= FVM_ADDR_TEXT && m->symbols[i].addr <= (FVM_ADDR_TEXT + FVM_MAX_TEXT) ) {
      nprocs++;
    }
  }
  
  pg = alloc_program( m->header.progid, m->header.versid, nprocs, fvm_rpc_proc );
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
  int sts;
  
  sts = fvm_state_init( &state, fvm->progid, fvm->procid );
  if( sts ) {
    fvm_log( LOG_LVL_INFO, "Failed to init state %u %u", fvm->progid, fvm->procid );
    return;
  }
  
  sts = fvm_run( &state, -1 );
  if( sts ) {
    fvm_log( LOG_LVL_INFO, "Failed to run %u %u", fvm->progid, fvm->procid );
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
  uint64_t key;
  struct freg_entry entry;
  char path[256];
  char name[FVM_MAX_NAME];
  uint64_t clid;
  struct fvm_module *m;
  uint32_t progid;
  
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

  
  rpc_program_register( &fvm_prog );

  raft_notify_register( &fvm_notify_cxt );  
}


static int fvm_call_ping( struct raft_cluster *cl, uint64_t hostid, uint32_t progid, char *data, int datasize ) {
  /* send data to followers */
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  struct rpc_conn *tmpconn;

  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) return -1;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FVM_RPC_PROG;
  hcall.vers = FVM_RPC_VERS;
  hcall.proc = 6;
  hcall.timeout = 500;
  hcall.service = HRAUTH_SERVICE_PRIV;
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

static int fvm_call_write( struct raft_cluster *cl, uint64_t hostid, struct fvm_s *state ) {
  /* send data segment to leader */
  /* send data to followers */
  int sts;
  struct xdr_s xdr;
  struct rpc_conn *tmpconn;
  struct hrauth_call hcall;
  
  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) return -1;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FVM_RPC_PROG;
  hcall.vers = FVM_RPC_VERS;
  hcall.proc = 7;
  hcall.timeout = 500;
  hcall.service = HRAUTH_SERVICE_PRIV;
  xdr_init( &xdr, tmpconn->buf, tmpconn->count );
  xdr_encode_uint64( &xdr, cl->id );
  xdr_encode_uint32( &xdr, state->module->header.progid );
  xdr_encode_opaque( &xdr, (uint8_t *)state->data, state->datasize );
  
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
    fvm_call_ping( cl, member[i].hostid, module->header.progid, (char *)module->data, (int)module->header.datasize );
  }
}

int fvm_cluster_update( struct fvm_s *state ) {
  int sts;
  struct raft_cluster cl;

  fvm_log( LOG_LVL_INFO, "Updating cluster for program %s %u:%u cluster %"PRIx64"",
	   state->module->header.name,
	   state->module->header.progid,
	   state->module->header.versid,
	   state->module->clusterid );
  
  sts = raft_cluster_by_id( state->module->clusterid, &cl );
  if( sts ) return sts;

  if( cl.state == RAFT_STATE_LEADER ) {
    /* send ping rpcs to all other members */
    fvm_send_pings( &cl, state->module );
  } else {
    /* send write to leader */
    if( !cl.leaderid ) return -1;
    sts = fvm_call_write( &cl, cl.leaderid, state );
    if( sts ) return sts;
  }
  
  return 0;
}

