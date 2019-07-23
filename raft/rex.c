
#include "rex.h"

#include <rpc.h>
#include <hrauth.h>
#include <hostreg.h>
#include "raft.h"

#define REX_MAX_BUF 32

static char rex_buf[REX_MAX_BUF];

static int rex_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int rex_proc_read( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t clid;
  struct raft_cluster cl;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_opaque( &inc->xdr, (uint8_t *)rex_buf, REX_MAX_BUF );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}


static void rex_call_ping_cb( struct xdr_s *xdr, void *cxt ) {
  if( !xdr ) {
    rpc_log( RPC_LOG_DEBUG, "rex_call_ping_cb timeout" );
  }
  
  return;
}

static void rex_call_ping( uint64_t clid, uint64_t hostid, uint64_t seq ) {
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  uint8_t xdr_buf[256];
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = REX_RPC_PROG;
  hcall.vers = REX_RPC_VERS;
  hcall.proc = 3;
  hcall.donecb = rex_call_ping_cb;
  hcall.cxt = NULL;
  hcall.timeout = 500;
  hcall.service = HRAUTH_SERVICE_PRIV;
  xdr_init( &xdr, xdr_buf, sizeof(xdr_buf) );
  xdr_encode_uint64( &xdr, clid );
  xdr_encode_uint64( &xdr, hostreg_localid() );
  xdr_encode_uint64( &xdr, seq );
  xdr_encode_opaque( &xdr, (uint8_t *)rex_buf, REX_MAX_BUF );
  sts = hrauth_call_udp( &hcall, &xdr );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "raft_call_ping: hrauth_call failed" );
  }

}

static void rex_send_pings( struct raft_cluster *cl ) {
  int i, n;
  struct raft_member member[32];
  
  n = raft_member_list( cl->id, member, 32 );
  for( i = 0; i < n; i++ ) {
    if( !(member[i].flags & RAFT_MEMBER_LOCAL) ) {      
      rex_call_ping( cl->id, member[i].hostid, cl->seq );
    }
  }

}

static int rex_proc_write( struct rpc_inc *inc ) {
  int handle, sts, len;
  uint64_t clid;
  struct raft_cluster cl;
  uint8_t buf[REX_MAX_BUF];
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  len = REX_MAX_BUF;
  if( !sts ) sts = xdr_decode_opaque( &inc->xdr, buf, &len );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
	      
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, 0 );
    goto done;
  }

  if( cl.state != RAFT_STATE_LEADER ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, cl.leaderid );
    goto done;
  }

  if( len > REX_MAX_BUF ) len = REX_MAX_BUF;
  memcpy( rex_buf, buf, len );
  xdr_encode_boolean( &inc->xdr, 1 );
  xdr_encode_uint64( &inc->xdr, cl.leaderid );
  
 done:

  rpc_complete_accept_reply( inc, handle );

  rex_send_pings( &cl );
  
  return 0;
}

static int rex_proc_ping( struct rpc_inc *inc ) {
  int handle, sts, len;
  uint64_t clid, seq, leaderid;
  struct raft_cluster cl;
  uint8_t buf[REX_MAX_BUF];
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &leaderid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &seq );
  len = REX_MAX_BUF;
  if( !sts ) sts = xdr_decode_opaque( &inc->xdr, buf, &len );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
	      
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, 0 );
    goto done;
  }

  if( cl.state == RAFT_STATE_LEADER ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, cl.leaderid );
    goto done;
  }

  if( cl.leaderid != leaderid ) {
    /* bad leader */
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, cl.leaderid );    
  }
  
  if( seq != cl.seq ) {
    /* bad seq */
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, cl.leaderid );
  }

  /* all good, write state */
  if( len > REX_MAX_BUF ) len = REX_MAX_BUF;
  memcpy( rex_buf, buf, len );
  xdr_encode_boolean( &inc->xdr, 1 );
  xdr_encode_uint64( &inc->xdr, cl.leaderid );
  
 done:

  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct rpc_proc rex_procs[] = {
  { 0, rex_proc_null },
  { 1, rex_proc_read },
  { 2, rex_proc_write },
  { 3, rex_proc_ping },
  { 0, NULL }
};

static struct rpc_version rex_vers = {
  NULL, REX_RPC_VERS, rex_procs
};

static struct rpc_program rex_prog = {
  NULL, REX_RPC_PROG, &rex_vers
};


static void rex_notify( struct raft_cluster *cl, void *cxt ) {
  
  if( cl->state == RAFT_STATE_LEADER ) {
    rex_send_pings( cl );
  }
  
}

static struct raft_notify_context rex_notify_cxt = {
  NULL,
  rex_notify,
  NULL
};

void rex_register( void ) {
  rpc_program_register( &rex_prog );

  raft_notify_register( &rex_notify_cxt );
  
  //rpc_iterator_register( &rex_iter );
}
