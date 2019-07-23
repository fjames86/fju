
#include <rpc.h>
#include <sec.h>
#include <inttypes.h>
#include <hrauth.h>
#include <hostreg.h>

#include "raft.h"

static struct {  
  struct raft_prop prop;
  uint64_t nextprop;
} glob;

static void raft_iter_cb( struct rpc_iterator *iter );
static uint32_t term_timeout( void );
static uint32_t elec_timeout( void );
static void raft_transition_follower( struct raft_cluster *cl );
static void raft_transition_candidate( struct raft_cluster *cl );
static void raft_transition_leader( struct raft_cluster *cl );
static void raft_call_ping( uint64_t clid, uint64_t hostid, uint64_t seq );
static void raft_call_vote( uint64_t clid, uint64_t hostid, uint64_t seq );

static struct rpc_iterator raft_iter = {
    NULL,
    0,
    1000,
    raft_iter_cb,
    NULL
};
static void raft_iter_set_timeout( uint64_t when ) {
  if( when < raft_iter.timeout ) raft_iter.timeout = when;
}

static void raft_transition_follower( struct raft_cluster *cl ) {
  uint64_t now;
  uint32_t timeo;
  
  cl->state = RAFT_STATE_FOLLOWER;

  now = rpc_now();
  timeo = term_timeout();
  cl->timeout = now + timeo;
  cl->voteid = 0;
  cl->votes = 0;
  raft_cluster_set( cl );

  raft_member_clear_voted( cl->id );

  raft_iter_set_timeout( cl->timeout );
}


struct raft_ping_cxt {
  uint64_t clid;
  uint64_t hostid;
  uint64_t seq;
};

static void raft_call_ping_cb( struct xdr_s *xdr, void *cxt ) {
  int sts, b;
  struct raft_ping_cxt *pcxt = (struct raft_ping_cxt *)cxt;
  struct raft_member member;
  uint64_t seq;
  struct raft_cluster cl;

  //  rpc_log( RPC_LOG_DEBUG, "raft_call_ping_cb clid=%"PRIx64" hostid=%"PRIx64" seq=%"PRIu64"",
  //	   pcxt->clid, pcxt->hostid, pcxt->seq );

  if( !xdr ) {
    //    rpc_log( RPC_LOG_DEBUG, "raft_call_ping_cb timeout" );
    goto done;
  }

  sts = raft_member_by_hostid( pcxt->clid, pcxt->hostid, &member );
  if( sts ) goto done;
  member.lastseen = time( NULL );
  raft_member_set( &member );

  sts = raft_cluster_by_id( pcxt->clid, &cl );
  if( sts ) goto done;
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) goto done;
  sts = xdr_decode_uint64( xdr, &seq );
  if( sts ) goto done;

  if( seq > cl.seq ) {
    cl.leaderid = pcxt->hostid;
    raft_transition_follower( &cl );
  }

  if( !b ) {
    //    raft_cluster_by_id( pcxt->clid, &cl );
    //    cl.leaderid = pcxt->hostid;
    //    raft_transition_follower( &cl );
  }
  
 done:
  free( pcxt );
  
  return;
}

static void raft_call_ping( uint64_t clid, uint64_t hostid, uint64_t seq ) {
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  uint8_t xdr_buf[64];
  struct raft_ping_cxt *pcxt;
  
  //  rpc_log( RPC_LOG_DEBUG, "raft_call_ping clid=%"PRIx64" hostid=%"PRIx64" seq=%"PRIu64"",
  //	   clid, hostid, seq );

  pcxt = malloc( sizeof(*pcxt) );
  pcxt->clid = clid;
  pcxt->hostid = hostid;
  pcxt->seq = seq;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT_RPC_PROG;
  hcall.vers = RAFT_RPC_VERS;
  hcall.proc = 1;
  hcall.donecb = raft_call_ping_cb;
  hcall.cxt = pcxt;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  xdr_init( &xdr, xdr_buf, sizeof(xdr_buf) );
  xdr_encode_uint64( &xdr, clid );
  xdr_encode_uint64( &xdr, hostreg_localid() );
  xdr_encode_uint64( &xdr, seq );
  sts = hrauth_call_udp( &hcall, &xdr );
  if( sts ) {
    free( pcxt );
    rpc_log( RPC_LOG_ERROR, "raft_call_ping: hrauth_call failed" );
  }

}



struct raft_vote_cxt {
  uint64_t clid;
  uint64_t hostid;
  uint64_t seq;
};

static void raft_call_vote_cb( struct xdr_s *xdr, void *cxt ) {
  int sts, b;
  struct raft_ping_cxt *pcxt = (struct raft_ping_cxt *)cxt;
  struct raft_member member;
  struct raft_cluster cl;
  uint64_t seq;
  
  //  rpc_log( RPC_LOG_DEBUG, "raft_call_vote_cb clid=%"PRIx64" hostid=%"PRIx64" seq=%"PRIu64"",
  //	   pcxt->clid, pcxt->hostid, pcxt->seq );

  if( !xdr ) {
    //rpc_log( RPC_LOG_DEBUG, "raft_call_vote_cb timeout" );
    goto done;    
  }

  sts = raft_member_by_hostid( pcxt->clid, pcxt->hostid, &member );
  if( sts ) goto done;
  member.lastseen = time( NULL );
  sts = raft_member_set( &member );

  sts = raft_cluster_by_id( pcxt->clid, &cl );
  if( sts ) goto done;
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) goto done;
  sts = xdr_decode_uint64( xdr, &seq );
  if( sts ) goto done;

  if( seq > cl.seq ) {
    cl.leaderid = 0;
    raft_transition_follower( &cl );
    goto done;
  } else if( seq < cl.seq ) {
    goto done;
  }
  
  if( b && (cl.state == RAFT_STATE_CANDIDATE) && !(member.flags & RAFT_MEMBER_VOTED) ) {
    /* vote granted */
    member.flags |= RAFT_MEMBER_VOTED;
    raft_member_set( &member );

    cl.votes++;
    raft_cluster_set( &cl );

    if( cl.votes >= raft_cluster_quorum( cl.id ) ) {
      rpc_log( RPC_LOG_DEBUG, "raft_call_vote_cb sufficient votes received - LEADER" );
      raft_transition_leader( &cl );
    }
      
  }

 done:
  free( pcxt );
  
  return;
}

static void raft_call_vote( uint64_t clid, uint64_t hostid, uint64_t seq ) {
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  uint8_t xdr_buf[64];
  struct raft_ping_cxt *pcxt;
  
  //  rpc_log( RPC_LOG_DEBUG, "raft_call_vote clid=%"PRIx64" hostid=%"PRIx64" seq=%"PRIu64"",
  //	   clid, hostid, seq );

  pcxt = malloc( sizeof(*pcxt) );
  pcxt->clid = clid;
  pcxt->hostid = hostid;
  pcxt->seq = seq;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT_RPC_PROG;
  hcall.vers = RAFT_RPC_VERS;
  hcall.proc = 2;
  hcall.donecb = raft_call_vote_cb;
  hcall.cxt = pcxt;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  xdr_init( &xdr, xdr_buf, sizeof(xdr_buf) );
  xdr_encode_uint64( &xdr, clid );
  xdr_encode_uint64( &xdr, hostreg_localid() );
  xdr_encode_uint64( &xdr, seq );  
  sts = hrauth_call_udp( &hcall, &xdr );
  if( sts ) {
    free( pcxt );
    rpc_log( RPC_LOG_ERROR, "raft_call_vote: hrauth_call failed" );
  }

}


static void raft_transition_candidate( struct raft_cluster *cl ) {
  int i, n;
  struct raft_member member[32];
  uint32_t timeo;
  uint64_t now;

#if 0
  if( cl->state != RAFT_STATE_CANDIDATE ) {
    cl->seq++;
    cl->state = RAFT_STATE_CANDIDATE;
  }  
#else
  cl->seq++;
  cl->state = RAFT_STATE_CANDIDATE;
#endif
  
  now = rpc_now();
  timeo = elec_timeout();
  cl->timeout = now + timeo;
  cl->voteid = hostreg_localid(); /* vote for self */
  cl->votes = 1;
  cl->leaderid = 0;
  raft_cluster_set( cl );

  raft_member_clear_voted( cl->id );

  /* make sure the iterator fires in time */
  raft_iter_set_timeout( cl->timeout );
  
  /* send vote requests */
  n = raft_member_list( cl->id, member, 32 );
  for( i = 0; i < n; i++ ) {
    if( !(member[i].flags & RAFT_MEMBER_LOCAL) ) {      
      raft_call_vote( cl->id, member[i].hostid, cl->seq );
    }
  }
  
}

static void raft_send_pings( struct raft_cluster *cl ) {
  int i, n;
  struct raft_member member[32];
  
  n = raft_member_list( cl->id, member, 32 );
  for( i = 0; i < n; i++ ) {
    if( !(member[i].flags & RAFT_MEMBER_LOCAL) ) {      
      raft_call_ping( cl->id, member[i].hostid, cl->seq );
    }
  }

}

static void raft_transition_leader( struct raft_cluster *cl ) {
  uint32_t timeo;
  uint64_t now;
  
  cl->state = RAFT_STATE_LEADER;

  now = rpc_now();
  timeo = glob.prop.term_low;
  cl->timeout = now + timeo;
  cl->voteid = 0;
  cl->votes = 0;
  cl->leaderid = hostreg_localid();
  raft_cluster_set( cl );

  raft_iter_set_timeout( cl->timeout );

  raft_send_pings( cl );
}


static void raft_iter_cb( struct rpc_iterator *iter ) {
  int sts;
  uint64_t now;
  struct raft_cluster cl[32];
  int i, n, j, m;
  struct raft_member member[32];
  uint32_t timeo;
  
  now = rpc_now();
  if( now >= glob.nextprop ) {
    raft_prop( &glob.prop );
    glob.nextprop = now + 60000;
  }

  n = raft_cluster_list( cl, 32 );
  for( i = 0; i < n; i++ ) {
    switch( cl[i].state ) {
    case RAFT_STATE_FOLLOWER:
      /* check for term timeout */
      if( now >= cl[i].timeout ) {
	raft_transition_candidate( &cl[i] );
      }
      
      break;
    case RAFT_STATE_CANDIDATE:
      /* check for election timeout */
      if( now >= cl[i].timeout ) {
	if( cl[i].votes >= raft_cluster_quorum( cl[i].id ) ) {
	  rpc_log( RPC_LOG_DEBUG, "candidate election successful" );
	  raft_transition_leader( &cl[i] );
	} else {
	  /* not enough votes gathered, transition back to candidate */
	  rpc_log( RPC_LOG_DEBUG, "candidate election timeout votes=%u/%u",
		   cl[i].votes, raft_cluster_quorum( cl[i].id ) );
	  raft_transition_candidate( &cl[i] );
	}
      } else {
      }
      
      break;
    case RAFT_STATE_LEADER:      
      /* send pings */
      if( now >= cl[i].timeout ) {
	cl[i].timeout = rpc_now() + glob.prop.term_low;
	raft_cluster_set( &cl[i] );	
	raft_iter_set_timeout( cl[i].timeout );

	raft_send_pings( &cl[i] );
	
      }
      
      // TODO

      /* check quorum? */
      
      break;
    }
  }

}










static int raft_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static uint32_t term_timeout( void ) {
  uint32_t n;
  sec_rand( &n, sizeof(n) );
  return glob.prop.term_low + (n % (glob.prop.term_high - glob.prop.term_low));
}

static uint32_t elec_timeout( void ) {
  uint32_t n;
  sec_rand( &n, sizeof(n) );
  n = glob.prop.elec_low + (n % (glob.prop.elec_high - glob.prop.elec_low));
  return n;
}
    
/* sent from leaders to followers for term keep alives */
static int raft_proc_ping( struct rpc_inc *inc ) {
  int handle, sts;
  struct raft_member member;
  struct raft_cluster cl;
  uint64_t clid, leaderid, seq;
  struct hrauth_context *hcxt;
  
  if( !inc->pvr || (inc->pvr->flavour != RPC_AUTH_HRAUTH) ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }

  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) xdr_decode_uint64( &inc->xdr, &leaderid );
  if( !sts ) xdr_decode_uint64( &inc->xdr, &seq );
  if( sts ) rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  //  rpc_log( RPC_LOG_DEBUG, "raft_proc_ping clid=%"PRIx64" hostid=%"PRIx64" Seq=%"PRIu64"",
  //	   clid, leaderid, seq );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  memset( &cl, 0, sizeof(cl) );
  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  /* compare seq */
  if( seq < cl.seq ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }
  
  if( seq > cl.seq ) {
    cl.leaderid = leaderid;
    cl.seq = seq;    
    raft_transition_follower( &cl );
  }
  
  if( cl.leaderid && (cl.leaderid != leaderid) ) {
    rpc_log( RPC_LOG_DEBUG, "conflicting leadership claim with matching seqno?" );
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  cl.leaderid = leaderid;
  cl.timeout = rpc_now() + term_timeout();
  raft_cluster_set( &cl );
  raft_iter_set_timeout( cl.timeout );
  
  xdr_encode_boolean( &inc->xdr, 1 );

 done:

  /* update last seen */
  hcxt = (struct hrauth_context *)inc->pcxt;
  sts = raft_member_by_hostid( clid, hcxt->remoteid, &member );
  if( !sts ) {
    member.lastseen = time( NULL );
    raft_member_set( &member );
  } else {
  }
    
  xdr_encode_uint64( &inc->xdr, cl.seq );  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

/* sent from candidates for elections */
static int raft_proc_vote( struct rpc_inc *inc ) {
  int handle, sts;
  struct raft_cluster cl;
  uint64_t clid, candid, seq;
  struct hrauth_context *hcxt;
  struct raft_member member;
  
  if( !inc->pvr || (inc->pvr->flavour != RPC_AUTH_HRAUTH) ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) xdr_decode_uint64( &inc->xdr, &candid );
  if( !sts ) xdr_decode_uint64( &inc->xdr, &seq );
  if( sts ) rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  //  rpc_log( RPC_LOG_DEBUG, "raft_proc_vote clid=%"PRIx64" hostid=%"PRIx64" seq=%"PRIu64"",
  //	   clid, candid, seq );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  /* check seqno */
  if( seq < cl.seq ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  if( seq > cl.seq ) {
    cl.seq = seq;
    cl.leaderid = 0;
    raft_transition_follower( &cl );
  }

  switch( cl.state ) {
  case RAFT_STATE_FOLLOWER:
    if( cl.voteid == 0 || cl.voteid == candid ) {
      xdr_encode_boolean( &inc->xdr, 1 );
      cl.voteid = candid;
      cl.timeout = rpc_now() + term_timeout();
      raft_cluster_set( &cl );
      raft_iter_set_timeout( cl.timeout );
    } else {
      xdr_encode_boolean( &inc->xdr, 0 );
    }
    break;
  case RAFT_STATE_CANDIDATE:
    xdr_encode_boolean( &inc->xdr, 0 );
    break;
  case RAFT_STATE_LEADER:
    xdr_encode_boolean( &inc->xdr, 0 );
    break;
  }

  
 done:
  /* update last seen */
  hcxt = (struct hrauth_context *)inc->pcxt;
  sts = raft_member_by_hostid( clid, hcxt->remoteid, &member );
  if( !sts ) {
    member.lastseen = time( NULL );
    raft_member_set( &member );
  } else {
  }

  xdr_encode_uint64( &inc->xdr, cl.seq );  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

/* sent from candidates for elections */
static int raft_proc_list( struct rpc_inc *inc ) {
  int handle, sts;
  struct raft_cluster cl[32];
  struct raft_member member[32];
  int i, j, n, m;
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  n = raft_cluster_list( cl, 32 );
  xdr_encode_uint32( &inc->xdr, n );
  for( i = 0; i < n; i++ ) {
    xdr_encode_uint64( &inc->xdr, cl[i].id );
    xdr_encode_uint64( &inc->xdr, cl[i].leaderid );
    xdr_encode_uint64( &inc->xdr, cl[i].seq );
    xdr_encode_uint64( &inc->xdr, cl[i].voteid );
    xdr_encode_uint32( &inc->xdr, cl[i].state );
    
    m = raft_member_list( cl[i].id, member, 32 );
    xdr_encode_uint32( &inc->xdr, m );
    for( j = 0; j < m; j++ ) {
      xdr_encode_uint64( &inc->xdr, member[j].hostid );
      xdr_encode_uint64( &inc->xdr, member[j].lastseen );
      xdr_encode_uint32( &inc->xdr, member[j].flags );
    }
  }
  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct rpc_proc raft_procs[] = {
  { 0, raft_proc_null },
  { 1, raft_proc_ping },
  { 2, raft_proc_vote },
  { 3, raft_proc_list },
  { 0, NULL }
};

static struct rpc_version raft_vers = {
  NULL, RAFT_RPC_VERS, raft_procs
};

static struct rpc_program raft_prog = {
  NULL, RAFT_RPC_PROG, &raft_vers
};

void raft_register( void ) {
  int i, n;
  struct raft_cluster cl[32];
  
  raft_open();

  raft_prop( &glob.prop );
  
  /* set all clusters to follower state */
  n = raft_cluster_list( cl, 32 );
  for( i = 0; i < n; i++ ) {
    cl[i].state = RAFT_STATE_FOLLOWER;
    cl[i].votes = 0;
    cl[i].voteid = 0;
    cl[i].leaderid = 0;
    cl[i].timeout = rpc_now() + glob.prop.term_high;
    raft_cluster_set( &cl[i] );
  }
  
  rpc_program_register( &raft_prog );
  rpc_iterator_register( &raft_iter );
}
