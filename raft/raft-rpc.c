/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
*/
 
#include <rpc.h>
#include <sec.h>
#include <inttypes.h>
#include <hrauth.h>
#include <hostreg.h>

#include "raft.h"

static struct {  
  struct raft_prop prop;
  uint64_t nextprop;
  struct raft_notify_context *notlist;
} glob;

static void raft_iter_cb( struct rpc_iterator *iter );
static uint32_t term_timeout( void );
static uint32_t elec_timeout( void );
static void raft_transition_follower( struct raft_cluster *cl );
static void raft_transition_candidate( struct raft_cluster *cl );
static void raft_transition_leader( struct raft_cluster *cl );
static void raft_call_ping( struct raft_cluster *cl, uint64_t hostid );
static void raft_call_vote( struct raft_cluster *cl, uint64_t hostid );
static void raft_notify( raft_notify_t evt, struct raft_cluster *cl, void *reserved );

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

  if( cl->state != RAFT_STATE_FOLLOWER ) raft_notify( RAFT_NOTIFY_FOLLOWER, cl, NULL );
  
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
  uint64_t termseq;
};

static void raft_call_ping_cb( struct xdr_s *xdr, void *cxt ) {
  int sts, b;
  struct raft_ping_cxt *pcxt = (struct raft_ping_cxt *)cxt;
  struct raft_member member;
  uint64_t termseq, commitseq;
  struct raft_cluster cl;

  //  rpc_log( RPC_LOG_DEBUG, "raft_call_ping_cb clid=%"PRIx64" hostid=%"PRIx64" termseq=%"PRIu64"",
  //	   pcxt->clid, pcxt->hostid, pcxt->termseq );

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
  sts = xdr_decode_uint64( xdr, &termseq );
  if( sts ) goto done;
  if( sts ) xdr_decode_uint64( xdr, &commitseq );
  if( sts ) goto done;
  
  if( termseq > cl.termseq ) {
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

static void raft_call_ping( struct raft_cluster *cl, uint64_t hostid ) {
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  uint8_t xdr_buf[64];
  struct raft_ping_cxt *pcxt;
  
  //  rpc_log( RPC_LOG_DEBUG, "raft_call_ping clid=%"PRIx64" hostid=%"PRIx64" termseq=%"PRIu64"",
  //	   clid, hostid, termseq );

  pcxt = malloc( sizeof(*pcxt) );
  pcxt->clid = cl->id;
  pcxt->hostid = hostid;
  pcxt->termseq = cl->termseq;
  
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
  xdr_encode_uint64( &xdr, cl->id );
  xdr_encode_uint64( &xdr, hostreg_localid() );
  xdr_encode_uint64( &xdr, cl->termseq );
  xdr_encode_uint64( &xdr, cl->commitseq );
  sts = hrauth_call_udp_async( &hcall, &xdr, NULL );
  if( sts ) {
    free( pcxt );
    rpc_log( RPC_LOG_ERROR, "raft_call_ping: hrauth_call failed" );
  }

}



struct raft_vote_cxt {
  uint64_t clid;
  uint64_t hostid;
  uint64_t termseq;
};

static void raft_call_vote_cb( struct xdr_s *xdr, void *cxt ) {
  int sts, b;
  struct raft_ping_cxt *pcxt = (struct raft_ping_cxt *)cxt;
  struct raft_member member;
  struct raft_cluster cl;
  uint64_t termseq;
  
  //  rpc_log( RPC_LOG_DEBUG, "raft_call_vote_cb clid=%"PRIx64" hostid=%"PRIx64" termseq=%"PRIu64"",
  //	   pcxt->clid, pcxt->hostid, pcxt->termseq );

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
  sts = xdr_decode_uint64( xdr, &termseq );
  if( sts ) goto done;

  if( termseq > cl.termseq ) {
    cl.leaderid = 0;
    raft_transition_follower( &cl );
    goto done;
  } else if( termseq < cl.termseq ) {
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

static void raft_call_vote( struct raft_cluster *cl, uint64_t hostid ) {
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  uint8_t xdr_buf[64];
  struct raft_ping_cxt *pcxt;
  
  //  rpc_log( RPC_LOG_DEBUG, "raft_call_vote clid=%"PRIx64" hostid=%"PRIx64" termseq=%"PRIu64"",
  //	   clid, hostid, termseq );

  pcxt = malloc( sizeof(*pcxt) );
  pcxt->clid = cl->id;
  pcxt->hostid = hostid;
  pcxt->termseq = cl->termseq;
  
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
  xdr_encode_uint64( &xdr, cl->id );
  xdr_encode_uint64( &xdr, hostreg_localid() );
  xdr_encode_uint64( &xdr, cl->termseq );
  xdr_encode_uint64( &xdr, cl->stateseq );
  xdr_encode_uint64( &xdr, cl->stateterm );  
  sts = hrauth_call_udp_async( &hcall, &xdr, NULL );
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

  if( cl->flags & RAFT_CLUSTER_WITNESS ) return;
  
  if( cl->state != RAFT_STATE_CANDIDATE ) raft_notify( RAFT_NOTIFY_CANDIDATE, cl, NULL );
  
#if 0
  if( cl->state != RAFT_STATE_CANDIDATE ) {
    cl->termseq++;
    cl->state = RAFT_STATE_CANDIDATE;
  }  
#else
  cl->termseq++;
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
  raft_notify( RAFT_NOTIFY_SEND_VOTE, cl, NULL );
  n = raft_member_list( cl->id, member, 32 );
  for( i = 0; i < n; i++ ) {
    raft_call_vote( cl, member[i].hostid );
  }
  
}

static void raft_send_pings( struct raft_cluster *cl ) {
  int i, n;
  struct raft_member member[32];
  
  n = raft_member_list( cl->id, member, 32 );
  for( i = 0; i < n; i++ ) {
    raft_call_ping( cl, member[i].hostid );
  }

  raft_notify( RAFT_NOTIFY_SEND_PING, cl, NULL );
  
}

static void raft_transition_leader( struct raft_cluster *cl ) {
  uint32_t timeo;
  uint64_t now;

  if( cl->state != RAFT_STATE_LEADER ) raft_notify( RAFT_NOTIFY_LEADER, cl, NULL );
  
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
  uint64_t now;
  struct raft_cluster cl[32];
  int i, n;
  
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
  uint64_t clid, leaderid, termseq, commitseq;
  struct hrauth_context *hcxt;
  
  if( !inc->pvr || (inc->pvr->flavour != RPC_AUTH_HRAUTH) ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }

  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &leaderid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &termseq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &commitseq );
  if( sts ) rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  //  rpc_log( RPC_LOG_DEBUG, "raft_proc_ping clid=%"PRIx64" hostid=%"PRIx64" termseq=%"PRIu64"",
  //	   clid, leaderid, termseq );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  memset( &cl, 0, sizeof(cl) );
  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  /* compare termseq */
  if( termseq < cl.termseq ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }
  
  if( termseq > cl.termseq ) {
    cl.leaderid = leaderid;
    cl.termseq = termseq;    
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
    
  xdr_encode_uint64( &inc->xdr, cl.termseq );  
  rpc_complete_accept_reply( inc, handle );

  raft_notify( RAFT_NOTIFY_RECV_PING, &cl, NULL );
  
  return 0;
}

/* sent from candidates for elections */
static int raft_proc_vote( struct rpc_inc *inc ) {
  int handle, sts;
  struct raft_cluster cl;
  uint64_t clid, candid, termseq, stateseq, stateterm;
  struct hrauth_context *hcxt;
  struct raft_member member;
  
  if( !inc->pvr || (inc->pvr->flavour != RPC_AUTH_HRAUTH) ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &candid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &termseq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &stateseq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &stateterm ); 
  if( sts ) rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  //  rpc_log( RPC_LOG_DEBUG, "raft_proc_vote clid=%"PRIx64" hostid=%"PRIx64" termseq=%"PRIu64"",
  //	   clid, candid, termseq );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  /* check term seqno */
  if( termseq < cl.termseq ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  if( termseq > cl.termseq ) {
    cl.termseq = termseq;
    cl.leaderid = 0;
    raft_transition_follower( &cl );
  }

  switch( cl.state ) {
  case RAFT_STATE_FOLLOWER:
    if( (cl.voteid == 0 || cl.voteid == candid) &&
	(stateseq >= cl.stateseq) ) {
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

  xdr_encode_uint64( &inc->xdr, cl.termseq );  
  rpc_complete_accept_reply( inc, handle );

  raft_notify( RAFT_NOTIFY_RECV_VOTE, &cl, NULL );
  
  return 0;
}

/* sent from candidates for elections */
static int raft_proc_list( struct rpc_inc *inc ) {
  int handle;
  struct raft_cluster cl[32];
  struct raft_member member[32];
  int i, j, n, m;
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  n = raft_cluster_list( cl, 32 );
  xdr_encode_uint32( &inc->xdr, n );
  for( i = 0; i < n; i++ ) {
    xdr_encode_uint64( &inc->xdr, cl[i].id );
    xdr_encode_uint64( &inc->xdr, cl[i].leaderid );
    xdr_encode_uint64( &inc->xdr, cl[i].termseq );
    xdr_encode_uint64( &inc->xdr, cl[i].voteid );
    xdr_encode_uint32( &inc->xdr, cl[i].state );
    xdr_encode_uint32( &inc->xdr, cl[i].typeid );
    xdr_encode_uint64( &inc->xdr, cl[i].commitseq );
    xdr_encode_uint64( &inc->xdr, cl[i].stateseq );
    xdr_encode_uint64( &inc->xdr, cl[i].stateterm );
    
    m = raft_member_list( cl[i].id, member, 32 );
    xdr_encode_uint32( &inc->xdr, m );
    for( j = 0; j < m; j++ ) {
      xdr_encode_uint64( &inc->xdr, member[j].hostid );
      xdr_encode_uint64( &inc->xdr, member[j].lastseen );
      xdr_encode_uint32( &inc->xdr, member[j].flags );
      xdr_encode_uint64( &inc->xdr, member[j].nextseq );
      xdr_encode_uint64( &inc->xdr, member[j].stateseq );
    }
  }
  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int raft_proc_set( struct rpc_inc *inc ) {
  int handle, sts;
  struct raft_cluster cl;
  struct raft_member member;
  int j, m;

  if( !inc->pvr || (inc->pvr->flavour != RPC_AUTH_HRAUTH) ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }

  memset( &cl, 0, sizeof(cl) );

  sts = xdr_decode_uint64( &inc->xdr, &cl.id );
  if( !sts ) raft_cluster_by_id( cl.id, &cl );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &cl.typeid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &cl.flags );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  raft_cluster_set( &cl );
  
  sts = xdr_decode_uint32( &inc->xdr, (uint32_t *)&m );
  if( sts || (m > 32) ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  
  for( j = 0; j < m; j++ ) {
    memset( &member, 0, sizeof(member) );
    sts = xdr_decode_uint64( &inc->xdr, &member.hostid );
    if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
    sts = raft_member_by_hostid( cl.id, member.hostid, &member );
    if( sts ) raft_member_set( &member );
  }
    
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, 1 );
  rpc_complete_accept_reply( inc, handle );
    
  return 0;
}

static int raft_proc_rem( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t clid;

  if( !inc->pvr || (inc->pvr->flavour != RPC_AUTH_HRAUTH) ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }

  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  sts = raft_cluster_rem( clid );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static struct rpc_proc raft_procs[] = {
  { 0, raft_proc_null },
  { 1, raft_proc_ping },
  { 2, raft_proc_vote },
  { 3, raft_proc_list },
  { 4, raft_proc_set },
  { 5, raft_proc_rem },
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

void raft_notify_register( struct raft_notify_context *cxt ) {
  cxt->next = glob.notlist;
  glob.notlist = cxt;
}

static void raft_notify( raft_notify_t evt, struct raft_cluster *cl, void *reserved ) {
    struct raft_notify_context *ncxt;
    ncxt = glob.notlist;
    while( ncxt ) {
	ncxt->cb( evt, cl, ncxt->cxt, reserved );
	ncxt = ncxt->next;
    }  
}
