
#include <fju/raft2.h>
#include <fju/mmf.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/hostreg.h>
#include <fju/hrauth.h>
#include <fju/programs.h>
#include <fju/log.h>
#include <fju/sec.h>

static log_deflogger(raft2_log, RAFT2_RPC_PROG)
  
struct raft2_file {
  struct raft2_prop prop;
  struct raft2_cluster cluster[RAFT2_MAX_CLUSTER];
};

static struct {
  uint32_t ocount;
  struct mmf_s mmf;
  struct raft2_file *file;

  struct raft2_prop prop;
  struct raft2_cluster cl[RAFT2_MAX_CLUSTER];
  uint32_t ncl;
} glob;

static uint64_t raft2_term_timeout( void ) {
  return rpc_now() + glob.prop.term_low +
    (sec_rand_uint32() % (glob.prop.term_high - glob.prop.term_low));
}

static uint64_t raft2_elec_timeout( void ) {
  return rpc_now() + glob.prop.elec_low +
    (sec_rand_uint32() % (glob.prop.elec_high - glob.prop.elec_low));
}

static void raft2_lock( void ) {
  mmf_lock( &glob.mmf );
}
static void raft2_unlock( void ) {
  mmf_unlock( &glob.mmf );
}

int raft2_open( void ) {
  int sts;

  if( glob.ocount > 0 ) {
    glob.ocount++;
    return 0;
  }

  sts = mmf_open( mmf_default_path( "raft", "raft2.dat", NULL ), &glob.mmf );
  if( sts ) return sts;

  sts = mmf_remap( &glob.mmf, sizeof(*glob.file) );

  glob.file = (struct raft2_file *)glob.mmf.file;

  if( glob.file->prop.magic != RAFT2_MAGIC ) {
    glob.file->prop.magic = RAFT2_MAGIC;
    glob.file->prop.version = RAFT2_VERSION;
    glob.file->prop.seq = 1;
    glob.file->prop.count = 0;
    glob.file->prop.flags = 0;
    glob.file->prop.elec_low = 1000;
    glob.file->prop.elec_high = 3000;
    glob.file->prop.term_low = 3000;
    glob.file->prop.term_high = 8000;
    glob.file->prop.rpc_timeout = 200;
  }
  
  glob.ocount = 1;
  return 0;
}

int raft2_close( void ) {
  if( glob.ocount == 0 ) return -1;
  if( glob.ocount > 1 ) {
    glob.ocount--;
    return 0;
  }
  mmf_close( &glob.mmf );
  glob.ocount = 0;
  return 0;
}

int raft2_prop( struct raft2_prop *prop ) {
  if( !glob.ocount ) return -1;
  raft2_lock();
  *prop = glob.file->prop;
  raft2_unlock();
  return 0;
}

int raft2_prop_set( uint32_t mask, struct raft2_prop *prop ) {
  if( !glob.ocount ) return -1;
  raft2_lock();
  if( mask & RAFT2_PROP_ELEC_LOW ) {
    glob.file->prop.elec_low = prop->elec_low;
  }
  if( mask & RAFT2_PROP_ELEC_HIGH ) {
    glob.file->prop.elec_high = prop->elec_high;
  }
  if( mask & RAFT2_PROP_TERM_LOW ) {
    glob.file->prop.term_low = prop->term_low;
  }
  if( mask & RAFT2_PROP_TERM_HIGH ) {
    glob.file->prop.term_high = prop->term_high;
  }
  if( mask & RAFT2_PROP_RPC_TIMEOUT ) {
    glob.file->prop.rpc_timeout = prop->rpc_timeout;
  }
  raft2_unlock();
  return 0;
}  


/* database functions */
int raft2_cluster_list( struct raft2_cluster *cl, int ncl ) {
  int i;
  if( !glob.ocount ) return -1;
  raft2_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( i < ncl ) cl[i] = glob.file->cluster[i];
  }
  i = glob.file->prop.count;
  raft2_unlock();
  return i;
}

int raft2_clid_list( uint64_t *clid, int ncl ) {
  int i;
  if( !glob.ocount ) return -1;
  raft2_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( i < ncl ) clid[i] = glob.file->cluster[i].clid;
  }
  i = glob.file->prop.count;
  raft2_unlock();
  return i;  
}

int raft2_cluster_by_clid( uint64_t clid, struct raft2_cluster *cl ) {
  int i, sts;

  sts = -1;
  if( !glob.ocount ) return -1;
  raft2_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( glob.file->cluster[i].clid == clid ) {
      if( cl ) *cl = glob.file->cluster[i];
      sts = 0;
      break;
    }
  }
  raft2_unlock();
  return sts;
}

int raft2_cluster_set( struct raft2_cluster *cl ) {
  int i, sts;

  if( !cl->clid ) sec_rand( &cl->clid, sizeof(cl->clid) );
  
  sts = -1;
  if( !glob.ocount ) return -1;
  raft2_lock();
  
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( glob.file->cluster[i].clid == cl->clid ) {
      if( cl ) {
	glob.file->cluster[i] = *cl;
	glob.file->prop.seq++;
      }
      sts = 0;
      goto done;
    }
  }

  if( glob.file->prop.count >= RAFT2_MAX_CLUSTER ) goto done;

  i = glob.file->prop.count;
  cl->term = 1;
  cl->seq = 1;
  glob.file->cluster[i] = *cl;
  glob.file->prop.count++;
  glob.file->prop.seq++;
  sts = 0;
  
 done:
  raft2_unlock();
  return sts;  
}

int raft2_cluster_rem( uint64_t clid ) {
  int i, sts;

  sts = -1;
  if( !glob.ocount ) return -1;
  raft2_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( glob.file->cluster[i].clid == clid ) {
      if( i != (glob.file->prop.count - 1) ) glob.file->cluster[i] = glob.file->cluster[glob.file->prop.count - 1];
      glob.file->prop.count--;
      sts = 0;
      break;
    }
  }
  raft2_unlock();
  return sts;    
}

/* ------------ rpc -------------------- */

static void raft2_convert_follower( struct raft2_cluster *cl, uint64_t term );
static void raft2_convert_candidate( struct raft2_cluster *cl );
static void raft2_convert_leader( struct raft2_cluster *cl );

static struct raft2_cluster *cl_by_id( uint64_t clid ) {
  int i;
  for( i = 0; i < glob.ncl; i++ ) {
    if( glob.cl[i].clid == clid ) return &glob.cl[i];
  }
  return NULL;
}

static void raft2_ping_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t clid, hostid, term;
  struct raft2_cluster *cl;
  struct raft2_member *mp;
  int i, success, sts;
  
  raft2_log( LOG_LVL_TRACE, "Ping %s", res ? "Timeout" : "Success" );

  clid = hcallp->cxt2;
  cl = cl_by_id( clid );
  if( !cl ) return;
  hostid = hcallp->hostid;

  mp = NULL;
  for( i = 0; i < cl->nmember; i++ ) {
    if( cl->member[i].hostid == hostid ) {
      mp = &cl->member[i];
      break;
    }
  }
  if( !mp ) {
    raft2_log( LOG_LVL_ERROR, "Unknown member" );
    return;
  }

  sts = xdr_decode_boolean( res, &success );
  if( !sts ) sts = xdr_decode_uint64( res, &term );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "Xdr decode error" );
    return;
  }

  if( term > cl->term ) {
  }

  if( !success ) {
  }
  
  mp->lastseen = rpc_now();
  raft2_cluster_set( cl );
  return;  
}

static void raft2_call_ping( struct raft2_cluster *cl, uint64_t hostid ) {
  struct hrauth_call hcall;
  struct xdr_s args;
  char argbuf[256];
  int sts;
  
  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  /* encode args */


  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT2_RPC_PROG;
  hcall.vers = RAFT2_RPC_VERS;
  hcall.proc = 1; /* ping */
  hcall.donecb = raft2_ping_cb;
  hcall.cxt2 = cl->clid;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, &args );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "hrauth_call failed" );
    return;
  }

  
}

static void raft2_send_pings( struct raft2_cluster *cl ) {
  int i;
  for( i = 0; i < cl->nmember; i++ ) {
    raft2_call_ping( cl, cl->member[i].hostid );
  }
}

static void raft2_vote_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t clid, hostid, term;
  struct raft2_cluster *cl;
  struct raft2_member *mp;
  int i, success, sts, count;
  
  raft2_log( LOG_LVL_DEBUG, "vote %s", res ? "timeout" : "success" );

  clid = hcallp->cxt2;
  cl = cl_by_id( clid );
  if( !cl ) return;
  hostid = hcallp->hostid;

  mp = NULL;
  for( i = 0; i < cl->nmember; i++ ) {
    if( cl->member[i].hostid == hostid ) {
      mp = &cl->member[i];
      break;
    }
  }
  if( !mp ) {
    raft2_log( LOG_LVL_ERROR, "Unknown member" );
    return;
  }

  sts = xdr_decode_boolean( res, &success );
  if( !sts ) sts = xdr_decode_uint64( res, &term );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "Xdr decode error" );
    return;
  }

  if( term > cl->term ) {
    raft2_convert_follower( cl, term );
  } else if( success ) {
    mp->flags |= RAFT2_MEMBER_VOTED;
    count = 1;
    for( i = 0; i < cl->nmember; i++ ) {
      if( cl->member[i].flags & RAFT2_MEMBER_VOTED ) count++;
    }
    if( count >= (cl->nmember + 1) / 2 ) {
      raft2_convert_leader( cl );
    }
  }
  
  mp->lastseen = rpc_now();
  raft2_cluster_set( cl );
  return;  
}

static void raft2_call_vote( struct raft2_cluster *cl, uint64_t hostid ) {
  struct hrauth_call hcall;
  struct xdr_s args;
  char argbuf[256];
  int sts;
  
  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  /* encode args */


  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT2_RPC_PROG;
  hcall.vers = RAFT2_RPC_VERS;
  hcall.proc = 2; /* vote */
  hcall.donecb = raft2_vote_cb;
  hcall.cxt2 = cl->clid;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, &args );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "hrauth_call failed" );
    return;
  }

  
}

static void raft2_convert_follower( struct raft2_cluster *cl, uint64_t term ) {
  raft2_log( LOG_LVL_INFO, "Convert to follower" );  
  cl->state = RAFT2_STATE_FOLLOWER;
  cl->term = term;
  cl->timeout = raft2_term_timeout();
  raft2_cluster_set( cl );
}

static void raft2_convert_candidate( struct raft2_cluster *cl ) {
  int i;
  raft2_log( LOG_LVL_INFO, "Convert to candidate" );
  cl->state = RAFT2_STATE_CANDIDATE;
  cl->term++;
  cl->timeout = raft2_elec_timeout();
  for( i = 0; i < cl->nmember; i++ ) {
    cl->member[i].flags &= ~RAFT2_MEMBER_VOTED;
    raft2_call_vote( cl, cl->member[i].hostid );
  }
  raft2_cluster_set( cl );
}

static void raft2_convert_leader( struct raft2_cluster *cl ) {
  raft2_log( LOG_LVL_INFO, "Convert to leader" );

  cl->state = RAFT2_STATE_LEADER;
  cl->timeout = glob.prop.term_low;
  raft2_send_pings( cl );
  raft2_cluster_set( cl );
}






static int raft_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

/* 
 * send from leaders to keep their term alive
 */
static int raft_proc_ping( struct rpc_inc *inc ) {
  int handle, sts, success;
  uint64_t clid, hostid, term, seq, commitseq, nextseq, s;
  struct raft2_cluster *clp;
  struct hrauth_context *hc;
  struct xdr_s res;
  char resbuf[16];
  
  /* we can guarantee this because we set a mandatory authenticator */
  hc = (struct hrauth_context *)inc->pcxt;
  success = 0;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &hostid ); /* leaderid (must match sender id!) */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &term ); /* current term */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &seq ); /* current seq */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &commitseq ); /* leaders commit seq */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &nextseq ); /* next command seq */
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  if( hc->remoteid != hostid ) {
    /* sender is different to claimed leader */
    raft2_log( LOG_LVL_WARN, "Remoteid mismatch" );
  }
  
  clp = cl_by_id( clid );
  if( !clp ) goto done;

  if( term < clp->term ) {
    /* term too old, reject */
    raft2_log( LOG_LVL_WARN, "Term old - rejected" );
    term = clp->term;
    goto done;
  }

  if( term > clp->term ) {
    /* term increased, convert to follower */
    raft2_log( LOG_LVL_INFO, "Term incresed - Convert to follower" );
    raft2_convert_follower( clp, term );
  }

  if( seq > clp->seq ) {
    /* we have missed some updates?  */
    raft2_log( LOG_LVL_WARN, "Seq jump - missed updates?" );
  }
    
  
  /* lookup command buffer for (term,seq), reply false if not found */
  /* TODO */

  /* if a command buffer is found for (seq) but a different term then delete that entry and all others after it */
  /* TODO */

  if( nextseq > seq ) {
    raft2_log( LOG_LVL_DEBUG, "Seq bump" );
    /* 
     * Apply new command:
     * - Lookup command buffer for seq+1, apply. set clp->commitseq to seq+1 
     * - continue for seq+2, ... upto nextseq 
     * - Stop if command buffer not found. Request the buffer from leader.
     */
    for( s = clp->seq; s <= nextseq; s++ ) {
      /* Lookup command buffer, break if not found */

      /* Apply command buffer */
      
      clp->seq = s;
      raft2_cluster_set( clp );
    }
  }

  success = 1;

 done:
  xdr_init( &res, (uint8_t *)resbuf, sizeof(resbuf) );
  xdr_encode_boolean( &res, success );
  xdr_encode_uint64( &res, term );
  return hrauth_reply( inc, &res );
}

/* 
 * sent from candidates to gather votes 
 */
static int raft_proc_vote( struct rpc_inc *inc ) {
  int handle, sts, success;
  uint64_t clid, hostid, term, seq;
  struct raft2_cluster *clp;
  struct hrauth_context *hc;
  struct xdr_s res;
  char resbuf[16];

  /* we can guarantee this because we set a mandatory authenticator */
  hc = (struct hrauth_context *)inc->pcxt;
  success = 0;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &hostid ); /* leaderid (must match sender id!) */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &term ); /* current term */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &seq ); /* current seq */
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  if( hc->remoteid != hostid ) {
    /* sender is different to claimed leader */
    raft2_log( LOG_LVL_WARN, "Remoteid mismatch" );
  }
  
  clp = cl_by_id( clid );
  if( !clp ) goto done;

  if( term < clp->term ) {
    /* term too old, reject */
    raft2_log( LOG_LVL_WARN, "Old term - rejecting" );
    term = clp->term;
    goto done;
  }

  if( term > clp->term ) {
    /* term increased, convert to follower */
    raft2_log( LOG_LVL_INFO, "Term increased - convert to follower" );
    clp->state = RAFT2_STATE_FOLLOWER;
    clp->term = term;
    raft2_cluster_set( clp );    
  }

  /* grant vote if not voted yet or voted for this host already AND the candidate is at least as up to date as us */
  if( (clp->voteid == 0 || clp->voteid == hostid) && (seq >= clp->seq) ) {
    raft2_log( LOG_LVL_DEBUG, "Granting vote" );
    clp->voteid = hostid;
    success = 1;
  }

  
 done:
  xdr_init( &res, (uint8_t *)resbuf, sizeof(resbuf) );
  xdr_encode_boolean( &res, success );
  xdr_encode_uint64( &res, term );
  return hrauth_reply( inc, &res );
}

/*
 * sent from leaders to distribute command buffers. 
 */
static int raft_proc_putcommand( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

/* 
 * Sent from nodes that come online to update their state 
 */
static int raft_proc_getcommand( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}


static struct rpc_proc raft_procs[] = {
  { 0, raft_proc_null },
  { 1, raft_proc_ping },
  { 2, raft_proc_vote },  
  { 3, raft_proc_putcommand },
  { 4, raft_proc_getcommand },  
  { 0, NULL }
};

static struct rpc_version raft_vers = {
  NULL, RAFT2_RPC_VERS, raft_procs
};

static uint32_t raft_prog_auths[] = { RPC_AUTH_HRAUTH, 0 };
static struct rpc_program raft_prog = {
    NULL, RAFT2_RPC_PROG, &raft_vers, raft_prog_auths
};


static void raft2_iter_cb( struct rpc_iterator *iter ) {
  uint64_t now, to;
  int i, j;
  
  raft2_log( LOG_LVL_TRACE, "raft2 iterator" );
  raft2_prop( &glob.prop );
  glob.ncl = raft2_cluster_list( glob.cl, RAFT2_MAX_CLUSTER );

  /* ensure connections registered */
  for( i = 0; i < glob.ncl; i++ ) {
    for( j = 0; j < glob.cl[i].nmember; j++ ) {
      hrauth_conn_register( glob.cl[i].member[j].hostid, NULL );
    }
  }
  
  /* check timeouts */
  now = rpc_now();
  for( i = 0; i < glob.ncl; i++ ) {
    if( now >= glob.cl[i].timeout ) {
      switch( glob.cl[i].state ) {
      case RAFT2_STATE_FOLLOWER:
	/* term timeout - convert to candidate */
	raft2_convert_candidate( &glob.cl[i] );
	break;
      case RAFT2_STATE_CANDIDATE:
	/* election timeout - start new election */
	raft2_convert_candidate( &glob.cl[i] );
	break;
      case RAFT2_STATE_LEADER:
	/* send pings to keep term alive */
	raft2_send_pings( &glob.cl[i] );
	glob.cl[i].timeout = rpc_now() + glob.prop.term_low;
	raft2_cluster_set( &glob.cl[i] );
	break;
      }
    }
  }

  /* set timeout to min cluster timeout */
  to = iter->timeout;
  for( i = 0; i < glob.ncl; i++ ) {
    if( glob.cl[i].timeout < to ) to = glob.cl[i].timeout;
  }
  iter->timeout = to;
}


static struct rpc_iterator raft2_iter =
  {
   NULL,
   0,
   1000,
   raft2_iter_cb,
   NULL
  };

void raft2_register( void ) {
  raft2_open();
  glob.ncl = raft2_cluster_list( glob.cl, RAFT2_MAX_CLUSTER );
  
  rpc_program_register( &raft_prog );
  rpc_iterator_register( &raft2_iter );
}
