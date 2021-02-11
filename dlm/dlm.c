
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <fju/mmf.h>
#include <fju/dlm.h>
#include <fju/rpc.h>
#include <fju/raft.h>
#include <fju/log.h>
#include <fju/programs.h>
#include <inttypes.h>
#include <fju/hostreg.h>
#include <fju/sec.h>
#include <fju/rpcd.h>
#include <fju/hrauth.h>
#include <fju/dmb.h>
#include <fju/dmb-category.h>

static log_deflogger(dlm_log,"DLM")

#define DLM_MAX_LOCK 512
#define DLM_MAX_HOST RAFT_MAX_MEMBER
#define DLM_HBTIMEOUT 5000
#define DLM_MSGID_HEARTBEAT 0x00000201
  
struct dlm_lockcxt {
  uint64_t lockid;
  dlm_donecb_t cb;
  void *cxt;
};

struct dlm_host {
  uint64_t hostid;
  uint64_t seq;
  uint64_t lasthb;
};

static struct {
  uint32_t ocount;
  uint64_t raftclid;
  uint64_t hbseq;
  int releaseownlocks;
  
  int nlock;
  struct dlm_lock lock[DLM_MAX_LOCK];   /* lock database */
  
  int nlockcxt;
  struct dlm_lockcxt lockcxt[DLM_MAX_LOCK];  /* queued locks waiting to be acquired */

  int nhost;
  struct dlm_host host[DLM_MAX_HOST];   /* host heartbeat seqs */
} glob;


static void lockcxt_invoke( uint64_t lockid, dlm_lockstat_t lockstat, int remove ) {
  int i;
  struct dlm_lockcxt *lcxt;
  lcxt = glob.lockcxt;
  for( i = 0; i < glob.nlockcxt; i++, lcxt++ ) {
    if( lcxt->lockid == lockid ) {
      if( lcxt->cb ) lcxt->cb( lockid, lockstat, lcxt->cxt );
      if( remove ) {
	if( i != (glob.nlockcxt - 1) ) glob.lockcxt[i] = glob.lockcxt[glob.nlockcxt - 1];
	glob.nlockcxt--;
      }
      break;
    }
  }
}

static struct dlm_lock *lock_by_lockid( uint64_t lockid ) {
  int i;
  struct dlm_lock *lcxt;
  lcxt = glob.lock;
  for( i = 0; i < glob.nlock; i++, lcxt++ ) {
    if( lcxt->lockid == lockid ) return lcxt;
  }
  return NULL;
}
static struct dlm_lock *lock_by_hostid( uint64_t hostid ) {
  int i;
  struct dlm_lock *lcxt;
  lcxt = glob.lock;
  for( i = 0; i < glob.nlock; i++, lcxt++ ) {
    if( lcxt->hostid == hostid ) return lcxt;
  }
  return NULL;
}

static struct dlm_lock *lock_wouldblock( uint64_t resid, int shared ) {
  int i;
  struct dlm_lock *lcxt;
  
  if( shared ) {
    /* acquiring a shared lock is blocked if resource locked exclusively OR an exclusive lock request is blocked */
    lcxt = glob.lock;
    for( i = 0; i < glob.nlock; i++, lcxt++ ) {
      if( (lcxt->resid == resid) && ((lcxt->state == DLM_EX) || (lcxt->state == DLM_BLOCKEX)) ) {
	return lcxt;
      }
    }
  } else {
    /* exclusive locks are blocked if resource held exclusively OR shared */
    lcxt = glob.lock;
    for( i = 0; i < glob.nlock; i++, lcxt++ ) {
      if( (lcxt->resid == resid) && ((lcxt->state == DLM_EX) || (lcxt->state == DLM_SH)) ) {
	return lcxt;
      }
    }
  }
  return NULL;
}

static struct dlm_host *host_by_hostid( uint64_t hostid ) {
  int i;
  for( i = 0; i < glob.nhost; i++ ) {
    if( glob.host[i].hostid == hostid ) return &glob.host[i];
  }
  return NULL;
}



static void dlm_iter_cb( struct rpc_iterator *iter );
static RPC_ITERATOR(dlm_iter,1000,dlm_iter_cb);

static void dlm_command( struct raft_app *app, struct raft_cluster *cl, uint64_t cmdseq, char *buf, int len );
static void dlm_snapsave( struct raft_app *app, struct raft_cluster *cl, uint64_t term, uint64_t seq );
static void dlm_snapload( struct raft_app *app, struct raft_cluster *cl, char *buf, int len );
static struct dlm_lock *lock_by_lockid( uint64_t lockid );

static struct raft_app dlm_app = {
    NULL,
    DLM_RPC_PROG,
    dlm_command,
    dlm_snapsave,
    dlm_snapload,				  
};
  


int dlm_list( struct dlm_lock *dl, int ndl ) {
  /* list locks */
  int i;
  
  for( i = 0; i < glob.nlock; i++ ) {
    if( i < ndl ) {
      dl[i] = glob.lock[i];
    }
  }
  
  return glob.nlock;
}

int dlm_lock_by_lockid( uint64_t lockid, struct dlm_lock *lock ) {
  int i;
  struct dlm_lock *lcxt;
  
  lcxt = glob.lock;
  for( i = 0; i < glob.nlock; i++, lcxt++ ) {
    if( lcxt->lockid == lockid ) {
      if( lock ) *lock = *lcxt;
      return 0;
    }
  }
  
  return -1;
}


struct dlm_command {  
  uint64_t lockid;
  uint64_t hostid;
  uint64_t resid;
  uint32_t cmd;
#define DLM_CMD_RELEASE 0
#define DLM_CMD_LOCKEX  1
#define DLM_CMD_LOCKSH  2
#define DLM_CMD_RESERVED 3
#define DLM_CMD_RELEASEALL 4
  union {
    struct {
      uint64_t seq;
    } hb;
  } u;
};

static int dlm_encode_command( struct xdr_s *xdr, struct dlm_command *cmd ) {
  xdr_encode_uint64( xdr, cmd->lockid );
  xdr_encode_uint64( xdr, cmd->hostid );
  xdr_encode_uint64( xdr, cmd->resid );  
  xdr_encode_uint32( xdr, cmd->cmd );
  switch( cmd->cmd ) {
  case DLM_CMD_RELEASE:
  case DLM_CMD_RELEASEALL:
    break;
  case DLM_CMD_LOCKEX:
  case DLM_CMD_LOCKSH:
    break;
  }
  return 0;
}
static int dlm_decode_command( struct xdr_s *xdr, struct dlm_command *cmd ) {
  int sts;
  sts = xdr_decode_uint64( xdr, &cmd->lockid );
  if( !sts ) sts = xdr_decode_uint64( xdr, &cmd->hostid );
  if( !sts ) sts = xdr_decode_uint64( xdr, &cmd->resid );
  if( !sts ) sts = xdr_decode_uint32( xdr, &cmd->cmd );
  if( sts ) return -1;
  switch( cmd->cmd ) {
  case DLM_CMD_RELEASE:
  case DLM_CMD_RELEASEALL:
    break;
  case DLM_CMD_LOCKEX:
  case DLM_CMD_LOCKSH:
    break;
  case DLM_CMD_RESERVED:
    break;
  default:
    return -1;
  }
  if( sts ) return sts;
  
  return 0;
}

static int dlm_command_publish( struct dlm_command *cmd ) {
  struct xdr_s xdr;
  char cmdbuf[64];
  int sts;
  
  xdr_init( &xdr, (uint8_t *)cmdbuf, sizeof(cmdbuf) );
  dlm_encode_command( &xdr, cmd );
  sts = raft_command( glob.raftclid, (char *)xdr.buf, xdr.offset, NULL );
  if( sts ) {
    dlm_log( LOG_LVL_ERROR, "Failed to publish command" );
  }
  
  return sts;
}

static void call_acquire_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t hostid, lockid;

  hostid = hcallp->hostid;
  lockid = hcallp->cxt2;
  
  if( !res ) {
    dlm_log( LOG_LVL_ERROR, "call_acquire_cb timeout lockid=%"PRIx64"", lockid );
    lockcxt_invoke( lockid, DLM_LOCKSTAT_FAIL, 1 );
    return;
  }

}

static int dlm_call_acquire( uint64_t hostid, uint64_t resid, int shared, uint64_t lid ) {
  char argbuf[64];
  struct xdr_s args;
  struct hrauth_call hcall;
  int sts;
  
  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  xdr_encode_uint64( &args, resid );
  xdr_encode_boolean( &args, shared );
  xdr_encode_uint64( &args, lid );  
  xdr_encode_uint64( &args, hostreg_localid() );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = DLM_RPC_PROG;
  hcall.vers = DLM_RPC_VERS;
  hcall.proc = 4; /* acquire_internal */
  hcall.timeout = 500;
  hcall.service = HRAUTH_SERVICE_PRIV;
  hcall.donecb = call_acquire_cb;
  hcall.cxt2 = lid;
  sts = hrauth_call_async( &hcall, &args, 1 );
  if( sts ) {
    dlm_log( LOG_LVL_ERROR, "dlm_call_acquire failed" );
    return -1;
  }

  return 0;
}

static int dlm_acquire2( uint64_t resid, int shared, uint64_t lid, uint64_t hostid, uint64_t *lockidp, dlm_donecb_t cb, void *cxt );

int dlm_acquire( uint64_t resid, int shared, uint64_t *lockid, dlm_donecb_t cb, void *cxt ) {
  return dlm_acquire2( resid, shared, 0, 0, lockid, cb, cxt );
}

static int dlm_acquire2( uint64_t resid, int shared, uint64_t lid, uint64_t hostid, uint64_t *lockidp, dlm_donecb_t cb, void *cxt ) {
  /* request to acquire lock */
  int sts, i;
  struct dlm_command cmd;
  struct raft_cluster cl;

  if( lockidp ) *lockidp = 0;
  
  sts = raft_cluster_by_clid( glob.raftclid, &cl );
  if( sts ) return -1;

  if( cl.state == RAFT_STATE_CANDIDATE ) return -1;
  
  dlm_log( LOG_LVL_TRACE, "Acquire resid %"PRIx64" %s", resid, shared ? "Shared" : "Exclusive" );
  
  if( !glob.ocount ) return -1;
  
  /* insert new entry in wait state */
  if( glob.nlockcxt >= DLM_MAX_LOCK ) {
    dlm_log( LOG_LVL_ERROR, "Out of lockcxt descriptors" );
    return -1;
  }

  /* choose a lockid */
  i = glob.nlockcxt;
  memset( &glob.lockcxt[i], 0, sizeof(glob.lockcxt[i]) );
    
  if( lid ) {
    if( lock_by_lockid( lid ) ) {
      dlm_log( LOG_LVL_ERROR, "Lock %"PRIx64" already exists", lid );
      return -1;
    }
  } else {
    sec_rand( (char *)&lid, sizeof(lid) );
    while( lock_by_lockid( lid ) ) {
      sec_rand( (char *)&lid, sizeof(lid) );
    }
  }
  
  glob.lockcxt[i].lockid = lid;
  glob.lockcxt[i].cb = cb;
  glob.lockcxt[i].cxt = cxt;
  glob.nlockcxt++;

  if( cl.state == RAFT_STATE_LEADER ) {
  
    /* submit raft command to acquire lock */
    memset( &cmd, 0, sizeof(cmd) );
    cmd.cmd = shared ? DLM_CMD_LOCKSH : DLM_CMD_LOCKEX;
    cmd.lockid = lid;
    cmd.hostid = hostid ? hostid : hostreg_localid();
    cmd.resid = resid;
    
    sts = dlm_command_publish( &cmd );
    if( sts ) {
      dlm_log( LOG_LVL_ERROR, "dlm_command_publish failed" );
    }
  } else {
    /* send rpc to leader */
    sts = dlm_call_acquire( cl.leaderid, resid, shared, lid );
    if( sts ) {
      dlm_log( LOG_LVL_ERROR, "dlm_call_acquire failed" );
    }
  }
  
  if( sts ) {
    lid = 0;
    glob.nlockcxt--;
  } 
 
  if( lockidp ) *lockidp = lid;
  
  return sts;
}

int dlm_release( uint64_t lockid ) {
  /* submit raft command to relase lock */
  int sts;
  struct dlm_command cmd;
  struct dlm_lock *lcxt;
  
  if( !glob.ocount ) return -1;

  dlm_log( LOG_LVL_TRACE, "Release %"PRIx64"", lockid );
  
  lcxt = lock_by_lockid( lockid );
  if( !lcxt ) return -1;
  
  /* submit raft command to release lock */
  memset( &cmd, 0, sizeof(cmd) );
  cmd.cmd = DLM_CMD_RELEASE;
  cmd.lockid = lockid;
  cmd.hostid = hostreg_localid();
  cmd.resid = lcxt->resid;

  sts = dlm_command_publish( &cmd );
  //  if( !sts ) lcxt->state = DLM_RELEASE;

  return sts;
}

static void dlm_iter_cb( struct rpc_iterator *iter ) {
  /* 
   * If any locks held locally then publish heartbeat command. 
   * If leader then check seqnos of other hosts are being incremented. If not then
   * publish releaseall command
   */
  int i, sts;
  struct dlm_host *host;
  struct dlm_command cmd;
  struct raft_cluster cl;
  struct xdr_s xdr;
  char xdrbuf[8];

  xdr_init( &xdr, (uint8_t *)xdrbuf, sizeof(xdrbuf) );
  xdr_encode_uint64( &xdr, glob.hbseq );
  dmb_publish( DLM_MSGID_HEARTBEAT, DMB_REMOTE, xdrbuf, xdr.offset, NULL );
  
  sts = raft_cluster_by_clid( glob.raftclid, &cl );
  if( !sts && (cl.state == RAFT_STATE_LEADER) ) {
    /* check hosts are still heartbeating */
    host = glob.host;
    for( i = 0; i < glob.nhost; i++, host++ ) {
      if( host->hostid == hostreg_localid() ) {
	if( glob.releaseownlocks ) {
	  dlm_log( LOG_LVL_INFO, "Releasing own locks" );
	  memset( &cmd, 0, sizeof(cmd) );
	  cmd.cmd = DLM_CMD_RELEASEALL;
	  cmd.hostid = host->hostid;
	  dlm_command_publish( &cmd );
	  
	  glob.releaseownlocks = 0;
	}
      } else if( host->lasthb && lock_by_hostid( host->hostid ) && (rpc_now() > (host->lasthb + DLM_HBTIMEOUT)) ) {
	dlm_log( LOG_LVL_INFO, "Host %"PRIx64" has locks and stopped heartbeating - releasing all locks", host->hostid );
	memset( &cmd, 0, sizeof(cmd) );
	cmd.cmd = DLM_CMD_RELEASEALL;
	cmd.hostid = host->hostid;
	dlm_command_publish( &cmd );
      }
    }
  }
  
}


static void release_lock( uint64_t lockid ) {
  int i, lockstate;
  struct dlm_lock *lockp;
  struct dlm_lockcxt *lcxt;
  uint64_t resid;
  
  /* Lookup lock for this lockid and delete it */
  lockp = glob.lock;
  lockstate = 0;
  resid = 0;
  for( i = 0; i < glob.nlock; i++, lockp++ ) {
    if( lockp->lockid == lockid ) {
      if( lockp->state == DLM_EX ) lockstate = 1;
      else if( lockp->state == DLM_SH ) lockstate = 2;
      dlm_log( LOG_LVL_TRACE, "Releasing lock %"PRIx64" lockstate=%u", lockid, lockstate );
      resid = lockp->resid;
			     
      if( i != (glob.nlock - 1) ) glob.lock[i] = glob.lock[glob.nlock - 1];
      glob.nlock--;
      break;
    }
  }
  
  lcxt = glob.lockcxt;
  for( i = 0; i < glob.nlockcxt; i++, lcxt++ ) {
    if( lcxt->lockid == lockid ) {
      if( i != (glob.nlockcxt - 1) ) glob.lockcxt[i] = glob.lockcxt[glob.nlockcxt - 1];
      glob.nlockcxt--;
      break;
    }
  }

  /* if resid=0 then we never found the corresponding lock */
  if( !resid ) return;

    
  if( lockstate == 1 ) {
    int lockacquired = 0;
      
    /* 
     * released an exclusive lock - look for a pending exclusive lock. if found then acquire it.
     * if not found, then look for a pending shared lock. if found then acquire it.
     */
    lockp = glob.lock;
    for( i = 0; i < glob.nlock; i++, lockp++ ) {
      if( (lockp->resid == resid) && (lockp->state == DLM_BLOCKEX) ) {
	dlm_log( LOG_LVL_TRACE, "Acquiring blocked exclusive lock %"PRIx64"", lockp->lockid );
	lockp->state = DLM_EX;
	
	lockcxt_invoke( lockp->lockid, DLM_LOCKSTAT_ACQUIRED, 1 );
	lockacquired = 1;
	break;
      }
    }

    if( !lockacquired ) {
      lockp = glob.lock;
      for( i = 0; i < glob.nlock; i++, lockp++ ) {
	if( (lockp->resid == resid) && (lockp->state == DLM_BLOCKSH) ) {
	  dlm_log( LOG_LVL_TRACE, "Acquiring blocked shared lock %"PRIx64"", lockp->lockid );
	  lockp->state = DLM_SH;
	  lockcxt_invoke( lockp->lockid, DLM_LOCKSTAT_ACQUIRED, 1 );
	}
      }
    }
    
    
  } else if( lockstate == 2 ) {
    struct dlm_lock *exlockp;
    int foundp;
    
    /* 
     * release a shared lock. Look for any other shared locks. If none found then 
     * look for a pending exclusive lock. If found then acquire it.
     */
    
    lockp = glob.lock;
    foundp = 0;
    exlockp = NULL;
    for( i = 0; i < glob.nlock; i++, lockp++ ) {
      if( lockp->resid == resid ) {
	if( lockp->state == DLM_SH ) {
	  foundp = 1;
	} else if( lockp->state == DLM_BLOCKEX ) {
	  if( !exlockp ) exlockp = lockp;
	}
      }
    }
    
    if( !foundp && exlockp ) {
      dlm_log( LOG_LVL_TRACE, "Acquiring blocked exclusive lock %"PRIx64"", exlockp->lockid );
      exlockp->state = DLM_EX;
      lockcxt_invoke( lockp->lockid, DLM_LOCKSTAT_ACQUIRED, 1 );
    }
    
  }
       
}

static void dlm_command( struct raft_app *app, struct raft_cluster *cl, uint64_t cmdseq, char *buf, int len ) {
  int sts, i;
  struct xdr_s xdr;
  struct dlm_command cmd;
  struct dlm_lock *lockp;
  
  xdr_init( &xdr, (uint8_t *)buf, len );
  sts = dlm_decode_command( &xdr, &cmd );
  if( sts ) {
    dlm_log( LOG_LVL_ERROR, "Failed to decode command" );
    return;
  }

  dlm_log( LOG_LVL_DEBUG, "Command Seq %"PRIu64" %s LockID %"PRIx64" HostID %"PRIx64" ResID %"PRIx64"",
	   cmdseq,
	   cmd.cmd == DLM_CMD_LOCKEX ? "LockEX" :
	   cmd.cmd == DLM_CMD_LOCKSH ? "LockSH" :
	   cmd.cmd == DLM_CMD_RELEASE ? "Release" :
	   cmd.cmd == DLM_CMD_RELEASEALL ? "ReleaseAll" : 
	   "Other",
	   cmd.lockid,
	   cmd.hostid,
	   cmd.resid );
  
  switch( cmd.cmd ) {
  case DLM_CMD_LOCKEX:
  case DLM_CMD_LOCKSH:
    lockp = lock_wouldblock( cmd.resid, cmd.cmd == DLM_CMD_LOCKEX ? 0 : 1 );
    if( lockp ) {
      dlm_log( LOG_LVL_INFO, "Lock %"PRIx64" block pending %"PRIx64"", cmd.lockid, lockp->lockid );
      i = glob.nlock;
      if( i >= DLM_MAX_LOCK ) {
	dlm_log( LOG_LVL_ERROR, "Out of lock descriptors" );
      } else {
	lockp = &glob.lock[i];
	memset( lockp, 0, sizeof(*lockp) );
	lockp->lockid = cmd.lockid;
	lockp->hostid = cmd.hostid;
	lockp->resid = cmd.resid;
	lockp->state = (cmd.cmd == DLM_CMD_LOCKEX) ? DLM_BLOCKEX : DLM_BLOCKSH;
	glob.nlock++;

	lockcxt_invoke( cmd.lockid, DLM_LOCKSTAT_BLOCKED, 0 );
      }
    } else {
      i = glob.nlock;
      if( i >= DLM_MAX_LOCK ) {
	dlm_log( LOG_LVL_ERROR, "Out of lock descriptors" );
      } else {
	dlm_log( LOG_LVL_TRACE, "Acquring lock %"PRIx64"", cmd.lockid );
	lockp = &glob.lock[i];
	memset( lockp, 0, sizeof(*lockp) );
	lockp->lockid = cmd.lockid;
	lockp->hostid = cmd.hostid;
	lockp->resid = cmd.resid;
	lockp->state = (cmd.cmd == DLM_CMD_LOCKEX) ? DLM_EX : DLM_SH;
	glob.nlock++;
  
	/* invoke callback if found */
	lockcxt_invoke( cmd.lockid, DLM_LOCKSTAT_ACQUIRED, 1 );
      }
    }
    break;
  case DLM_CMD_RELEASE:
    release_lock( cmd.lockid );
    break;
  case DLM_CMD_RELEASEALL:
    i = 0;
    while( i < glob.nlock ) {
      if( glob.lock[i].hostid == cmd.hostid ) {
	release_lock( glob.lock[i].lockid );
      } else {
	i++;
      }
    }

    break;
  default:
    dlm_log( LOG_LVL_ERROR, "Invalid command %u", cmd.cmd );
    return;
  }

  
}

static void dlm_snapsave( struct raft_app *app, struct raft_cluster *cl, uint64_t term, uint64_t seq ) {
  int sts;
  struct log_iov iov[2];

  iov[0].buf = (char *)&glob.nlock;
  iov[0].len = 4;
  iov[1].buf = (char *)glob.lock;
  iov[1].len = sizeof(glob.lock);
  sts = raft_snapshot_save( cl->clid, term, seq, iov, 2 );
  if( sts ) dlm_log( LOG_LVL_ERROR, "Snapshot save failed" );

}

static void dlm_snapload( struct raft_app *app, struct raft_cluster *cl, char *buf, int len ) {
  uint32_t nlock;
  int i;
  
  if( len != (4 + sizeof(glob.lock)) ) {
    dlm_log( LOG_LVL_ERROR, "Snapload Bad snapshot size %u != %u", len, sizeof(glob.lock) + 4 );
    return;
  }

  nlock = *((uint32_t *)buf);
  if( nlock > DLM_MAX_LOCK ) {
    dlm_log( LOG_LVL_ERROR, "Snapload Bad nlock" );
    return;
  }

  dlm_log( LOG_LVL_TRACE, "dlm_snapload len=%u nlocks=%u/%u", len, nlock, (len - 4) / DLM_MAX_LOCK );

  glob.nlock = nlock;
  memcpy( glob.lock, buf, sizeof(glob.lock) );
  for( i = 0; i < glob.nlock; i++ ) {
    dlm_log( LOG_LVL_DEBUG, "Snapload Lockid=%"PRIx64" HostID=%"PRIx64" State=%u ResID=%"PRIx64"",
	     glob.lock[i].lockid,
	     glob.lock[i].hostid,
	     glob.lock[i].state,
	     glob.lock[i].resid );
  }
}

static int dlm_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int dlm_proc_list( struct rpc_inc *inc ) {
  int handle, i;
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  for( i = 0; i < glob.nlock; i++ ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_uint64( &inc->xdr, glob.lock[i].lockid );
    xdr_encode_uint64( &inc->xdr, glob.lock[i].hostid );
    xdr_encode_uint64( &inc->xdr, glob.lock[i].resid );
    xdr_encode_uint32( &inc->xdr, glob.lock[i].state );
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int dlm_proc_acquire( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t resid, lockid;
  int shared;
  
  sts = xdr_decode_uint64( &inc->xdr, &resid );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &shared );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  sts = dlm_acquire( resid, shared, &lockid, NULL, NULL );
  dlm_log( LOG_LVL_TRACE, "dlm_proc_acquire resid=%"PRIx64" %s lockid=%"PRIx64" %s",
	   resid, shared ? "Shared" : "Exclusive", lockid, sts ? "Failed" : "Success" );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_uint64( &inc->xdr, lockid );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int dlm_proc_release( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t lockid;

  
  sts = xdr_decode_uint64( &inc->xdr, &lockid );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  dlm_release( lockid );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int dlm_proc_acquire_internal( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t resid, lockid, hostid;
  int shared;
  struct xdr_s res;
  uint8_t resbuf[16];
  
  sts = xdr_decode_uint64( &inc->xdr, &resid );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &shared );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &lockid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &hostid );  
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  sts = dlm_acquire2( resid, shared, lockid, hostid, &lockid, NULL, NULL );
  dlm_log( LOG_LVL_TRACE, "dlm_proc_acquire_internal resid=%"PRIx64" %s lockid=%"PRIx64" %s",
	   resid, shared ? "Shared" : "Exclusive", lockid, sts ? "Failed" : "Success" );
  
  xdr_init( &res, resbuf, sizeof(resbuf) );
  xdr_encode_uint64( &res, lockid );
  return hrauth_reply( inc, &res, 1 );
}


static struct rpc_proc dlm_procs[] = {
  { 0, dlm_proc_null },
  { 1, dlm_proc_list },
  { 2, dlm_proc_acquire },
  { 3, dlm_proc_release },
  { 4, dlm_proc_acquire_internal },      
  { 0, NULL }
};

static struct rpc_version dlm_vers = {
  NULL, DLM_RPC_VERS, dlm_procs
};

static uint32_t dlm_prog_auths[] = { RPC_AUTH_HRAUTH, 0 };
static struct rpc_program dlm_prog = {
    NULL, DLM_RPC_PROG, &dlm_vers, dlm_prog_auths
};


static void dlm_subscriber( uint64_t hostid, uint64_t seq, uint32_t msgid, char *buf, int size ) {
  int i, sts;
  struct dlm_host *host;
  uint64_t hseq;
  struct xdr_s xdr;
  
  switch( msgid ) {
  case DLM_MSGID_HEARTBEAT:
    xdr_init( &xdr, (uint8_t *)buf, size );
    sts = xdr_decode_uint64( &xdr, &hseq );
    if( sts ) hseq = 0;    
    
    dlm_log( LOG_LVL_TRACE, "Heartbeat %"PRIx64" Seq %"PRIu64"", hostid, hseq );
    host = host_by_hostid( hostid );
    if( host ) {
      if( host->seq != hseq ) {
	struct raft_cluster cl;
	if( !raft_cluster_by_clid( glob.raftclid, &cl ) && (cl.state == RAFT_STATE_LEADER) ) {
	  struct dlm_command cmd;
	  
	  dlm_log( LOG_LVL_INFO, "Host %"PRIx64" seq changed %"PRIu64" -> %"PRIu64"", hostid, host->seq, hseq );
	  memset( &cmd, 0, sizeof(cmd) );
	  cmd.cmd = DLM_CMD_RELEASEALL;
	  cmd.hostid = hostid;
	  dlm_command_publish( &cmd );
	}
      }
      
      host->seq = hseq;
      host->lasthb = rpc_now();
    } else {
      i = glob.nhost;
      if( i >= DLM_MAX_HOST ) {
	dlm_log( LOG_LVL_ERROR, "Out of host descriptors" );
      } else {
	host = &glob.host[i];
	host->hostid = hostid;
	host->seq = seq;
	host->lasthb = rpc_now();
	glob.nhost++;
      }
    }
    break;
  }
    
}
static DMB_SUBSCRIBER(dlm_sc,DLM_MSGID_HEARTBEAT,dlm_subscriber);

int dlm_open( void ) {
  /* open database, register raft app etc */

  if( glob.ocount > 0 ) {
    glob.ocount++;
    return 0;
  }

  glob.hbseq = time( NULL );
  
  /* register raft etc */
  rpc_iterator_register( &dlm_iter );
  raft_app_register( &dlm_app );
  rpc_program_register( &dlm_prog );
  dmb_subscribe( &dlm_sc );
  
  glob.raftclid = raft_clid_by_appid( DLM_RPC_PROG );
  if( !glob.raftclid ) {
    uint64_t clid;
    struct raft_cluster cl, dlmcl;
    int i, sts;
    
    dlm_log( LOG_LVL_ERROR, "No DLM cluster" );

    clid = raft_clid_by_appid( RAFT_RPC_PROG );
    if( !clid ) {
      dlm_log( LOG_LVL_ERROR, "No raft cluster to clone" );
      return -1;
    }

    memset( &dlmcl, 0, sizeof(dlmcl) );
    dlmcl.appid = DLM_RPC_PROG;
    strcpy( dlmcl.cookie, "dlm" );
    dlmcl.nmember = cl.nmember;
    for( i = 0; i < cl.nmember; i++ ) {
      dlmcl.member[i].hostid = cl.member[i].hostid;
    }
    sts = raft_cluster_set( &dlmcl );
    if( sts ) {
      dlm_log( LOG_LVL_ERROR, "Failed to add DLM cluster" );
      return -1;
    }

    glob.raftclid = dlmcl.clid;
  }

  raft_replay( glob.raftclid );

  glob.releaseownlocks = 1;
  glob.ocount = 1;
  return 0;
}

int dlm_close( void ) {
  
  if( !glob.ocount ) return -1;
  
  glob.ocount--;
  if( glob.ocount > 0 ) return 0;

  memset( &glob, 0, sizeof(glob) );
  
  return 0;
}

