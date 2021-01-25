

/*
 * Needs a rethink:
 * - Raft should only be needed to maintain a consistent view across the cluster 
 * of which hosts hold what locks. 
 * - Nodes need to be able to wait to acquire a lock 
 * - Does the cluster-wide view need to know about locks that are waiting to be 
 * acquired? 
 * - The cluster wide view provides the information for a finite state machine. 
 * So every node in the cluster should evaluate to the same states ie all state change 
 * info should come from the cluster.
 * 
 * - Submit request to acquire a lock. This includes the lockid, resid etc. The caller 
 * also provides a callback function, but this doesn't form part of the state change command.
 * So we must save this separately (volatile, local only). 
 * - When a lock command is processed, check if the lock can be immediately acquired. If 
 * so then set to locked state. If not then set blocked state. 
 * If we hold a notification callback then invoke it.
 * 
 * Q: how is this any different from what we already have? 
 * At the moment we have entries which are not yet processed by the state machine (WAIT)
 * and information which is not submitted as part of the command (cb/cxt) but this isn't really 
 * much different from holding them in a separate queue.
 *
 * - Submit lock command to raft.
 * - Add local entry in state wait with cb/cxt 
 * - When command processed change state to locked if possible or blocked otherwise. 
 * If changed to locked invoke callback.
 * - When command processed to release, check for existing lock in blocked state. If found 
 * then acquire that one, invoking callback it required.
 * - We do we need to ensure that ordering is the same across the cluster? Yes we do. 
 * But we can't ensure that if we start inserting things locally. This is the advantage of 
 * storing that info separately.
 * Under what conditions should a lock be released? There needs to be a mechanism to release 
 * locks which have been acquired by hosts which have later gone offline. 
 * So all we need is for each lock to know which host acquired it, and some mechanism to know 
 * if a host has gone offline or stopped responding. 
 * We can have a heartbeat command which keeps all locks held by that host alive. All hosts 
 * which think they have locks should be calling heartbeat commands until the lock is released.
 * Once heartbeats stop being received from that host then the leader will publish a command 
 * to release all locks held by that host.
 * 
 * Commands: 
 * acquire lock (lockid,hostid,resid,mode) -> allocate entry in state ex/sh/blocked depending on conditions 
 * release lock (lockid) -> delete entry 
 * heartbeat (hostid) -> increment host seq 
 * release all (hostid) -> delete all locks held by hostid
 *
 * Q: do we want a separate heartbeating service that is possibly part of raft? Maybe... but this is easier 
 */

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

static log_deflogger(dlm_log,"DLM")

#define DLM_MAX_LOCK 512
#define DLM_MAX_HOST RAFT_MAX_MEMBER
#define DLM_HBTIMEOUT 5000
  
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

  int nlock;
  struct dlm_lock lock[DLM_MAX_LOCK];   /* lock database */
  
  int nlockcxt;
  struct dlm_lockcxt lockcxt[DLM_MAX_LOCK];  /* queued locks waiting to be acquired */

  int nhost;
  struct dlm_host host[DLM_MAX_HOST];   /* host heartbeat seqs */
} glob;


static void lockcxt_invoke( uint64_t lockid ) {
  int i;
  struct dlm_lockcxt *lcxt;
  lcxt = glob.lockcxt;
  for( i = 0; i < glob.nlockcxt; i++, lcxt++ ) {
    if( lcxt->lockid == lockid ) {
      if( lcxt->cb ) lcxt->cb( lockid, lcxt->cxt );
      if( i != (glob.nlockcxt - 1) ) glob.lockcxt[i] = glob.lockcxt[glob.nlockcxt - 1];
      glob.nlockcxt--;
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
#define DLM_CMD_HEARTBEAT 3
#define DLM_CMD_RELEASEALL 4
  union {
    struct {
      char cookie[DLM_MAX_COOKIE];
    } lock;
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
    break;
  case DLM_CMD_LOCKEX:
  case DLM_CMD_LOCKSH:
    xdr_encode_fixed( xdr, (uint8_t *)cmd->u.lock.cookie, DLM_MAX_COOKIE );
    break;
  case DLM_CMD_HEARTBEAT:
    xdr_encode_uint64( xdr, cmd->u.hb.seq );
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
    break;
  case DLM_CMD_LOCKEX:
  case DLM_CMD_LOCKSH:
    sts = xdr_decode_fixed( xdr, (uint8_t *)cmd->u.lock.cookie, DLM_MAX_COOKIE );
    break;
  case DLM_CMD_HEARTBEAT:
    sts = xdr_decode_uint64( xdr, &cmd->u.hb.seq );
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

int dlm_acquire( uint64_t resid, int shared, char *cookie, uint64_t *lockid, dlm_donecb_t cb, void *cxt ) {
  /* request to acquire lock */
  int sts, i;
  uint64_t lid;
  struct dlm_command cmd;

  dlm_log( LOG_LVL_TRACE, "Acquire resid %"PRIx64" %s", resid, shared ? "Shared" : "Exclusive" );
  
  if( lockid ) *lockid = 0;
  
  if( !glob.ocount ) return -1;

  /* insert new entry in wait state */
  if( glob.nlockcxt >= DLM_MAX_LOCK ) {
    dlm_log( LOG_LVL_ERROR, "Out of lockcxt descriptors" );
    return -1;
  }
  
  i = glob.nlockcxt;
  memset( &glob.lockcxt[i], 0, sizeof(glob.lockcxt[i]) );
  sec_rand( (char *)&lid, sizeof(lid) );
  while( lock_by_lockid( lid ) ) {
    sec_rand( (char *)&lid, sizeof(lid) );
  }
  
  glob.lockcxt[i].lockid = lid;
  glob.lockcxt[i].cb = cb;
  glob.lockcxt[i].cxt = cxt;
  glob.nlockcxt++;
  
  /* submit raft command to acquire lock */
  memset( &cmd, 0, sizeof(cmd) );
  cmd.cmd = shared ? DLM_CMD_LOCKSH : DLM_CMD_LOCKEX;
  cmd.lockid = lid;
  cmd.hostid = hostreg_localid();
  cmd.resid = resid;
  if( cookie ) memcpy( cmd.u.lock.cookie, cookie, DLM_MAX_COOKIE );

  sts = dlm_command_publish( &cmd );
  if( sts ) {
    glob.nlockcxt--;
  }
  
  if( lockid ) *lockid = lid;
  
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
  if( !sts ) lcxt->state = DLM_RELEASE;

  return sts;
}

static void dlm_iter_cb( struct rpc_iterator *iter ) {
  /* 
   * If any locks held locally then publish heartbeat command. 
   * If leader then check seqnos of other hosts are being incremented. If not then
   * publish releaseall command
   */

  int i, sts;
  uint64_t hostid;
  struct dlm_lock *lcxt;
  struct dlm_host *host;
  struct dlm_command cmd;
  struct raft_cluster cl;
  
  hostid = hostreg_localid();
  lcxt = glob.lock;
  for( i = 0; i < glob.nlock; i++ ) {
    if( (lcxt->hostid == hostid) && ((lcxt->state == DLM_EX) || (lcxt->state == DLM_SH)) ) {
      memset( &cmd, 0, sizeof(cmd) );
      cmd.cmd = DLM_CMD_HEARTBEAT;
      cmd.hostid = hostid;
      dlm_command_publish( &cmd );
      break;
    }
  }
  
  sts = raft_cluster_by_clid( glob.raftclid, &cl );
  if( !sts && (cl.state == RAFT_STATE_LEADER) ) {
    /* check hosts are still heartbeating */
    host = glob.host;
    for( i = 0; i < glob.nhost; i++ ) {
      if( lock_by_hostid( host->hostid ) && (rpc_now() > (host->lasthb + DLM_HBTIMEOUT)) ) {
	dlm_log( LOG_LVL_INFO, "Host %"PRIx64" has locks and stopped heartbeating - releasing all locks", host->hostid );
	memset( &cmd, 0, sizeof(cmd) );
	cmd.cmd = DLM_CMD_RELEASEALL;
	cmd.hostid = host->hostid;
	dlm_command_publish( &cmd );
      }
    }
  }
  
}


static void dlm_command( struct raft_app *app, struct raft_cluster *cl, uint64_t cmdseq, char *buf, int len ) {
  int sts, i, lockstate;
  struct xdr_s xdr;
  struct dlm_command cmd;
  struct dlm_lockcxt *lcxt;
  struct dlm_lock *lockp;
  struct dlm_host *host;
  
  xdr_init( &xdr, (uint8_t *)buf, len );
  sts = dlm_decode_command( &xdr, &cmd );
  if( sts ) {
    dlm_log( LOG_LVL_ERROR, "Failed to decode command" );
    return;
  }

  dlm_log( LOG_LVL_DEBUG, "Command %s LockID %"PRIx64" HostID %"PRIx64" ResID %"PRIx64"",
	   cmd.cmd == DLM_CMD_LOCKEX ? "LockEX" :
	   cmd.cmd == DLM_CMD_LOCKSH ? "LockSH" :
	   cmd.cmd == DLM_CMD_HEARTBEAT ? "Heartbeat" :
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
	memcpy( lockp->cookie, cmd.u.lock.cookie, DLM_MAX_COOKIE );
	glob.nlock++;	
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
	memcpy( lockp->cookie, cmd.u.lock.cookie, DLM_MAX_COOKIE );
	glob.nlock++;

	/* invoke callback if found */
	lockcxt_invoke( cmd.lockid );
      }
    }
    break;
  case DLM_CMD_RELEASE:
    /* Lookup lock for this lockid and delete it */
    lockp = glob.lock;
    lockstate = 0;
    for( i = 0; i < glob.nlock; i++, lockp++ ) {
      if( lockp->lockid == cmd.lockid ) {
	if( lockp->state == DLM_EX ) lockstate = 1;
	else if( lockp->state == DLM_SH ) lockstate = 2;
	
	if( i != (glob.nlock - 1) ) glob.lock[i] = glob.lock[glob.nlock - 1];
	glob.nlock--;
	break;
      }
    }

    lcxt = glob.lockcxt;
    for( i = 0; i < glob.nlockcxt; i++, lcxt++ ) {
      if( lcxt->lockid == cmd.lockid ) {
	if( i != (glob.nlockcxt - 1) ) glob.lockcxt[i] = glob.lockcxt[glob.nlockcxt - 1];
	glob.nlockcxt--;
	break;
      }
    }

    if( lockstate == 1 ) {
      int lockacquired = 0;
      
      /* 
       * released an exclusive lock - look for a pending exclusive lock. if found then acquire it.
       * if not found, then look for a pending shared lock. if found then acquire it.
       */
      lockp = glob.lock;
      for( i = 0; i < glob.nlock; i++, lockp++ ) {
	if( (lockp->resid == cmd.resid) && (lockp->state == DLM_BLOCKEX) ) {
	  dlm_log( LOG_LVL_TRACE, "Acquiring blocked exclusive lock %"PRIx64"", lockp->lockid );
	  lockp->state = DLM_EX;

	  lockcxt_invoke( lockp->lockid );
	  lockacquired = 1;
	  break;
	}
      }

      if( !lockacquired ) {
	lockp = glob.lock;
	for( i = 0; i < glob.nlock; i++, lockp++ ) {
	  if( (lockp->resid == cmd.resid) && (lockp->state == DLM_BLOCKSH) ) {
	    dlm_log( LOG_LVL_TRACE, "Acquiring blocked shared lock %"PRIx64"", lockp->lockid );
	    lockp->state = DLM_SH;
	    lockcxt_invoke( lockp->lockid );
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
	if( lockp->resid == cmd.resid ) {
	  if( lockp->state == DLM_SH ) {
	    foundp = 1;
	  } else if( lockp->state == DLM_BLOCKEX ) {
	    if( !exlockp ) exlockp = lockp;
	  }
	}
      }

      if( !foundp && exlockp ) {
	dlm_log( LOG_LVL_TRACE, "Acquring blocked exclusive lock %"PRIx64"", exlockp->lockid );
	exlockp->state = DLM_EX;
	lockcxt_invoke( lockp->lockid );
      }
      
    }
     
    break;
  case DLM_CMD_HEARTBEAT:
    host = host_by_hostid( cmd.hostid );
    if( host ) {
      host->seq = cmd.u.hb.seq;
      host->lasthb = rpc_now();
    } else {
      i = glob.nhost;
      if( i >= DLM_MAX_HOST ) {
	dlm_log( LOG_LVL_ERROR, "Out of host descriptors" );
      } else {
	host = &glob.host[i];
	host->hostid = cmd.hostid;
	host->seq = cmd.u.hb.seq;
	host->lasthb = rpc_now();
	glob.nhost++;
      }
    }
    break;
  case DLM_CMD_RELEASEALL:
    i = 0;
    while( i < glob.nlock ) {
      if( glob.lock[i].hostid == cmd.hostid ) {
	if( i != (glob.nlock - 1) ) glob.lock[i] = glob.lock[glob.nlock - 1];
	glob.nlock--;
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
  sts = raft_snapshot_save( cl->clid, term, seq, 0, (char *)&glob.nlock, 4 );
  if( sts ) dlm_log( LOG_LVL_ERROR, "Snapshot save failed offset=0" );
  sts = raft_snapshot_save( cl->clid, term, seq, 4, (char *)glob.lock, sizeof(glob.lock) );
  if( sts ) dlm_log( LOG_LVL_ERROR, "Snapshot save failed offset=4" );
  sts = raft_snapshot_save( cl->clid, term, seq, -1, NULL, 0 );
  if( sts ) dlm_log( LOG_LVL_ERROR, "Snapshot save failed final block" );
}

static void dlm_snapload( struct raft_app *app, struct raft_cluster *cl, char *buf, int len ) {
  uint32_t nlock;
  int i;
  
  if( len != (4 + sizeof(glob.lock)) ) {
    dlm_log( LOG_LVL_ERROR, "Snapload Bad snapshot size %u", len );
    return;
  }

  nlock = *((uint32_t *)buf);
  if( nlock > DLM_MAX_LOCK ) {
    dlm_log( LOG_LVL_ERROR, "Snapload Bad nlock" );
    return;
  }

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
    xdr_encode_fixed( &inc->xdr, (uint8_t *)glob.lock[i].cookie, DLM_MAX_COOKIE );
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int dlm_proc_acquire( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t resid, lockid;
  int shared;
  char cookie[DLM_MAX_COOKIE];
  
  sts = xdr_decode_uint64( &inc->xdr, &resid );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &shared );
  if( !sts ) sts = xdr_decode_fixed( &inc->xdr, (uint8_t *)cookie, DLM_MAX_COOKIE );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  dlm_acquire( resid, shared, cookie, &lockid, NULL, NULL );
  
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

static struct rpc_proc dlm_procs[] = {
  { 0, dlm_proc_null },
  { 1, dlm_proc_list },
  { 2, dlm_proc_acquire },
  { 3, dlm_proc_release },    
  { 0, NULL }
};

static struct rpc_version dlm_vers = {
  NULL, DLM_RPC_VERS, dlm_procs
};

static uint32_t dlm_prog_auths[] = { RPC_AUTH_HRAUTH, 0 };
static struct rpc_program dlm_prog = {
    NULL, DLM_RPC_PROG, &dlm_vers, dlm_prog_auths
};

int dlm_open( void ) {
  /* open database, register raft app etc */

  if( glob.ocount > 0 ) {
    glob.ocount++;
    return 0;
  }

  /* register raft etc */
  rpc_iterator_register( &dlm_iter );
  raft_app_register( &dlm_app );
  rpc_program_register( &dlm_prog );
  
  glob.raftclid = raft_clid_by_appid( DLM_RPC_PROG );
  if( !glob.raftclid ) {
    dlm_log( LOG_LVL_ERROR, "No DLM cluster" );
    return -1;
  }
  
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

