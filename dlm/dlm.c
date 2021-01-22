
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

struct dlm_lockcxt {
  struct dlm_lock lock;
  dlm_donecb_t cb;
  void *cxt;
  uint32_t prevseq;
};

static struct {
  uint32_t ocount;
  uint64_t raftclid;
  struct mmf_s mmf;
  int nlock;
  struct dlm_lockcxt lock[DLM_MAX_LOCK];
} glob;

static void dlm_iter_cb( struct rpc_iterator *iter );
static RPC_ITERATOR(dlm_iter,1000,dlm_iter_cb);

static void dlm_command( struct raft_app *app, struct raft_cluster *cl, uint64_t cmdseq, char *buf, int len );
static void dlm_snapsave( struct raft_app *app, struct raft_cluster *cl, uint64_t term, uint64_t seq );
static void dlm_snapload( struct raft_app *app, struct raft_cluster *cl, char *buf, int len );
static struct dlm_lockcxt *lock_by_lockid( uint64_t lockid, int *idx );

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
      dl[i] = glob.lock[i].lock;
    }
  }
  
  return glob.nlock;
}

int dlm_lock_by_lockid( uint64_t lockid, struct dlm_lock *lock ) {
  int i;
  for( i = 0; i < glob.nlock; i++ ) {
    if( glob.lock[i].lock.lockid == lockid ) {
      if( lock ) *lock = glob.lock[i].lock;
      return 0;
    }
  }
  
  return -1;
}

struct dlm_command {
  uint64_t lockid;
  uint64_t resid;
  uint32_t cmd;
#define DLM_CMD_RELEASE 0
#define DLM_CMD_LOCKEX  1
#define DLM_CMD_LOCKSH  2
#define DLM_CMD_RENEW   3
  union {
    struct {
      char cookie[DLM_MAX_COOKIE];
    } lock;
    struct {
      uint32_t seq;
    } renew;
  } u;
};

static int dlm_encode_command( struct xdr_s *xdr, struct dlm_command *cmd ) {
  xdr_encode_uint64( xdr, cmd->lockid );
  xdr_encode_uint64( xdr, cmd->resid );
  xdr_encode_uint32( xdr, cmd->cmd );
  switch( cmd->cmd ) {
  case DLM_CMD_RELEASE:
    break;
  case DLM_CMD_LOCKEX:
  case DLM_CMD_LOCKSH:
    xdr_encode_fixed( xdr, (uint8_t *)cmd->u.lock.cookie, DLM_MAX_COOKIE );
    break;
  case DLM_CMD_RENEW:
    xdr_encode_uint32( xdr, cmd->u.renew.seq );
    break;
  }
  return 0;
}
static int dlm_decode_command( struct xdr_s *xdr, struct dlm_command *cmd ) {
  int sts;
  sts = xdr_decode_uint64( xdr, &cmd->lockid );
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
  case DLM_CMD_RENEW:
    sts = xdr_decode_uint32( xdr, &cmd->u.renew.seq );
    break;
  default:
    return -1;
  }
  if( sts ) return sts;
  
  return 0;
}


int dlm_acquire( uint64_t resid, uint32_t mode, char *cookie, uint64_t *lockid, dlm_donecb_t cb, void *cxt ) {
  /* request to acquire lock */
  int sts, i;
  struct xdr_s xdr;
  char cmdbuf[64];
  uint64_t lid;
  struct dlm_command cmd;
  
  if( !glob.ocount ) return -1;
  if( (mode != DLM_EX) && (mode != DLM_SH) ) return -1;
  
  /* check database for lock conflicts, fail if not possible to acquire lock */
  for( i = 0; i < glob.nlock; i++ ) {
    if( glob.lock[i].lock.resid == resid ) {
      /* found an existing lock for this resource */
      return -1;
    }
  }
  /* insert new entry in wait state */
  if( glob.nlock >= DLM_MAX_LOCK ) return -1;
  
  i = glob.nlock;
  memset( &glob.lock[i], 0, sizeof(glob.lock[i]) );
  sec_rand( (char *)&lid, sizeof(lid) );
  glob.lock[i].lock.lockid = lid;
  glob.lock[i].lock.hostid = hostreg_localid();
  glob.lock[i].lock.mode = DLM_WAIT;
  glob.lock[i].cb = cb;
  glob.lock[i].cxt = cxt;
  glob.nlock++;
  
  /* submit raft command to acquire lock */
  memset( &cmd, 0, sizeof(cmd) );
  cmd.cmd = (mode == DLM_EX) ? DLM_CMD_LOCKEX : DLM_CMD_LOCKSH;
  cmd.lockid = lid;
  cmd.resid = resid;
  if( cookie ) memcpy( cmd.u.lock.cookie, cookie, DLM_MAX_COOKIE );
  
  xdr_init( &xdr, (uint8_t *)cmdbuf, sizeof(cmdbuf) );
  dlm_encode_command( &xdr, &cmd );
  sts = raft_command( glob.raftclid, (char *)xdr.buf, xdr.offset, NULL );

  if( lockid ) *lockid = lid;
  
  return sts;
}

int dlm_renew( uint64_t lockid ) {
  /* submit raft command to renew lock */
  struct xdr_s xdr;
  char cmdbuf[64];
  struct dlm_command cmd;
  struct dlm_lockcxt *lcxt;
  
  if( !glob.ocount ) return -1;

  lcxt = lock_by_lockid( lockid, NULL );
  if( !lcxt ) return -1;
  
  /* submit raft command to renew lock */
  memset( &cmd, 0, sizeof(cmd) );
  cmd.cmd = DLM_CMD_RENEW;
  cmd.lockid = lockid;
  cmd.resid = lcxt->lock.resid;
  cmd.u.renew.seq = lcxt->lock.seq;
  
  xdr_init( &xdr, (uint8_t *)cmdbuf, sizeof(cmdbuf) );
  dlm_encode_command( &xdr, &cmd );
  return raft_command( glob.raftclid, (char *)xdr.buf, xdr.offset, NULL );
}

int dlm_release( uint64_t lockid ) {
  /* submit raft command to relase lock */
  int sts;
  struct xdr_s xdr;
  char cmdbuf[64];
  struct dlm_command cmd;
  struct dlm_lockcxt *lcxt;
  
  if( !glob.ocount ) return -1;

  lcxt = lock_by_lockid( lockid, NULL );
  if( !lcxt ) return -1;
  
  /* submit raft command to renew lock */
  memset( &cmd, 0, sizeof(cmd) );
  cmd.cmd = DLM_CMD_RELEASE;
  cmd.lockid = lockid;
  cmd.resid = lcxt->lock.resid;
  
  xdr_init( &xdr, (uint8_t *)cmdbuf, sizeof(cmdbuf) );
  dlm_encode_command( &xdr, &cmd );
  sts = raft_command( glob.raftclid, (char *)xdr.buf, xdr.offset, NULL );

  return sts;
}

static void dlm_iter_cb( struct rpc_iterator *iter ) {
  /* check seqnos have been incremented since last time */
  int i;
  for( i = 0; i < glob.nlock; i++ ) {
    if( (glob.lock[i].prevseq == glob.lock[i].lock.seq) && (glob.lock[i].lock.mode != DLM_RELEASE) ) {
      /* seqno unchanged - release lock? */
      dlm_log( LOG_LVL_ERROR, "lock %"PRIx64" seqno unchanged - releasing", glob.lock[i].lock.lockid );
      dlm_release( glob.lock[i].lock.lockid );
      glob.lock[i].lock.mode = DLM_RELEASE;
    }

    glob.lock[i].prevseq = glob.lock[i].lock.seq;
  }
  
}

static struct dlm_lockcxt *lock_by_lockid( uint64_t lockid, int *idx ) {
  int i;
  for( i = 0; i < glob.nlock; i++ ) {
    if( glob.lock[i].lock.lockid == lockid ) {
      if( idx ) *idx = i;
      return &glob.lock[i];
    }
  }
  return NULL;
}
static struct dlm_lockcxt *lock_by_resid( uint64_t resid ) {
  int i;
  for( i = 0; i < glob.nlock; i++ ) {
    if( glob.lock[i].lock.resid == resid ) return &glob.lock[i];
  }
  return NULL;
}

static void dlm_command( struct raft_app *app, struct raft_cluster *cl, uint64_t cmdseq, char *buf, int len ) {
  int sts, i;
  struct xdr_s xdr;
  struct dlm_command cmd;
  struct dlm_lockcxt *lcxt;
  
  xdr_init( &xdr, (uint8_t *)buf, len );
  sts = dlm_decode_command( &xdr, &cmd );
  if( sts ) {
    dlm_log( LOG_LVL_ERROR, "Failed to decode command" );
    return;
  }

  switch( cmd.cmd ) {
  case DLM_CMD_LOCKEX:
    /* lookup lock with this lockid, if found and state is wait then set state to lockex and invoke cb */
    lcxt = lock_by_lockid( cmd.lockid, NULL );
    if( lcxt ) {
      if( lcxt->lock.mode != DLM_WAIT ) {
	dlm_log( LOG_LVL_ERROR, "LOck %"PRIx64" in state %u not wait", cmd.lockid, lcxt->lock.mode );
	return;
      }

      lcxt->lock.mode = DLM_EX;
      if( lcxt->cb ) lcxt->cb( &lcxt->lock, lcxt->cxt );      

      return;
    }
    
    /* existing lock not found - check resource not already locked */
    lcxt = lock_by_resid( cmd.resid );
    if( lcxt ) {
      dlm_log( LOG_LVL_ERROR, "Resource %"PRIx64" already locked by %"PRIx64"", cmd.resid, lcxt->lock.lockid );
      return;
    }
    
    /* add new lock */
    i = glob.nlock;
    if( i >= DLM_MAX_LOCK ) {
      dlm_log( LOG_LVL_ERROR, "Out of lock descriptors" );
      return;
    }

    memset( &glob.lock[i], 0, sizeof(glob.lock[i]) );
    glob.lock[i].lock.lockid = cmd.lockid;
    glob.lock[i].lock.resid = cmd.resid;
    glob.lock[i].lock.seq = 1;
    glob.lock[i].lock.mode = DLM_EX;
    glob.nlock++;
    break;
  case DLM_CMD_LOCKSH:
    /* lookup lock with this lockid, if found and state is wait then set state to lockex and invoke cb */
    lcxt = lock_by_lockid( cmd.lockid, NULL );
    if( lcxt ) {
      if( lcxt->lock.mode != DLM_WAIT ) {
	dlm_log( LOG_LVL_ERROR, "LOck %"PRIx64" in state %u not wait", cmd.lockid, lcxt->lock.mode );
	return;
      }

      lcxt->lock.mode = DLM_SH;
      if( lcxt->cb ) lcxt->cb( &lcxt->lock, lcxt->cxt );      

      return;
    }
    
    /* existing lock not found - check resource not already locked */
    lcxt = lock_by_resid( cmd.resid );
    if( lcxt && (lcxt->lock.mode == DLM_EX) ) {
      dlm_log( LOG_LVL_ERROR, "Resource %"PRIx64" already locked exclusively by %"PRIx64"", cmd.resid, lcxt->lock.lockid );
      return;
    }
    
    /* add new lock */
    i = glob.nlock;
    if( i >= DLM_MAX_LOCK ) {
      dlm_log( LOG_LVL_ERROR, "Out of lock descriptors" );
      return;
    }

    memset( &glob.lock[i], 0, sizeof(glob.lock[i]) );
    glob.lock[i].lock.lockid = cmd.lockid;
    glob.lock[i].lock.resid = cmd.resid;
    glob.lock[i].lock.seq = 1;
    glob.lock[i].lock.mode = DLM_SH;
    glob.nlock++;
    break;
  case DLM_CMD_RELEASE:
    /* lookup lock and release */
    lcxt = lock_by_lockid( cmd.lockid, &i );
    if( lcxt ) {
      dlm_log( LOG_LVL_TRACE, "Releasing lock %"PRIx64"", cmd.lockid );
      if( i != (glob.nlock - 1) ) glob.lock[i] = glob.lock[glob.nlock - 1];
      glob.nlock--;
    } else {
      dlm_log( LOG_LVL_ERROR, "Unknown lock %"PRIx64"", cmd.lockid );
    }
					 
    break;
  case DLM_CMD_RENEW:
    /* increment seqno */
    lcxt = lock_by_lockid( cmd.lockid, NULL );
    if( lcxt ) {
      lcxt->lock.seq++;
    } else {
      dlm_log( LOG_LVL_ERROR, "Unknown lock %"PRIx64"", cmd.lockid );
    }
    break;
  default:
    dlm_log( LOG_LVL_ERROR, "Invalid command %u", cmd.cmd );
    return;
  }

  
}

static void dlm_snapsave( struct raft_app *app, struct raft_cluster *cl, uint64_t term, uint64_t seq ) {
  raft_snapshot_save( cl->clid, term, seq, 0, (char *)&glob.nlock, sizeof(glob.nlock) );
  raft_snapshot_save( cl->clid, term, seq, 4, (char *)glob.lock, sizeof(glob.lock) );
  raft_snapshot_save( cl->clid, term, seq, -1, NULL, 0 );
}

static void dlm_snapload( struct raft_app *app, struct raft_cluster *cl, char *buf, int len ) {
  uint32_t nlock;
  
  if( len != (4 + sizeof(glob.lock)) ) {
    dlm_log( LOG_LVL_ERROR, "Bad snapshot size %u", len );
    return;
  }

  nlock = *((uint32_t *)buf);
  if( nlock > DLM_MAX_LOCK ) {
    dlm_log( LOG_LVL_ERROR, "Bad nlock" );
    return;
  }

  glob.nlock = nlock;
  memcpy( glob.lock, buf, sizeof(glob.lock) );
}

static int dlm_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}
static struct rpc_proc dlm_procs[] = {
  { 0, dlm_proc_null },
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

