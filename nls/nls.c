
#include "nls.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <mmf.h>
#include <sec.h>
#include <rpc.h>
#include <rpcd.h>
#include <sys/stat.h>
#include <hostreg.h>

#define NLS_MAX_SHARE 32
#define NLS_MAX_REMOTE 32
#define NLS_READ_TIMEOUT 1000    /* milli seocnds to wait for an rpc reply */
#define NLS_POLL_TIMEOUT 500     /* seconds to wait since last notification */

struct nls_header {
    uint32_t magic;
#define NLS_MAGIC 0x22AE9EF6
    uint32_t version;
    uint64_t seq;

    /* entry counts */
    uint32_t share_max;
    uint32_t share_count;
    uint32_t remote_max;
    uint32_t remote_count;
  
    /* header fields */
};


struct nls_file {
    struct nls_header header;
    struct nls_share share[NLS_MAX_SHARE];
    struct nls_remote remote[NLS_MAX_REMOTE];
};

static struct {
    struct mmf_s mmf;
    int ocount;
    struct nls_file *file;
} glob;

static void nls_lock( void ) {
    mmf_lock( &glob.mmf );
}
static void nls_unlock( void ) {
    mmf_unlock( &glob.mmf );
}

int nls_open( void ) {
    int sts;
    
    if( glob.ocount < 0 ) return -1;
    if( glob.ocount > 0 ) {
        glob.ocount++;
        return 0;
    }
    
    sts = mmf_open( mmf_default_path( "nls", "nls.dat", NULL ), &glob.mmf );
    if( sts ) return sts;
    
    sts = mmf_remap( &glob.mmf, sizeof(*glob.file) );
    if( sts ) goto bad;
    glob.file = (struct nls_file *)glob.mmf.file;
    
    nls_lock();
    if( glob.file->header.magic != NLS_MAGIC ) {
        glob.file->header.magic = NLS_MAGIC;
        glob.file->header.version = NLS_VERSION;
        glob.file->header.seq = 1;
        glob.file->header.share_max = NLS_MAX_SHARE;
        glob.file->header.share_count = 0;
	glob.file->header.remote_max = NLS_MAX_REMOTE;
	glob.file->header.remote_count = 0;
    } else if( glob.file->header.version != NLS_VERSION ) {
        nls_unlock();
        goto bad;
    }
    nls_unlock();
    
    glob.ocount = 1;
    return 0;
 bad:
    mmf_close( &glob.mmf );
    return -1;
}

int nls_close( void ) {
    if( glob.ocount <= 0 ) return -1;
    glob.ocount--;
    if( glob.ocount > 0 ) return 0;
    mmf_close( &glob.mmf );
    return 0;
}

int nls_reset( void ) {
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    glob.file->header.magic = NLS_MAGIC;
    glob.file->header.version = NLS_VERSION;
    glob.file->header.seq = 1;
    glob.file->header.share_max = NLS_MAX_SHARE;
    glob.file->header.share_count = 0;
    glob.file->header.remote_max = NLS_MAX_REMOTE;
    glob.file->header.remote_count = 0;
    nls_unlock();
    return 0;
}

int nls_prop( struct nls_prop *prop ) {
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    prop->version = glob.file->header.version;
    prop->seq = glob.file->header.seq;
    prop->share_max = glob.file->header.share_max;
    prop->share_count = glob.file->header.share_count;
    prop->remote_max = glob.file->header.remote_max;
    prop->remote_count = glob.file->header.remote_count;
    nls_unlock();
    return 0;
}

/* ------------ share commands ----------- */

int nls_share_list( struct nls_share *sharelist, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    for( i = 0; i < glob.file->header.share_count; i++ ) {
         if( i < n ) {
             sharelist[i] = glob.file->share[i];
         }
    }
    sts = glob.file->header.share_count;
    nls_unlock();
    return sts;
}

int nls_share_by_hshare( uint64_t hshare, struct nls_share *share ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.share_count; i++ ) {
        if( glob.file->share[i].hshare == hshare ) {
            if( share ) *share = glob.file->share[i];
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_share_add( struct nls_share *share ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    if( glob.file->header.share_count < glob.file->header.share_max ) {
	if( !share->hshare ) sec_rand( &share->hshare, sizeof(share->hshare) );
        i = glob.file->header.share_count;
        glob.file->share[i] = *share;
        glob.file->header.share_count++;
        glob.file->header.seq++;
        sts = 0;
    }
    nls_unlock();
    return sts;
}

int nls_share_rem( uint64_t hshare ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.share_count; i++ ) {
        if( glob.file->share[i].hshare == hshare ) {
            if( i != (glob.file->header.share_count - 1) ) glob.file->share[i] = glob.file->share[glob.file->header.share_count - 1];
            glob.file->header.share_count--;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_share_set( struct nls_share *share ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.share_count; i++ ) {
        if( glob.file->share[i].hshare == share->hshare ) {
            glob.file->share[i] = *share;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_share_open( struct nls_share *share, struct log_s *log ) {
  char name[256];
  sprintf( name, "%s.log", share->name );
  return log_open( mmf_default_path( "nls", name, NULL ), NULL, log );
}


/* ------------ remote commands ----------- */

int nls_remote_list( struct nls_remote *rlist, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    for( i = 0; i < glob.file->header.remote_count; i++ ) {
         if( i < n ) {
             rlist[i] = glob.file->remote[i];
         }
    }
    sts = glob.file->header.remote_count;
    nls_unlock();
    return sts;
}

int nls_remote_by_hshare( uint64_t hshare, struct nls_remote *remote ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.remote_count; i++ ) {
        if( glob.file->remote[i].share.hshare == hshare ) {
            if( remote ) *remote = glob.file->remote[i];
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_remote_add( struct nls_remote *remote ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    if( !remote->share.hshare ) return -1;
    
    nls_lock();
    sts = -1;
    if( glob.file->header.remote_count < glob.file->header.remote_max ) {
        i = glob.file->header.remote_count;
        glob.file->remote[i] = *remote;
        glob.file->header.remote_count++;
        glob.file->header.seq++;
        sts = 0;
    }
    nls_unlock();
    return sts;
}

int nls_remote_rem( uint64_t hshare ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.remote_count; i++ ) {
        if( glob.file->remote[i].share.hshare == hshare ) {
            if( i != (glob.file->header.remote_count - 1) ) glob.file->remote[i] = glob.file->remote[glob.file->header.remote_count - 1];
            glob.file->header.remote_count--;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_remote_set( struct nls_remote *remote ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.remote_count; i++ ) {
        if( glob.file->remote[i].share.hshare == remote->share.hshare ) {
            glob.file->remote[i] = *remote;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_remote_open( struct nls_remote *remote, struct log_s *log ) {
  char name[256], hostid[64];
  sprintf( hostid, "%"PRIu64"", remote->hostid );
  sprintf( name, "%s.log", remote->share.name );
#ifdef WIN32
  CreateDirectoryA( mmf_default_path( "nls", hostid, NULL ), NULL );
#else
  mkdir( mmf_default_path( "nls", hostid, NULL ), 0755 );
#endif
  return log_open( mmf_default_path( "nls", hostid, name, NULL ), NULL, log );
}



/* --------------- rpc services --------------------- */

static int nls_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}
static int nls_proc_list( struct rpc_inc *inc ) {
  int handle;
  int sts, i;
  struct nls_share shares[32];
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = nls_share_list( shares, 32 );
  for( i = 0; i < sts; i++ ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_uint64( &inc->xdr, shares[i].hshare );
    xdr_encode_string( &inc->xdr, shares[i].name );
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}
static int nls_proc_prop( struct rpc_inc *inc ) {
  int handle;
  uint64_t hshare;
  struct log_prop prop;
  struct nls_share share;
  int sts;
  struct log_s log;
  
  sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  sts = nls_share_by_hshare( hshare, &share );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "failed to lookup share hshare=%"PRIu64"", hshare );
    xdr_encode_boolean( &inc->xdr, 0 );
  } else {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_uint64( &inc->xdr, hshare );
    xdr_encode_string( &inc->xdr, share.name );

    memset( &prop, 0, sizeof(prop) );
    sts = nls_share_open( &share, &log );
    if( sts ) {
      rpc_log( RPC_LOG_ERROR, "failed to open shared log name=%s", share.name );
    } else {
      log_prop( &log, &prop );
      log_close( &log );
    }
    
    /* encode log prop */
    xdr_encode_uint32( &inc->xdr, prop.version );
    xdr_encode_uint64( &inc->xdr, prop.seq );
    xdr_encode_uint32( &inc->xdr, prop.lbacount );
    xdr_encode_uint32( &inc->xdr, prop.start );
    xdr_encode_uint32( &inc->xdr, prop.count );
    xdr_encode_uint64( &inc->xdr, prop.last_id );
    xdr_encode_uint32( &inc->xdr, prop.flags );
  }
  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int nls_proc_read( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t hshare;
  struct log_s log;
  struct nls_share share;
  struct log_entry entry;
  struct log_iov iov[1];
  struct rpc_conn *tmpconn = NULL;
  uint64_t id;
  int ne;
  uint32_t count;
  
  sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &id );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &count );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = nls_share_by_hshare( hshare, &share );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  sts = nls_share_open( &share, &log );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  while( count > 0 ) {
    memset( &entry, 0, sizeof(entry) );
    iov[0].buf = (char *)tmpconn->buf;
    iov[0].len = tmpconn->count;
    entry.iov = iov;
    entry.niov = 1;
    sts = log_read( &log, id, &entry, 1, &ne );
    if( sts || (ne == 0) ) break;

    /* encode entry */
    sts = xdr_encode_boolean( &inc->xdr, 1 );
    sts = xdr_encode_uint64( &inc->xdr, entry.id );
    sts = xdr_encode_uint64( &inc->xdr, entry.prev_id );
    sts = xdr_encode_uint64( &inc->xdr, entry.seq );
    sts = xdr_encode_uint32( &inc->xdr, entry.flags );
    sts = xdr_encode_opaque( &inc->xdr, (uint8_t *)entry.iov[0].buf, entry.iov[0].len );
    
    id = entry.id;
    count--;
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  
  log_close( &log );
  
 done:
  if( tmpconn ) rpc_conn_release( tmpconn );
  rpc_complete_accept_reply( inc, handle );

  return 0;
}

static int nls_proc_write( struct rpc_inc *inc ) {
  int handle, sts, successp, buflen;
  uint64_t hshare, id;
  char *bufp;
  struct nls_share share;
  struct log_s log;
  
  sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &buflen );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  id = 0;
  successp = 0;
  sts = nls_share_by_hshare( hshare, &share );
  if( !sts ) {    
    sts = nls_share_open( &share, &log );
    if( !sts ) {
      successp = 1;
      sts = log_write_buf( &log, 0, bufp, buflen, &id );
      log_close( &log );
    }
  }

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, successp );
  xdr_encode_uint64( &inc->xdr, id );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int nls_proc_notreg( struct rpc_inc *inc ) {
  int handle;

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static void nls_read_cb( struct rpc_waiter *w, struct rpc_inc *inc ) {
  free( w );
}

static void nls_call_read( uint64_t hostid, uint64_t hshare, uint64_t lastid, int count ) {
  int sts;
  struct rpc_waiter *w;
  struct hostreg_host host;
  struct rpc_conn *conn;
  struct rpc_listen *listen;
  struct rpc_inc inc;
  int handle;
  
  /* lookup host */
  sts = hostreg_host_by_id( hostid, &host );
  if( sts ) return;

  /* get listen descriptor */
  listen = rpcd_listen_by_type( RPC_LISTEN_UDP );
  if( !listen ) return;
  
  /* prepare message */
  conn = rpc_conn_acquire();
  if( !conn ) return;

  /* encode message */
  memset( &inc, 0, sizeof(inc) );
  xdr_init( &inc.xdr, conn->buf, conn->count );
  
  rpc_init_call( &inc, NLS_RPC_PROG, NLS_RPC_VERS, 3, &handle );
  xdr_encode_uint64( &inc.xdr, hshare );
  xdr_encode_uint64( &inc.xdr, lastid );
  xdr_encode_uint32( &inc.xdr, 16 );
  rpc_complete_call( &inc, handle );
  
  /* send */
  sendto( listen->fd, inc.xdr.buf, inc.xdr.offset, 0,
	  (struct sockaddr *)&host.addr[0], sizeof(host.addr[0]) );
  
  rpc_conn_release( conn );

  /* await reply */
  w = malloc( sizeof(*w) );
  memset( w, 0, sizeof(*w) );
  w->xid = inc.msg.xid;
  w->timeout = rpc_now() + NLS_READ_TIMEOUT;
  w->cb = nls_read_cb;
  w->cxt = NULL;
  rpc_await_reply( w );
}

/* 
 * This is sent from a log server to a client with a registered remote log.
 * It is used to inform the client of the current log properties, so that it 
 * can read any missing entries 
 */
static int nls_proc_notify( struct rpc_inc *inc ) {
  int handle;
  int sts;
  uint64_t hshare, hostid, seq, lastid;
  struct nls_remote remote;
  
  sts = xdr_decode_uint64( &inc->xdr, &hostid );
  sts = xdr_decode_uint64( &inc->xdr, &hshare );
  sts = xdr_decode_uint64( &inc->xdr, &seq );
  sts = xdr_decode_uint64( &inc->xdr, &lastid );


  /* lookup remote entry */
  sts = nls_remote_by_hshare( hshare, &remote );
  if( sts ) goto done;
  
  /* compare against currently held info */
  if( remote.seq == seq ) goto done;

  /* issue async call to read missing log entries */
  nls_call_read( hostid, hshare, remote.lastid, seq - remote.seq );
  
 done:
  /* don't send a reply */
  return -1;
}


static struct rpc_proc nls_procs[] = {
  { 0, nls_proc_null },
  { 1, nls_proc_list },
  { 2, nls_proc_prop },
  { 3, nls_proc_read },
  { 4, nls_proc_write },
  { 5, nls_proc_notreg },
  { 6, nls_proc_notify },
  { 0, NULL }
};

static struct rpc_version nls_vers = {
  NULL, NLS_RPC_VERS, nls_procs
};

static struct rpc_program nls_prog = {
  NULL, NLS_RPC_PROG, &nls_vers
};


/* -------------- client side iterator ----------------- */

static void nls_iter_cb( struct rpc_iterator *iter ) {
  /* 
   * get a list of all remote logs. if we have not heard from them in a while then 
   * request an update callback 
   */

  struct nls_remote remote[NLS_MAX_REMOTE];
  int n, i;
  uint64_t now;

  now = time( NULL );
  n = nls_remote_list( remote, NLS_MAX_REMOTE );
  for( i = 0; i < n; i++ ) {
    if( remote[i].timestamp < (now - NLS_POLL_TIMEOUT) ) {
      /* ask for a callback */
      //nls_call_notreg( remote.hostid );
    }
  }
  
}

static struct rpc_iterator nls_iter = {
    NULL,
    0,
    1000,
    nls_iter_cb,
    NULL
};


/* -------------- register rpc service -------------------- */

void nls_register( void ) {
  nls_open();
  rpc_program_register( &nls_prog );
  rpc_iterator_register( &nls_iter );
}
