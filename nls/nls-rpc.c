

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

#include "nls-private.h"

struct nls_notcxt {
  uint64_t hostid;
  uint64_t hshare;
  uint64_t seq;
  uint64_t timestamp;
};
static struct nls_notcxt *nls_notcxt_get( uint64_t hostid, uint64_t hshare );
static uint64_t nls_share_seqno( uint64_t hshare, uint64_t *lastid );

/* --------------- rpc services --------------------- */

static int nls_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int nls_encode_prop( struct xdr_s *xdr, struct nls_share *share, struct log_prop *prop ) {
  xdr_encode_uint64( xdr, share->hshare );
  xdr_encode_string( xdr, share->name );  

  xdr_encode_uint32( xdr, prop->version );
  xdr_encode_uint64( xdr, prop->seq );
  xdr_encode_uint32( xdr, prop->lbacount );
  xdr_encode_uint32( xdr, prop->start );
  xdr_encode_uint32( xdr, prop->count );
  xdr_encode_uint64( xdr, prop->last_id );
  xdr_encode_uint32( xdr, prop->flags );

  return 0;
}
static int nls_decode_prop( struct xdr_s *xdr, struct nls_share *share, struct log_prop *prop ) {
  int sts;
  
  sts = xdr_decode_uint64( xdr, &share->hshare );
  if( !sts ) sts = xdr_decode_string( xdr, share->name, sizeof(share->name) );  

  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->version );  
  if( !sts ) sts = xdr_decode_uint64( xdr, &prop->seq );
  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->lbacount );
  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->start );
  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->count );
  if( !sts ) sts = xdr_decode_uint64( xdr, &prop->last_id );
  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->flags );

  return sts;
}

static int nls_encode_share( struct xdr_s *xdr, struct nls_share *share ) {
  int sts;
  struct log_s log;
  struct log_prop prop;
  
  memset( &prop, 0, sizeof(prop) );
  sts = nls_share_open( share, &log );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "failed to open shared log name=%s", share->name );
  } else {
    log_prop( &log, &prop );
    log_close( &log );
  }
  nls_encode_prop( xdr, share, &prop );

  return 0;
}

static int nls_proc_list( struct rpc_inc *inc ) {
  int handle;
  int sts, i;
  uint64_t seq, lastid;
  struct nls_share shares[32];
  struct log_prop prop;
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = nls_share_list( shares, 32 );
  for( i = 0; i < sts; i++ ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    nls_encode_share( &inc->xdr, &shares[i] );
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
    nls_encode_share( &inc->xdr, &share );
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
  struct log_prop prop;
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

  /* encode current log properties */
  xdr_encode_boolean( &inc->xdr, 1 );
  nls_encode_prop( &inc->xdr, &share, &prop );

  /* get a tmp buffer */
  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  /* read and encode entries */
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
  uint32_t flags;
  uint64_t hshare, id;
  char *bufp;
  struct nls_share share;
  struct log_s log;
  struct log_prop prop;
  struct log_entry e;
  struct log_iov iov[1];
   
  sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &buflen );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );


  memset( &prop, 0, sizeof(prop) );
  id = 0;
  successp = 0;
  sts = nls_share_by_hshare( hshare, &share );
  if( !sts ) {    
    sts = nls_share_open( &share, &log );
    if( !sts ) {
      successp = 1;
      memset( &e, 0, sizeof(e) );
      iov[0].buf = bufp;
      iov[0].len = buflen;
      e.iov = iov;
      e.niov = 1;
      e.flags = flags;
      sts = log_write( &log, &e );
      
      id = e.id;
      log_prop( &log, &prop );
      log_close( &log );
    }
  }

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, successp );
  if( successp ) {
    xdr_encode_uint64( &inc->xdr, id );
    nls_encode_prop( &inc->xdr, &share, &prop );
  }
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}


/* clients call this to register for callback notifications */
static int nls_proc_notreg( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t hostid, hshare;
  struct nls_notcxt *cxt;
 
  sts = xdr_decode_uint64( &inc->xdr, &hostid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( sts ) rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  /* lookup context or add new one */
  cxt = nls_notcxt_get( hostid, hshare );
  cxt->timestamp = rpc_now();

  rpc_log( RPC_LOG_DEBUG, "nls_proc_notreg hostid=%"PRIx64" hshare=%"PRIx64"", hostid, hshare );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );  
  xdr_encode_uint64( &inc->xdr, cxt->seq );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}




static void nls_call_read( uint64_t hostid, uint64_t hshare, uint64_t seq, uint64_t lastid, int count );
struct nls_read_cxt {
  uint64_t hostid;
  uint64_t hshare;
  uint64_t seq;
  uint64_t lastid;
};

static void nls_read_cb( struct rpc_waiter *w, struct rpc_inc *inc ) {
  int sts, b;
  struct log_s log;
  uint64_t id, lastid, previd, seq;
  uint32_t flags;
  char *bufp;
  int lenp;
  struct log_entry e;
  struct log_iov iov[1];
  struct nls_read_cxt *nlscxtp;
  int logopen = 0;
  struct nls_remote remote;
  struct nls_share rshare;
  struct log_prop prop;
  int nmsgs = 0;
  
  nlscxtp = (struct nls_read_cxt *)w->cxt;

  rpc_log( RPC_LOG_DEBUG, "nls_read_cb hostid=%"PRIx64"", nlscxtp->hostid );
  
  /* do nothing if call timed out? */
  if( !inc ) {
    rpc_log( RPC_LOG_ERROR, "nls_read_cb timeout" );
    goto done;
  }

  /* decode results */
  sts = xdr_decode_boolean( &inc->xdr, &b );
  if( sts || !b ) {
    rpc_log( RPC_LOG_ERROR, "error stauts" );
    goto done;
  }

  sts = nls_decode_prop( &inc->xdr, &rshare, &prop );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "nls_decode_prop failed" );
    goto done;
  }
  rpc_log( RPC_LOG_DEBUG, "reading entries for log hshare=%"PRIx64"", rshare.hshare );
  
  sts = nls_remote_by_hshare( rshare.hshare, &remote );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "nls_remote_by_hshare failed" );
    goto done;
  }
  
  /* open local log and write new entries */
  sts = nls_remote_open( &remote, &log );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "nls_remote_open failed" );
    goto done;
  }
  logopen = 1;
  
  sts = xdr_decode_boolean( &inc->xdr, &b );
  if( sts ) goto done;
  while( b ) {
    rpc_log( RPC_LOG_DEBUG, "xxx" );
    
    sts = xdr_decode_uint64( &inc->xdr, &id );
    if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &previd );
    if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &seq );
    if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
    if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
    if( sts ) {
      rpc_log( RPC_LOG_ERROR, "Failed to decode" );
      goto done;
    }

    rpc_log( RPC_LOG_DEBUG, "Writing entry name=%s id=%"PRIx64" seq=%"PRIu64" flags=%u count=%d",
	     remote.share.name, id, seq, flags, lenp );
    
    memset( &e, 0, sizeof(e) );
    iov[0].buf = bufp;
    iov[0].len = lenp;
    e.iov = iov;
    e.niov = 1;
    e.flags = flags;      
    log_write( &log, &e );

    lastid = id;
    nmsgs++;
    
    sts = xdr_decode_boolean( &inc->xdr, &b );
    if( sts ) goto done;
  }
  
  /* update remote descriptor */
  sts = nls_remote_by_hshare( rshare.hshare, &remote );
  if( sts ) goto done;
  
  remote.seq = seq;
  remote.lastid = lastid;
  nls_remote_set( &remote );

  /* Did we read all available messages? If not then continue */
  if( nmsgs == 0 || seq == nlscxtp->seq ) {
    rpc_log( RPC_LOG_DEBUG, "No more log entries" );
    goto done;
  }

  nlscxtp->lastid = lastid;
  rpc_log( RPC_LOG_DEBUG, "expected lastid=%"PRIu64" got lastid=%"PRIx64"", nlscxtp->lastid, lastid );
  nls_call_read( nlscxtp->hostid, nlscxtp->hshare, nlscxtp->seq, nlscxtp->lastid, 16 );
  
 done:
  if( logopen ) log_close( &log );
  free( w );
}

/* send a read command to server */
static void nls_call_read( uint64_t hostid, uint64_t hshare, uint64_t seq, uint64_t lastid, int count ) {
  int sts;
  struct rpc_waiter *w;
  struct hostreg_host host;
  struct rpc_conn *conn;
  struct rpc_listen *listen;
  struct rpc_inc inc;
  int handle;
  struct nls_read_cxt *nlscxtp;
  struct sockaddr_in sin;
  
  if( count > 16 ) count = 16;

  rpc_log( RPC_LOG_DEBUG, "nls_call_read hostid=%"PRIx64" hshare=%"PRIx64" seq=%"PRIu64" lastid=%"PRIx64" count=%u",
	   hostid, hshare, seq, lastid, count );
  
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
  xdr_encode_uint32( &inc.xdr, count );
  rpc_complete_call( &inc, handle );
  
  /* send */
  memset( &sin, 0, sizeof(sin) );
  sin.sin_family = AF_INET;
  sin.sin_port = listen->addr.sin.sin_port;
  sin.sin_addr.s_addr = host.addr[0];
  sts = sendto( listen->fd, inc.xdr.buf, inc.xdr.offset, 0,
	  (struct sockaddr *)&sin, sizeof(sin) );
  if( sts < 0 ) rpc_log( RPC_LOG_ERROR, "sendto: %s", strerror( errno ) );
  
  rpc_conn_release( conn );

  /* await reply */
  w = malloc( sizeof(*w) + sizeof(struct nls_read_cxt) );
  nlscxtp = (struct nls_read_cxt *)(((char *)w) + sizeof(*w));
  nlscxtp->hostid = hostid;
  nlscxtp->hshare = hshare;
  nlscxtp->lastid = lastid;
  nlscxtp->seq = seq;
  
  memset( w, 0, sizeof(*w) );
  w->xid = inc.msg.xid;
  w->timeout = rpc_now() + NLS_READ_TIMEOUT;
  w->cb = nls_read_cb;
  w->cxt = nlscxtp;
  rpc_await_reply( w );
}

static void nls_call_notreg_cb( struct rpc_waiter *w, struct rpc_inc *inc ) {
  if( !inc ) {
    rpc_log( RPC_LOG_ERROR, "nls_call_notreg_cb: timeout" );
    goto done;
  }

 done:
  free( w );
}

/* send a read command to server */
static void nls_call_notreg( uint64_t hostid, uint64_t hshare ) {
  int sts;
  struct hostreg_host host;
  struct rpc_conn *conn;
  struct rpc_listen *listen;
  struct rpc_inc inc;
  int handle;
  struct hostreg_prop prop;
  struct rpc_waiter *w;
  struct sockaddr_in sin;
  
  sts = hostreg_prop( &prop );
  if( sts ) return;
  
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
  
  rpc_init_call( &inc, NLS_RPC_PROG, NLS_RPC_VERS, 5, &handle );
  xdr_encode_uint64( &inc.xdr, prop.localid );
  xdr_encode_uint64( &inc.xdr, hshare );
  rpc_complete_call( &inc, handle );
  
  /* send */
  memset( &sin, 0, sizeof(sin) );
  sin.sin_family = AF_INET;
  sin.sin_port = listen->addr.sin.sin_port;
  sin.sin_addr.s_addr = host.addr[0];
  sts = sendto( listen->fd, inc.xdr.buf, inc.xdr.offset, 0,
		(struct sockaddr *)&sin, sizeof(sin) );
  if( sts < 0 ) rpc_log( RPC_LOG_ERROR, "sendto: %s", strerror( errno ) );
  
  rpc_conn_release( conn );

  /* await reply */
  w = malloc( sizeof(*w) );
  memset( w, 0, sizeof(*w) );
  w->xid = inc.msg.xid;
  w->timeout = rpc_now() + NLS_READ_TIMEOUT;   
  w->cb = nls_call_notreg_cb;
  rpc_await_reply( w );

}

/* 
 * This is sent from a log server to a client with a registered remote log.
 * It is used to inform the client of the current log properties, so that it 
 * can read any missing entries 
 */
static int nls_proc_notify( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t hshare, hostid, seq, lastid;
  struct nls_remote remote;
  
  sts = xdr_decode_uint64( &inc->xdr, &hostid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &seq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &lastid );
  if( sts ) goto done;

  /* lookup remote entry */
  sts = nls_remote_by_hshare( hshare, &remote );
  if( sts ) goto done;
  
  /* compare against currently held info */
  if( remote.seq == seq ) goto done;

  /* issue async call to read missing log entries */
  rpc_log( RPC_LOG_DEBUG, "nls_proc_notify hostid=%"PRIx64"", hostid );
  nls_call_read( hostid, hshare, seq, remote.lastid, seq - remote.seq );

 done:
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );

  return 0;
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

static void nls_clt_iter_cb( struct rpc_iterator *iter ) {
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
      /* ask for some entries */
      nls_call_read( remote[i].hostid, remote[i].share.hshare, remote[i].lastid, remote[i].seq, 16 );
      
      /* ask for a callback */
      //      rpc_log( RPC_LOG_DEBUG, "nls_clt_iter_cb nls_call_notreg hostid=%"PRIx64" hshare=%"PRIx64"", remote[i].hostid, remote[i].share.hshare );
      //      nls_call_notreg( remote[i].hostid, remote[i].share.hshare );
      
      /* schedule to ask again later */
      remote[i].timestamp = now + NLS_POLL_TIMEOUT;
      nls_remote_set( &remote[i] );      
    }
  }

}

static struct rpc_iterator nls_clt_iter = {
    NULL,
    0,
    1000,
    nls_clt_iter_cb,
    NULL
};

/* ------------------ server side iterator -------------- */

static uint64_t nls_share_seqno( uint64_t hshare, uint64_t *lastid ) {
  int sts;
  struct log_s log;
  struct log_prop prop;
  uint64_t seq = 0;
  struct nls_share share;

  if( lastid ) *lastid = 0;
  
  sts = nls_share_by_hshare( hshare, &share );
  if( sts ) return 0;
  
  sts = nls_share_open( &share, &log );
  if( sts ) return 0;

  sts = log_prop( &log, &prop );
  if( sts ) goto done;

  seq = prop.seq;
  if( lastid ) *lastid = prop.last_id;
  
 done:
  log_close( &log );
  return seq;  
}

#define NLS_MAX_NOTCXT 32 
static struct nls_notcxt notcxttab[NLS_MAX_NOTCXT];

static struct nls_notcxt *nls_notcxt_get( uint64_t hostid, uint64_t hshare ) {
  int i, oldest;
  struct nls_notcxt *cxt;
  uint64_t age, purgetimo;

  purgetimo = rpc_now();
  if( purgetimo > NLS_PURGE_TIMEOUT ) purgetimo -= NLS_PURGE_TIMEOUT;
  
  oldest = 0;
  age = 0;
  for( i = 0; i < NLS_MAX_NOTCXT; i++ ) {
    if( notcxttab[i].timestamp < purgetimo ) notcxttab[i].hostid = 0;
    
    if( notcxttab[i].hostid == 0 ) continue;
    if( notcxttab[i].hostid == hostid && notcxttab[i].hshare == hshare ) return &notcxttab[i];
    
    
    if( oldest == 0 || (notcxttab[i].timestamp < age) ) {
      oldest = i;
      age = notcxttab[i].timestamp;
    }
  }

  cxt = &notcxttab[oldest];
  memset( cxt, 0, sizeof(*cxt) );
  cxt->hostid = hostid;
  cxt->hshare = hshare;
  cxt->seq = nls_share_seqno( hshare, NULL ); 
  cxt->timestamp = rpc_now();
  
  return cxt;
}

/* send a notification callback to client */
static void nls_call_notify( struct nls_notcxt *cxt, uint64_t lastid ) {
  int handle, sts;
  struct hostreg_host host;
  struct rpc_conn *conn;
  struct rpc_listen *listen;
  struct rpc_inc inc;
  struct hostreg_prop prop;
  struct sockaddr_in sin;
  
  sts = hostreg_prop( &prop );
  if( sts ) return;
  
  /* lookup host */
  sts = hostreg_host_by_id( cxt->hostid, &host );
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
  
  rpc_init_call( &inc, NLS_RPC_PROG, NLS_RPC_VERS, 6, &handle );
  xdr_encode_uint64( &inc.xdr, prop.localid );
  xdr_encode_uint64( &inc.xdr, cxt->hshare );
  xdr_encode_uint64( &inc.xdr, cxt->seq );
  xdr_encode_uint64( &inc.xdr, lastid );
  rpc_complete_call( &inc, handle );
  
  /* send */
  memset( &sin, 0, sizeof(sin) );
  sin.sin_family = AF_INET;
  sin.sin_port = listen->addr.sin.sin_port;
  sin.sin_addr.s_addr = host.addr[0];
  sts = sendto( listen->fd, inc.xdr.buf, inc.xdr.offset, 0,
	  (struct sockaddr *)&sin, sizeof(sin) );
  if( sts < 0 ) rpc_log( RPC_LOG_ERROR, "sendto: %s", strerror( errno ) );
  
  rpc_conn_release( conn );

  /* we don't expect a reply */
  return;
}

/*
 * send out change notifications to any registered clients 
 */
static void nls_svr_iter_cb( struct rpc_iterator *iter ) {
  int i;
  struct nls_notcxt *cxt;
  uint64_t seq, lastid;
  
  for( i = 0; i < NLS_MAX_NOTCXT; i++ ) {
    if( notcxttab[i].hostid == 0 ) continue;

    cxt = &notcxttab[i];
    /* compare seqno against stored seqno */
    seq = nls_share_seqno( cxt->hshare, &lastid );
    if( cxt->seq != seq ) {      
      cxt->seq = seq;

      //rpc_log( RPC_LOG_DEBUG, "nls_svr_iter_cb nls_call_notify hostid=%"PRIx64"", cxt->hostid );
      //nls_call_notify( cxt, lastid );
    }
  }
  
}

static struct rpc_iterator nls_svr_iter = {
    NULL,
    0,
    1000,
    nls_svr_iter_cb,
    NULL
};

/* -------------- register rpc service -------------------- */

void nls_register( void ) {
  int i, n;
  struct nls_remote remote[NLS_MAX_REMOTE];
  
  nls_open();
  hostreg_open();

  /* reset all remote timestamps */
  n = nls_remote_list( remote, NLS_MAX_REMOTE );
  for( i = 0; i < n; i++ ) {
    remote[i].timestamp = 0;
    nls_remote_set( &remote[i] );
  }
      
  
  rpc_program_register( &nls_prog );
  rpc_iterator_register( &nls_clt_iter );
  rpc_iterator_register( &nls_svr_iter );
}
