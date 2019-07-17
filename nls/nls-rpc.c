
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

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
#include <hrauth.h>

#include "nls-private.h"

#ifdef WIN32
#define PRIu64 "llu"
#define PRIx64 "llx"
#endif

static struct {
  struct nls_prop prop;
} glob;

static uint64_t nls_share_seqno( uint64_t hshare, uint64_t *lastid );
static uint64_t nls_remote_seqno( uint64_t hshare, uint64_t *lastid );

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
  int ne, eof = 0;
  uint32_t xdrcount, xdrlen;
  
  sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &id );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &xdrcount );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  rpc_log( RPC_LOG_DEBUG, "proc_read: hshare=%"PRIx64" id=%"PRIx64" xdrcount=%u", hshare, id, xdrcount );
  if( xdrcount > 32*1024 ) xdrcount = 32*1024;
  if( xdrcount < 1024 ) xdrcount = 1024;
  
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

  log_prop( &log, &prop );
  nls_encode_prop( &inc->xdr, &share, &prop );

  /* get a tmp buffer */
  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  /* read and encode entries */
  xdrcount -= 4; /* reserve space for terminating bool */
  while( inc->xdr.offset < xdrcount ) {
    memset( &entry, 0, sizeof(entry) );
    iov[0].buf = (char *)tmpconn->buf;
    iov[0].len = tmpconn->count;
    entry.iov = iov;
    entry.niov = 1;
    sts = log_read( &log, id, &entry, 1, &ne );
    if( sts || (ne == 0) ) {
      eof = 1;
      break;
    }

    /* check this entry will fit in requested xdr buffer */
    xdrlen = 4 + 8 + 8 + 8 + 4 + 4 + entry.iov[0].len;
    if( inc->xdr.offset + xdrlen > xdrcount ) break;
    
    /* encode entry */
    sts = xdr_encode_boolean( &inc->xdr, 1 );
    sts = xdr_encode_uint64( &inc->xdr, entry.id );
    sts = xdr_encode_uint64( &inc->xdr, entry.prev_id );
    sts = xdr_encode_uint64( &inc->xdr, entry.seq );
    sts = xdr_encode_uint32( &inc->xdr, entry.flags );
    sts = xdr_encode_opaque( &inc->xdr, (uint8_t *)entry.iov[0].buf, entry.iov[0].len );
    
    id = entry.id;
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  
  log_close( &log );
  
 done:
  xdr_encode_boolean( &inc->xdr, eof );
  if( tmpconn ) rpc_conn_release( tmpconn );
  rpc_complete_accept_reply( inc, handle );

  return 0;
}

static int nls_proc_write( struct rpc_inc *inc ) {
  int handle, sts, successp, buflen;
  uint32_t flags;
  uint64_t hshare, id;
  char *bufp = NULL;
  struct nls_share share;
  struct log_s log;
  struct log_prop prop;
  struct log_entry e;
  struct log_iov iov[1];

  /* only allow authenticated clients */
  if( inc->msg.u.call.auth.flavour != RPC_AUTH_HRAUTH ) {
      return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }
  
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
  struct nls_notify notify;
  uint8_t cookie[NLS_MAX_COOKIE];
  
  sts = xdr_decode_uint64( &inc->xdr, &hostid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( !sts ) sts = xdr_decode_fixed( &inc->xdr, cookie, NLS_MAX_COOKIE );
  if( sts ) rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  /* lookup context or add new one */
  sts = nls_notify_by_hshare( hostid, hshare, &notify );
  if( sts ) {
    memset( &notify, 0, sizeof(notify) );
    notify.hostid = hostid;
    notify.hshare = hshare;
    memcpy( notify.cookie, cookie, NLS_MAX_COOKIE );
    sts = nls_notify_add( &notify );
  }
  notify.seq = nls_share_seqno( notify.hshare, &notify.lastid );
  notify.timestamp = time( NULL );
  nls_notify_set( &notify );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );  
  xdr_encode_uint64( &inc->xdr, notify.seq );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}




static void nls_call_read( uint64_t hostid, uint64_t hshare, uint64_t seq, uint64_t lastid, int xdrcount );
struct nls_read_cxt {
  uint64_t hostid;
  uint64_t hshare;
  uint64_t seq;
  uint64_t lastid;
};

static void nls_read_cb( struct xdr_s *xdr, void *cxt ) {
  int sts, b;
  struct log_s log;
  uint64_t id, lastid, previd, seq;
  uint32_t flags;
  char *bufp = NULL;
  int lenp;
  struct log_entry e;
  struct log_iov iov[1];
  struct nls_read_cxt *nlscxtp = (struct nls_read_cxt *)cxt;
  int logopen = 0;
  struct nls_remote remote;
  struct nls_share rshare;
  struct log_prop prop;
  int nmsgs = 0, eof;
  
  /* do nothing if call timed out? */
  if( !xdr ) {
    rpc_log( RPC_LOG_ERROR, "nls_read_cb timeout" );
    goto done;
  }

  /* decode results */
  sts = xdr_decode_boolean( xdr, &b );
  if( sts || !b ) {
    rpc_log( RPC_LOG_ERROR, "error stauts" );
    goto done;
  }

  sts = nls_decode_prop( xdr, &rshare, &prop );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "nls_decode_prop failed" );
    goto done;
  }
  rpc_log( RPC_LOG_DEBUG, "reading entries for log hshare=%"PRIx64" seq=%"PRIu64" lastid=%"PRIx64"",
	   rshare.hshare, prop.seq, prop.last_id );
  
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
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) goto done;
  while( b ) {
    sts = xdr_decode_uint64( xdr, &id );
    if( !sts ) sts = xdr_decode_uint64( xdr, &previd );
    if( !sts ) sts = xdr_decode_uint64( xdr, &seq );
    if( !sts ) sts = xdr_decode_uint32( xdr, &flags );
    if( !sts ) sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&bufp, &lenp );
    if( sts ) {
      rpc_log( RPC_LOG_ERROR, "Failed to decode" );
      goto done;
    }

    memset( &e, 0, sizeof(e) );
    iov[0].buf = bufp;
    iov[0].len = lenp;
    e.iov = iov;
    e.niov = 1;
    e.flags = flags;      
    log_write( &log, &e );

    lastid = id;
    nmsgs++;
    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) goto done;
  }
  sts = xdr_decode_boolean( xdr, &eof );
  
  /* update remote descriptor */
  sts = nls_remote_by_hshare( rshare.hshare, &remote );
  if( sts ) goto done;

  strncpy( remote.name, rshare.name, sizeof(remote.name) - 1 );
  remote.seq = prop.seq;
  remote.lastid = prop.last_id;
  remote.timestamp = time( NULL ) + glob.prop.poll_timeout;
  nls_remote_set( &remote );

  /* Did we read all available messages? If not then continue */
  if( eof || nmsgs == 0 || seq == nlscxtp->seq ) {
    goto done;
  }

  nlscxtp->lastid = lastid;
  nls_call_read( nlscxtp->hostid, nlscxtp->hshare, nlscxtp->seq, nlscxtp->lastid, 32*1024 );
  
 done:
  if( logopen ) log_close( &log );
  free( cxt );
}

/* send a read command to server */
static void nls_call_read( uint64_t hostid, uint64_t hshare, uint64_t seq, uint64_t lastid, int xdrcount ) {
  int sts;
  struct nls_read_cxt *nlscxtp;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  uint8_t xdr_buf[32];
  
  rpc_log( RPC_LOG_DEBUG, "nls_call_read hostid=%"PRIx64" hshare=%"PRIx64" seq=%"PRIu64" lastid=%"PRIx64" xdrcount=%u",
	   hostid, hshare, seq, lastid, xdrcount );

  nlscxtp = malloc( sizeof(*nlscxtp) );
  nlscxtp->hostid = hostid;
  nlscxtp->hshare = hshare;
  nlscxtp->lastid = lastid;
  nlscxtp->seq = seq;

  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = NLS_RPC_PROG;
  hcall.vers = NLS_RPC_VERS;
  hcall.proc = 3;
  hcall.donecb = nls_read_cb;
  hcall.cxt = nlscxtp;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  hcall.retry = 2;
  xdr_init( &xdr, xdr_buf, sizeof(xdr_buf) );
  xdr_encode_uint64( &xdr, hshare );
  xdr_encode_uint64( &xdr, lastid );
  xdr_encode_uint32( &xdr, xdrcount );  
  sts = hrauth_call_udp( &hcall, &xdr );
  if( sts ) {
    free( nlscxtp );
    rpc_log( RPC_LOG_ERROR, "nls_call_read: hrauth_call failed" );
  }

}

static void nls_call_notreg_cb( struct xdr_s *xdr, void *cxt ) {
  if( !xdr ) {
    rpc_log( RPC_LOG_ERROR, "nls_call_notreg_cb: timeout" );
    goto done;
  }

 done:
  return;
}

/* send a read command to server */
static void nls_call_notreg( uint64_t hostid, uint64_t hshare, uint8_t *cookiep ) {
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  uint8_t cookie[NLS_MAX_COOKIE];
  uint8_t xdr_buf[64];
  struct hostreg_prop prop;
  
  rpc_log( RPC_LOG_DEBUG, "nls_call_notreg hostid=%"PRIx64" hshare=%"PRIx64"", hostid, hshare );

  sts = hostreg_prop( &prop );
  if( sts ) return;

  memset( cookie, 0, sizeof(cookie) );

  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = NLS_RPC_PROG;
  hcall.vers = NLS_RPC_VERS;
  hcall.proc = 5;
  hcall.donecb = nls_call_notreg_cb;
  hcall.cxt = NULL;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  hcall.retry = 2;
  xdr_init( &xdr, xdr_buf, sizeof(xdr_buf) );
  xdr_encode_uint64( &xdr, prop.localid );
  xdr_encode_uint64( &xdr, hshare );
  xdr_encode_fixed( &xdr, cookiep ? cookiep : cookie, NLS_MAX_COOKIE );
  sts = hrauth_call_udp( &hcall, &xdr );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "nls_call_notreg: hrauth_call failed" );
  }
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
  uint8_t cookie[NLS_MAX_COOKIE];
  
  sts = xdr_decode_uint64( &inc->xdr, &hostid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &seq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &lastid );
  if( !sts ) sts = xdr_decode_fixed( &inc->xdr, cookie, NLS_MAX_COOKIE );
  if( sts ) goto done;

  /* lookup remote entry */
  sts = nls_remote_by_hshare( hshare, &remote );
  if( sts ) goto done;
  
  /* compare against currently held info */
  if( remote.seq == seq ) goto done;

  /* issue async call to read missing log entries */
  nls_call_read( hostid, hshare, seq, remote.lastid, 32*1024 );

  sts = 0;
 done:
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
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

  nls_prop( &glob.prop );
  
  now = time( NULL );
  n = nls_remote_list( remote, NLS_MAX_REMOTE );
  for( i = 0; i < n; i++ ) {
    if( (remote[i].timestamp == 0) || (remote[i].timestamp < now) ) {
      /* ask for some entries */
      nls_call_read( remote[i].hostid, remote[i].hshare, remote[i].seq, remote[i].lastid, 32*1024 );
      
      /* ask for a callback */
      rpc_log( RPC_LOG_DEBUG, "nls_clt_iter_cb nls_call_notreg hostid=%"PRIx64" hshare=%"PRIx64"", remote[i].hostid, remote[i].hshare );
      nls_call_notreg( remote[i].hostid, remote[i].hshare, NULL );
      
      /* schedule to ask again later */
      remote[i].timestamp = now + glob.prop.poll_timeout;
      nls_remote_set( &remote[i] );      
    }
  }

  iter->timeout = rpc_now() + glob.prop.poll_timeout*1000;  
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

static uint64_t nls_remote_seqno( uint64_t hshare, uint64_t *lastid ) {
  int sts;
  struct log_s log;
  struct log_prop prop;
  uint64_t seq = 0;
  struct nls_remote remote;

  if( lastid ) *lastid = 0;
  
  sts = nls_remote_by_hshare( hshare, &remote );
  if( sts ) return 0;
  
  sts = nls_remote_open( &remote, &log );
  if( sts ) return 0;

  sts = log_prop( &log, &prop );
  if( sts ) goto done;

  seq = prop.seq;
  if( lastid ) *lastid = prop.last_id;
  
 done:
  log_close( &log );
  return seq;  
}

struct nls_notify_cxt {
    uint64_t hostid;
    uint64_t hshare;
    uint64_t tag;
};

static void nls_notify_cb( struct xdr_s *xdr, void *cxt ) {
    int sts, b;
    struct nls_notify_cxt *ncxt = (struct nls_notify_cxt *)cxt;
    
    if( !xdr ) goto done;

    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) goto done;
    if( !b ) {
	/* doesn't want notifications for this hshare */
	rpc_log( RPC_LOG_DEBUG, "Deleting notification context hostid=%"PRIx64" hshare=%"PRIx64"", ncxt->hostid, ncxt->hshare );
	
	nls_notify_rem( ncxt->tag );
    }
    
done:
    free( ncxt );
    return;
}

/* send a notification callback to client */
static void nls_call_notify( struct nls_notify *notify ) {
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  uint8_t xdr_buf[64];
  struct hostreg_prop prop;
  struct nls_notify_cxt *ncxt;
  
  rpc_log( RPC_LOG_DEBUG, "nls_call_notify hostid=%"PRIx64" hshare=%"PRIx64"", notify->hostid, notify->hshare );

  sts = hostreg_prop( &prop );
  if( sts ) return;

  ncxt = malloc( sizeof(*ncxt) );
  ncxt->hostid = notify->hostid;
  ncxt->hshare = notify->hshare;
  ncxt->tag = notify->tag;

  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = notify->hostid;
  hcall.prog = NLS_RPC_PROG;
  hcall.vers = NLS_RPC_VERS;
  hcall.proc = 6;
  hcall.donecb = nls_notify_cb;
  hcall.cxt = ncxt;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  hcall.retry = 2;
  xdr_init( &xdr, xdr_buf, sizeof(xdr_buf) );
  xdr_encode_uint64( &xdr, prop.localid );
  xdr_encode_uint64( &xdr, notify->hshare );
  xdr_encode_uint64( &xdr, notify->seq );
  xdr_encode_uint64( &xdr, notify->lastid );
  xdr_encode_fixed( &xdr, notify->cookie, NLS_MAX_COOKIE );
  sts = hrauth_call_udp( &hcall, &xdr );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "nls_call_notify: hrauth_call failed" );
    free( ncxt );
  }
    
}

/*
 * send out change notifications to any registered clients 
 */
static void nls_svr_iter_cb( struct rpc_iterator *iter ) {
  int i, n;
  struct nls_notify notify[NLS_MAX_NOTIFY];
  uint64_t seq, lastid;

  n = nls_notify_list( notify, NLS_MAX_NOTIFY );
  for( i = 0; i < n; i++ ) {
    /* compare seqno against stored seqno */
    seq = nls_share_seqno( notify[i].hshare, &lastid );
    if( notify[i].seq != seq ) {      
      notify[i].seq = seq;
      notify[i].lastid = lastid;
      nls_notify_set( &notify[i] );

      rpc_log( RPC_LOG_DEBUG, "nls_svr_iter_cb nls_call_notify hostid=%"PRIx64"", notify[i].hostid );
      nls_call_notify( &notify[i] );
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

  nls_prop( &glob.prop );
  
  /* reset all remote timestamps */
  n = nls_remote_list( remote, NLS_MAX_REMOTE );
  for( i = 0; i < n; i++ ) {
    remote[i].timestamp = 0;
    remote[i].seq = nls_remote_seqno( remote[i].hshare, &remote[i].lastid );
    nls_remote_set( &remote[i] );
  }  
  
  rpc_program_register( &nls_prog );
  rpc_iterator_register( &nls_clt_iter );
  rpc_iterator_register( &nls_svr_iter );
}
