
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS

#include <WinSock2.h>
#include <Windows.h>
#include <iphlpapi.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>

#ifndef WIN32
#include <sys/types.h>
#include <ifaddrs.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include "hostreg.h"
#include "hrauth.h"
#include <mmf.h>
#include <sec.h>
#include <sec.h>


static int hrauth_common( uint64_t remoteid, uint8_t *common ) {
  int sts;
  struct hostreg_host host;
  struct hostreg_prop prop;
  struct sec_buf buf[3];
  
  hostreg_prop( &prop );
  sts = hostreg_host_by_id( remoteid, &host );
  if( sts ) return sts;

  sec_buf_init( &buf[0], (char *)prop.privkey, prop.privlen );
  sec_buf_init( &buf[1], (char *)host.pubkey, host.publen );
  sec_buf_init( &buf[2], (char *)common, SEC_ECDH_MAX_COMMON );
  return ecdh_common( &buf[0], &buf[1], &buf[2] );
}

int hrauth_init( struct hrauth_context *cxt, uint64_t remoteid ) {
  int sts;
  
  memset( cxt, 0, sizeof(*cxt) );

  cxt->remoteid = remoteid;
  sts = hrauth_common( remoteid, cxt->key );
  if( sts ) return sts;
  
  cxt->cipher = HRAUTH_CIPHER_AES128|HRAUTH_DIGEST_SHA1;
  cxt->window = 500;
  cxt->service = HRAUTH_SERVICE_NONE;

  return 0;
}

static int hrauth_encrypt( uint8_t *buf, int size, uint8_t *key, int cipher ) {
  
  switch( cipher & HRAUTH_CIPHER_MASK ) {
  case HRAUTH_CIPHER_AES128:
    aes_encrypt( key, buf, size );
    break;
  default:
    return -1;
  }
  
  return 0;
}
static int hrauth_decrypt( uint8_t *buf, int size, uint8_t *key, int cipher ) {
    
  switch( cipher & HRAUTH_CIPHER_MASK ) {
  case HRAUTH_CIPHER_AES128:
    aes_decrypt( key, buf, size );
    break;
  default:
    return -1;
  }
  
  return 0;

}

static int hrauth_digest( uint8_t *hash, uint8_t *buf, int size, uint8_t *key, int digest ) {
  struct sec_buf iov[1];
  
  switch( digest & HRAUTH_DIGEST_MASK ) {
  case HRAUTH_DIGEST_SHA1:
    sec_buf_init( &iov[0], (char *)buf, size );
    sha1_hmac( hash, key, iov, 1 );
    break;
  default:
    return -1;
  }

  return 0;
}

/* xdr encoders */
static int hrauth_encode_auth( struct xdr_s *xdr, struct hrauth_auth *x ) {
  xdr_encode_int32( xdr, x->tag );
  switch( x->tag ) {
  case HRAUTH_NICKNAME:
    xdr_encode_uint32( xdr, x->u.nickname );
    break;
  case HRAUTH_FULL:
    xdr_encode_uint64( xdr, x->u.full.id );
    xdr_encode_uint32( xdr, x->u.full.cipher );
    xdr_encode_fixed( xdr, x->u.full.ecred, sizeof(x->u.full.ecred) );
    break;
  default:
    return -1;
  }
  return 0;
}
static int hrauth_decode_auth( struct xdr_s *xdr, struct hrauth_auth *x ) {
  int sts;
  xdr_decode_int32( xdr, &x->tag );
  switch( x->tag ) {
  case HRAUTH_NICKNAME:
    sts = xdr_decode_uint32( xdr, &x->u.nickname );
    if( sts ) return sts;
    break;
  case HRAUTH_FULL:
    sts = xdr_decode_uint64( xdr, &x->u.full.id );
    if( sts ) return sts;
    sts = xdr_decode_uint32( xdr, &x->u.full.cipher );
    if( sts ) return sts;
    sts = xdr_decode_fixed( xdr, x->u.full.ecred, sizeof(x->u.full.ecred) );
    if( sts ) return sts;
    break;
  default:
    return -1;
  }
  return 0;
}
static int hrauth_encode_verf( struct xdr_s *xdr, struct hrauth_verf *x ) {
  xdr_encode_uint32( xdr, x->nonce );
  xdr_encode_uint32( xdr, x->timestamp );
  xdr_encode_uint32( xdr, x->tverf );
  xdr_encode_uint32( xdr, x->nickname );
  return 0;
}
static int hrauth_decode_verf( struct xdr_s *xdr, struct hrauth_verf *x ) {
  xdr_decode_uint32( xdr, &x->nonce );
  xdr_decode_uint32( xdr, &x->timestamp );
  xdr_decode_uint32( xdr, &x->tverf );
  xdr_decode_uint32( xdr, &x->nickname );
  return 0;
}
static int hrauth_encode_cred( struct xdr_s *xdr, struct hrauth_cred *x ) {
  xdr_encode_uint32( xdr, x->nonce );
  xdr_encode_fixed( xdr, x->session_key, 32 );
  xdr_encode_uint32( xdr, x->service );
  xdr_encode_uint32( xdr, x->window );
  xdr_encode_uint32( xdr, x->cipher );
  return 0;
}
static int hrauth_decode_cred( struct xdr_s *xdr, struct hrauth_cred *x ) {
  xdr_decode_uint32( xdr, &x->nonce );
  xdr_decode_fixed( xdr, x->session_key, 32 );
  xdr_decode_uint32( xdr, &x->service );
  xdr_decode_uint32( xdr, &x->window );
  xdr_decode_uint32( xdr, &x->cipher );
  return 0;
}






static int hrauth_cauth( struct rpc_provider *pvr, struct rpc_msg *msg, void *pcxt ) {
  /* generate client authenticator, write to msg->u.call.auth */

  struct hrauth_context *sa = (struct hrauth_context *)pcxt;
  struct hrauth_auth auth;
  struct xdr_s tmpx;
  int sts;
  struct hrauth_verf verf;
  struct hostreg_prop prop;
  
  memset( &auth, 0, sizeof(auth) );  
  if( sa->nickname ) {
    auth.tag = HRAUTH_NICKNAME;
    auth.u.nickname = sa->nickname;
  } else {
    struct hrauth_cred cred;
    
    auth.tag = HRAUTH_FULL;
    hostreg_prop( &prop );
    auth.u.full.id = prop.localid;
    auth.u.full.cipher = sa->cipher;
    
    /* generate and encode credential */
    memset( &cred, 0, sizeof(cred) );
    sec_rand( &cred.nonce, 4 );
    sec_rand( sa->session_key, sizeof(sa->session_key) );
    memcpy( cred.session_key, sa->session_key, sizeof(cred.session_key) );
    cred.service = sa->service;
    cred.window = sa->window;
    cred.cipher = sa->cipher;
    xdr_init( &tmpx, auth.u.full.ecred, 64 );
    hrauth_encode_cred( &tmpx, &cred );

    /* encrypt */
    hrauth_encrypt( tmpx.buf, 64, sa->key, sa->cipher );
  }

  /* encode to authenticator */
  xdr_init( &tmpx, msg->u.call.auth.data, RPC_MAX_OPAQUE_AUTH );
  hrauth_encode_auth( &tmpx, &auth );
  msg->u.call.auth.flavour = RPC_AUTH_HRAUTH;
  msg->u.call.auth.len = tmpx.offset;

  /* generate verifier */
  memset( &verf, 0, sizeof(verf) );
  sec_rand( &verf.nonce, 4 );
  sa->timestamp = (uint32_t)time( NULL );
  verf.timestamp = sa->timestamp;
  verf.tverf = verf.timestamp - 1;
  verf.nickname = 0;
  xdr_init( &tmpx, msg->u.call.verf.data, sizeof(msg->u.call.verf.data) );
  hrauth_encode_verf( &tmpx, &verf );
  hrauth_encrypt( tmpx.buf, tmpx.offset, sa->session_key, sa->cipher );
  msg->u.call.verf.flavour = RPC_AUTH_HRAUTH;
  msg->u.call.verf.len = tmpx.offset;
  
  return 0;
}

static int hrauth_cverf( struct rpc_provider *pvr, struct rpc_msg *msg, void *pcxt ) {
  /* recevive client verifier, parse msg->u.reply.u.accept.u.verf */
  struct hrauth_context *sa = (struct hrauth_context *)pcxt;   
  int sts;
  struct hrauth_verf verf;
  struct xdr_s tmpx;
  
  xdr_init( &tmpx, msg->u.reply.u.accept.verf.data, msg->u.reply.u.accept.verf.len );
  /* decrypt */
  hrauth_decrypt( tmpx.buf, tmpx.count, sa->session_key, sa->cipher );

  /* decode */
  sts = hrauth_decode_verf( &tmpx, &verf );
  if( sts ) return sts;

  /* validate verifier */
  if( verf.timestamp != sa->timestamp ) return -1;
  if( verf.tverf != (verf.timestamp - 1) ) return -1;
  
  if( sa->nickname == 0 ) {
    sa->nickname = verf.nickname;
  } else if( verf.nickname != sa->nickname ) {
    return -1;
  }

  /* all good */
  return 0;
}

static int hrauth_taste( struct rpc_provider *pvr, int flavour ) {
  /* server taste flavour. return 0 if we like it, -1 otherwise */
  if( flavour == RPC_AUTH_HRAUTH ) return 1;
  return 0;
}



static struct hrauth_context shcxttab[32];
static struct hrauth_context *hrauth_context_by_nickname( uint32_t nickname ) {
  int i;
  for( i = 0; i < 32; i++ ) {
    if( shcxttab[i].nickname == nickname ) return &shcxttab[i];
  }
  return NULL;
}
static struct hrauth_context *hrauth_context_alloc( void ) {
  int i, oldest;
  uint32_t age;
  age = 0;
  oldest = 0;
  for( i = 0; i < 32; i++ ) {
    if( shcxttab[i].nickname == 0 ) return &shcxttab[i];
    if( age == 0 || (shcxttab[i].timestamp < age) ) {
      age = shcxttab[i].timestamp;
      oldest = i;
    }
  }
  i = oldest;
  memset( &shcxttab[i], 0, sizeof(shcxttab[i]) );
  return &shcxttab[i];
}


static int hrauth_sauth( struct rpc_provider *pvr, struct rpc_msg *msg, void **pcxt ) {
  /* server auth. validate client authenticator and assign authentication context */
  struct hrauth_context *sa;
  struct xdr_s tmpx;
  int sts;
  struct hrauth_auth auth;
  struct hrauth_verf verf;
  uint32_t now;
  struct hrauth_cred cred;
  
  xdr_init( &tmpx, msg->u.call.auth.data, msg->u.call.auth.len );
  sts = hrauth_decode_auth( &tmpx, &auth );
  if( sts ) return sts;
  switch( auth.tag ) {
  case HRAUTH_NICKNAME:
    /* lookup existing context */
    sa = hrauth_context_by_nickname( auth.u.nickname );
    if( !sa ) return -1;
    rpc_log( RPC_LOG_DEBUG, "hrauth: nickname=%d", auth.u.nickname );
    break;
  case HRAUTH_FULL:
    /* allocate new context */
    sa = hrauth_context_alloc();
    sa->cipher = auth.u.full.cipher;
    sa->remoteid = auth.u.full.id;
    xdr_init( &tmpx, auth.u.full.ecred, 64 );

    /* derive common key */
    sts = hrauth_common( auth.u.full.id, sa->key );
    if( sts ) {
      rpc_log( RPC_LOG_DEBUG, "hrauth: unknown host %"PRIx64"", auth.u.full.id );
      return sts;
    }
    hrauth_decrypt( tmpx.buf, tmpx.count, sa->key, sa->cipher );
    hrauth_decode_cred( &tmpx, &cred );
    memcpy( sa->session_key, cred.session_key, 32 );
    sa->service = cred.service;
    sa->window = cred.window;
    sa->cipher = cred.cipher;
    sec_rand( &sa->nickname, 4 );
    rpc_log( RPC_LOG_DEBUG, "hrauth: full service=%d window=%d cipher=%08x nickname=%d",
	     cred.service,
	     cred.window,
	     cred.cipher,
	     sa->nickname );
    break;
  default:
    return -1;
  }
  
  /* validate verifier */
  xdr_init( &tmpx, msg->u.call.verf.data, msg->u.call.verf.len );
  hrauth_decrypt( tmpx.buf, tmpx.count, sa->session_key, sa->cipher );
  hrauth_decode_verf( &tmpx, &verf );

  now = time( NULL );
  if( ((now > verf.timestamp) ? (now - verf.timestamp) : (verf.timestamp - now)) > sa->window ) {
    /* clock skew */
    rpc_log( RPC_LOG_DEBUG, "clock skew now=%d verf.timestamp=%d", (int)now, (int)verf.timestamp );
    sa->nickname = 0;
    return -1;
  }
  if( verf.tverf != (verf.timestamp - 1) ) {
    rpc_log( RPC_LOG_DEBUG, "invalid tverf %d != %d", verf.tverf, verf.timestamp );
    sa->nickname = 0;
    return -1;
  }

  sa->timestamp = now;
  
  /* generate reply verifier */
  sec_rand( &verf.nonce, 4 );
  verf.nickname = sa->nickname;
  xdr_init( &tmpx, (uint8_t *)&pvr->rverf.data, sizeof(pvr->rverf.data) );
  hrauth_encode_verf( &tmpx, &verf );
  hrauth_encrypt( tmpx.buf, tmpx.offset, sa->session_key, sa->cipher );
  pvr->rverf.flavour = RPC_AUTH_HRAUTH;
  pvr->rverf.len = tmpx.offset;

  *pcxt = sa;
  
  return 0;
}


/* ----------- arg/res modification functions ------------- */

static int hrauth_mout( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  struct hrauth_context *sa;
  uint8_t hash[32];
  int count;

  (void)(pvr);

  sa = (struct hrauth_context *)pcxt;

  if( sa->service == HRAUTH_SERVICE_NONE ) return 0;

  count = end - start;
  
  if( sa->service == HRAUTH_SERVICE_PRIV ) {
    /* pad to multiple of 16 */
    if( count % 16 ) {
      memset( xdr->buf + start + count, 0, 16 - (count % 16) );
      end += 16 - (count % 16);
      xdr->offset = end;

      count = end - start;
    }

    /* encrypt */
    hrauth_encrypt( xdr->buf + start, count, sa->session_key, sa->cipher );
  }

  /* now prepend the hash */
  memset( hash, 0, sizeof(hash) );
  hrauth_digest( hash, xdr->buf + start, count, sa->session_key, sa->cipher );
  memmove( xdr->buf + start + sizeof(hash), xdr->buf + start, count );
  memcpy( xdr->buf + start, hash, sizeof(hash) );
  xdr->offset += sizeof(hash);

  return 0;
}
static int hrauth_min( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  struct hrauth_context *sa = (struct hrauth_context *)pcxt;
  uint8_t hash[32];
  int count;

  (void)(pvr);

  if( sa->service == HRAUTH_SERVICE_NONE ) return 0;

  if( start != xdr->offset ) return -1;

  count = end - xdr->offset;
  if( count < (int)sizeof(hash) ) return -1;

  /* Remove the hash */
  xdr->offset += sizeof(hash);
  count -= sizeof(hash);

  /* Check hash */
  memset( hash, 0, sizeof(hash) );
  hrauth_digest( hash, xdr->buf + xdr->offset, count, sa->session_key, sa->cipher );
  if( memcmp( xdr->buf + start, hash, sizeof(hash) ) != 0 ) return -1;

  if( sa->service == HRAUTH_SERVICE_PRIV ) {
    /* decrypt payload */
    if( count % 16 ) return -1;
    hrauth_decrypt( xdr->buf + xdr->offset, count, sa->session_key, sa->cipher );
  }

  return 0;
}

int hrauth_priv_header( struct hrauth_context *sa, struct xdr_s *xdr, int start, int end ) {
  int count;
  int data_start;

  data_start = start + 32;
  count = end - data_start;
  if( count < 0 ) return -1;

  /* generate outgoing header */

  /* pad to multiple of 16 */
  if( count % 16 ) {
    memset( xdr->buf + data_start + count, 0, 16 - (count % 16) );
    end += 16 - (count % 16);
    count = end - data_start;
  }

  /* encrypt */
  hrauth_encrypt( xdr->buf + data_start, count, sa->session_key, sa->cipher );

  /* now prepend the hash */
  hrauth_digest( xdr->buf + start, xdr->buf + data_start, count, sa->session_key, sa->cipher );

  return 0;
}

static int hrauth_cmargs( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  return hrauth_mout( pvr, xdr, start, end, pcxt );
}
static int hrauth_cmres( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  return hrauth_min( pvr, xdr, start, end, pcxt );
}

static int hrauth_smargs( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  return hrauth_min( pvr, xdr, start, end, pcxt );
}
static int hrauth_smres( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  return hrauth_mout( pvr, xdr, start, end, pcxt );
}


static struct rpc_provider hrauthpvr = {
  NULL,
  RPC_AUTH_HRAUTH,

  hrauth_cauth,
  hrauth_cverf,
  hrauth_cmargs,
  hrauth_cmres,
  
  hrauth_taste,
  hrauth_sauth,
  hrauth_smargs,
  hrauth_smres
};

struct rpc_provider *hrauth_provider( void ) {
  return &hrauthpvr;
}



/* hrauth private rpc program */
static int hrauth_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int hostreg_encode_host( struct xdr_s *xdr, struct hostreg_host *x ) {
  int i;
  xdr_encode_uint64( xdr, x->id );
  xdr_encode_string( xdr, x->name );
  xdr_encode_opaque( xdr, x->pubkey, x->publen );
  xdr_encode_uint32( xdr, x->naddr );
  for( i = 0; i < x->naddr; i++ ) {
    xdr_encode_uint32( xdr, x->addr[i] );
  }
  return 0;
}
static int hostreg_decode_host( struct xdr_s *xdr, struct hostreg_host *x ) {
  int sts, i;
  memset( x, 0, sizeof(*x) );
  sts = xdr_decode_uint64( xdr, &x->id );
  if( sts ) return sts;
  sts = xdr_decode_string( xdr, x->name, sizeof(x->name) );
  if( sts ) return sts;
  x->publen = sizeof(x->pubkey);
  sts = xdr_decode_opaque( xdr, x->pubkey, (int *)&x->publen );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &x->naddr );
  if( sts ) return sts;
  if( x->naddr > HOSTREG_MAX_ADDR ) return -1;
  for( i = 0; i < x->naddr; i++ ) {
    sts = xdr_decode_uint32( xdr, &x->addr[i] );
    if( sts ) return sts;
  }
  return 0;
}

static int hrauth_proc_local( struct rpc_inc *inc ) {
  int handle;
  struct hostreg_host host;
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  hostreg_host_local( &host );
  hostreg_encode_host( &inc->xdr, &host );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}
static int hrauth_proc_list( struct rpc_inc *inc ) {
  int handle;
  /* XXX: reserved for future use. Should return list of registered hosts */
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_PROC_UNAVAIL, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct rpc_proc hrauth_procs[] = {
  { 0, hrauth_proc_null },
  { 1, hrauth_proc_local },
  { 2, hrauth_proc_list },
  { 0, NULL }
};

static struct rpc_version hrauth_vers = {
  NULL, HRAUTH_VERSION, hrauth_procs
};

static struct rpc_program hrauth_prog = {
  NULL, HRAUTH_PROGRAM, &hrauth_vers
};





void hrauth_register( void ) {
  hostreg_open();
  rpc_provider_register( hrauth_provider() );
  rpc_program_register( &hrauth_prog );
}
