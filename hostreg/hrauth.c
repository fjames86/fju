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

#include <fju/hostreg.h>
#include <fju/hrauth.h>
#include <fju/mmf.h>
#include <fju/sec.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/log.h>
#include <fju/freg.h>

static void hrauth_conn_init( void );

static void hrauth_log( int lvl, char *fmt, ... ) {
  static struct log_s log;
  static int initialized = 0;
  int sts;
  va_list args;

  if( !initialized ) {
    sts = log_open( NULL, NULL, &log );
    if( sts ) return;
    log.ltag = HRAUTH_RPC_PROG;
    initialized = 1;
  }
  
  va_start( args, fmt );
  log_writev( &log, lvl, fmt, args );
  va_end( args );
}



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
  static uint32_t default_window;
  int sts;

  if( !default_window ) {
    default_window = 500;
    sts = freg_ensure( NULL, 0, "/fju/hrauth/window", FREG_TYPE_UINT32, (char *)&default_window, sizeof(default_window), NULL );
  }
  
  memset( cxt, 0, sizeof(*cxt) );

  cxt->remoteid = remoteid;
  sts = hrauth_common( remoteid, cxt->key );
  if( sts ) return sts;
  
  cxt->cipher = HRAUTH_CIPHER_AES128|HRAUTH_DIGEST_SHA1;
  cxt->window = default_window;
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
  if( sts ) {
    hrauth_log( LOG_LVL_DEBUG, "verf xdr error" );
    return sts;
  }

  /* validate verifier */
  if( verf.timestamp != sa->timestamp ) {
    hrauth_log( LOG_LVL_DEBUG, "bad timestamp timestamp=%u expected=%u tverf=%u nickname=%u",
	     verf.timestamp, sa->timestamp, verf.tverf, verf.nickname );
    return -1;
  }
  if( verf.tverf != (verf.timestamp - 1) ) {
    hrauth_log( LOG_LVL_DEBUG, "bad tverf" );
    return -1;
  }
  
  if( sa->nickname == 0 ) {
    sa->nickname = verf.nickname;
  } else if( verf.nickname != sa->nickname ) {
    hrauth_log( LOG_LVL_DEBUG, "hrauth_cverf unexpected nickname" );
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
    hrauth_log( LOG_LVL_TRACE, "hrauth: nickname=%d", auth.u.nickname );
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
      hrauth_log( LOG_LVL_DEBUG, "hrauth_common failed: host=%"PRIx64"", auth.u.full.id );
      return sts;
    }
    hrauth_decrypt( tmpx.buf, tmpx.count, sa->key, sa->cipher );
    hrauth_decode_cred( &tmpx, &cred );
    memcpy( sa->session_key, cred.session_key, 32 );
    sa->service = cred.service;
    sa->window = cred.window;
    sa->cipher = cred.cipher;
    sec_rand( &sa->nickname, 4 );
    hrauth_log( LOG_LVL_TRACE, "hrauth: full service=%d window=%d cipher=%08x nickname=%d", cred.service, cred.window, cred.cipher, sa->nickname );
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
    hrauth_log( LOG_LVL_DEBUG, "clock skew now=%d verf.timestamp=%d", (int)now, (int)verf.timestamp );
    sa->nickname = 0;
    return -1;
  }
  if( verf.tverf != (verf.timestamp - 1) ) {
    hrauth_log( LOG_LVL_DEBUG, "invalid tverf %d != %d", verf.tverf, verf.timestamp );
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
  struct hrauth_context *hcxt;
  struct hostreg_host *hlist;
  int n, m;
  
  /* check authenticated */
  if( !inc->pvr || (inc->pvr->flavour != RPC_AUTH_HRAUTH) ) {
    hrauth_log( LOG_LVL_WARN, "hrauth_proc_list bad authentication flavour=%u", inc->pvr ? inc->pvr->flavour : 0 );
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }

  hcxt = (struct hrauth_context *)inc->pcxt;
  if( hcxt->service != HRAUTH_SERVICE_PRIV ) {
    /* only allow listing if encrypted */
    hrauth_log( LOG_LVL_WARN, "hrauth_proc_list weak service level %u", hcxt->service );    
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );    
  }

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  
  n = hostreg_host_list( NULL, 0 );
  hlist = malloc( sizeof(*hlist) * n );
  m = hostreg_host_list( hlist, n );
  if( m < n ) n = m;
  for( m = 0; m < n; m++ ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    hostreg_encode_host( &inc->xdr, &hlist[m] );
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  free( hlist );
  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

#if 0
static int hrauth_proc_invoke( struct rpc_inc *inc ) {
  int handle;
  /* 
   * XXX: reserved for future use. 
   * Should invoke a specified procedure on local machine. THis allows us 
   * to send the whole rpc header+payload in encrypted+verified form, hiding 
   * our true rpc endpoint.
   */

  /* check authenticated */
  if( !inc->pvr || (inc->pvr->flavour != RPC_AUTH_HRAUTH) ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }

  /* extract endpoint address */
  xdr_decode_uint32( &inc->xdr, &prog );
  xdr_decode_uint32( &inc->xdr, &vers );
  xdr_decode_uint32( &inc->xdr, &proc );

  /* copy over inc, replacing the parts we need*/
  inc2 = *inc;  
  inc2.msg.u.call.prog = proc;
  inc2.msg.u.call.vers = vers;
  inc2.msg.u.call.proc = proc;
  inc2.msg.u.call.auth.flavour = 0;
  inc2.msg.u.call.verf.flavour = 0;
  inc2.pvr = NULL;
  inc2.pcxt = NULL;

  /* lookup function */
  sts = rpc_program_find( prog, vers, proc, &pg, &vs, &pc );
  if( !sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_PROC_UNAVAIL, NULL, NULL );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  /* invoke */
  rpc_complete_accept_reply( inc, handle );
  return 0;
}
#endif

static int hrauth_proc_nullping( struct rpc_inc *inc ) {
  /* 
   * this proc does nothing and does not reply. it is used by the tcp 
   * connection framework to keep connections alive 
   */

  struct hrauth_context *hcxt;

  if( inc->pvr && inc->pvr->flavour == RPC_AUTH_HRAUTH ) {
    hcxt = (struct hrauth_context *)inc->pcxt;

    hrauth_log( LOG_LVL_DEBUG, "Nullping received from %"PRIx64"", hcxt->remoteid );
    hrauth_reply_tcp( hcxt, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL );
  }
  
  return 1;
}

static struct rpc_proc hrauth_procs[] = {
  { 0, hrauth_proc_null },
  { 1, hrauth_proc_local },
  { 2, hrauth_proc_list },
  { 3, hrauth_proc_nullping },  
  { 0, NULL }
};

static struct rpc_version hrauth_vers = {
  NULL, HRAUTH_RPC_VERS, hrauth_procs
};

static struct rpc_program hrauth_prog = {
  NULL, HRAUTH_RPC_PROG, &hrauth_vers
};





void hrauth_register( void ) {
  hostreg_open();
  rpc_provider_register( hrauth_provider() );
  rpc_program_register( &hrauth_prog );
  hrauth_conn_init();
}

/* ---------- simple hrauth client ---------------- */

struct hrauth_call_cxt {
  struct hrauth_call hcall;
  struct xdr_s args;
};

static void hrauth_call_cb( struct rpc_waiter *w, struct rpc_inc *inc ) {
  int sts;
  struct hrauth_call_cxt *cxt;
  struct hrauth_call *hcallp;

  cxt = (struct hrauth_call_cxt *)w->cxt;
  hcallp = &cxt->hcall;
  
  if( !hcallp->donecb ) goto done;

  /* check for timeout */
  if( !inc ) {
    hrauth_log( LOG_LVL_TRACE, "hrauth_call_cb: XID=%u timeout", w->xid );
    hcallp->donecb( NULL, hcallp->cxt );
    goto done;
  }

  /* process msg */
  sts = rpc_process_reply( inc );
  if( sts ) {
    hrauth_log( LOG_LVL_TRACE, "hrauth_call_cb: failed processing reply reply.tag=%d reply.accept.tag=%d", inc->msg.u.reply.tag, inc->msg.u.reply.u.accept.tag );
    hcallp->donecb( NULL, hcallp->cxt );
    goto done;
  }

  /* invoke callback */
  hcallp->donecb( &inc->xdr, hcallp->cxt );
  
 done:
  free( w );         /* free waiter plus other state */
}

int hrauth_call_udp_async( struct hrauth_call *hcall, struct xdr_s *args, struct hrauth_call_opts *opts ) {
  int sts, handle;
  struct rpc_waiter *w;
  struct hostreg_host host;
  struct rpc_listen *listenp = NULL;
  struct rpc_inc inc;
  struct sockaddr_in sin;
  struct hrauth_context *hcxt;
  struct hrauth_call_cxt *hcallp;
#ifdef WIN32
  SOCKET fd;
#else
  int fd;
#endif
  struct xdr_s tmpbuf;
  struct rpc_conn *conn = NULL;
  int port;
  uint32_t addridx, addrmask;
  
  /* lookup host */
  sts = hostreg_host_by_id( hcall->hostid, &host );
  if( sts ) return -1;

  /* get fd */
  if( opts && opts->mask & HRAUTH_CALL_OPT_FD ) fd = opts->fd;
  else {
    listenp = rpcd_listen_by_type( RPC_LISTEN_UDP );
    if( !listenp ) return -1;
    fd = listenp->fd;
  }

  /* get port */
  if( opts && opts->mask & HRAUTH_CALL_OPT_PORT ) {
    port = opts->port;
  } else {
    if( !listenp ) {
      listenp = rpcd_listen_by_type( RPC_LISTEN_UDP );
      if( !listenp ) return -1;
    }
    port = ntohs( listenp->addr.sin.sin_port );
  }

  /* get tmpbuf, either passed in or from tmp connection descriptor */
  if( opts && opts->mask & HRAUTH_CALL_OPT_TMPBUF ) {
    tmpbuf = opts->tmpbuf;
  } else {
    conn = rpc_conn_acquire();
    if( !conn ) return -1;
    xdr_init( &tmpbuf, conn->buf, conn->count );
  }
  
  /* prepare message */
  memset( &inc, 0, sizeof(inc) );
  xdr_init( &inc.xdr, tmpbuf.buf, tmpbuf.count );
  inc.pvr = hrauth_provider();

  /* prepare auth context */
  w = malloc( sizeof(*w) + sizeof(*hcxt) + sizeof(*hcallp) + args->offset );  
  hcxt = (struct hrauth_context *)((char *)w + sizeof(*w)); 
  sts = hrauth_init( hcxt, hcall->hostid );
  hcxt->service = hcall->service;
  inc.pcxt = hcxt;
  
  rpc_init_call( &inc, hcall->prog, hcall->vers, hcall->proc, &handle );
  xdr_encode_fixed( &inc.xdr, args->buf, args->offset );
  rpc_complete_call( &inc, handle );

  /* send */
  addridx = 0;
  addrmask = (opts && (opts->mask & HRAUTH_CALL_OPT_ADDRMASK)) ? opts->addrmask : 1;
  while( addrmask && (addridx < host.naddr) ) {
    if( addrmask & 0x1 ) {
      memset( &sin, 0, sizeof(sin) );
      sin.sin_family = AF_INET;
      sin.sin_port = htons( port );
      sin.sin_addr.s_addr = host.addr[addridx]; 
      sts = sendto( fd, inc.xdr.buf, inc.xdr.offset, 0,
		    (struct sockaddr *)&sin, sizeof(sin) );
      if( sts < 0 ) hrauth_log( LOG_LVL_ERROR, "sendto: %s", strerror( errno ) );
    }
    addridx++;
    addrmask = addrmask >> 1;
  }
  
  /* await reply - copy args so we can resend on failure */
  /* XXX: we don't do auto resends so we don't need to copy args */
  hcallp = (struct hrauth_call_cxt *)(((char *)w) + sizeof(*w) + sizeof(*hcxt));
  hcallp->hcall = *hcall;
  xdr_init( &hcallp->args, (uint8_t *)(((char *)hcallp) + sizeof(*hcallp)), args->offset );
  memcpy( hcallp->args.buf, args->buf, args->offset );
  hcallp->args.offset = args->offset;
  
  memset( w, 0, sizeof(*w) );
  w->xid = inc.msg.xid;
  w->timeout = rpc_now() + hcall->timeout;
  w->cb = hrauth_call_cb;
  w->cxt = hcallp;
  w->pvr = hrauth_provider();
  w->pcxt = hcxt;
  rpc_await_reply( w );

  /* cleanup */
  if( conn ) rpc_conn_release( conn );
    
  return 0;  
}


struct hrauth_proxy_cxt {
  struct rpc_reply_data rdata;
};

static void hrauth_proxy_udp_cb( struct xdr_s *xdr, void *cxt ) {
  struct hrauth_proxy_cxt *pcxt = (struct hrauth_proxy_cxt *)cxt;
  struct rpc_listen *listen;
  int handle;
  struct rpc_inc inc;
  char *buf = NULL;
  
  if( !xdr ) {
    hrauth_log( LOG_LVL_DEBUG, "hrauth_proxy_udp_cb timeout" );
    goto done;
  }

  /* send reply message */
  memset( &inc, 0, sizeof(inc) );
  inc.pvr = pcxt->rdata.pvr;
  inc.pcxt = pcxt->rdata.pcxt;
  if( inc.pvr ) inc.pvr->rverf = pcxt->rdata.rverf;
  buf = malloc( 512 + xdr->count );
  xdr_init( &inc.xdr, (uint8_t *)buf, 512 + xdr->count );
  rpc_init_accept_reply( &inc, pcxt->rdata.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_fixed( &inc.xdr, xdr->buf + xdr->offset, xdr->count - xdr->offset );
  rpc_complete_accept_reply( &inc, handle );

  listen = rpcd_listen_by_type( RPC_LISTEN_UDP );
  if( listen ) sendto( listen->fd, inc.xdr.buf, inc.xdr.offset, 0, (struct sockaddr *)&pcxt->rdata.raddr, pcxt->rdata.raddr_len );
  
 done:
  if( buf ) free( buf );
  free( pcxt );
  return;
}

int hrauth_call_udp_proxy( struct rpc_inc *inc, uint64_t hostid, struct xdr_s *args ) {
  int sts;
  struct hrauth_call hcall;
  struct hrauth_proxy_cxt *pcxt;

  pcxt = malloc( sizeof(*pcxt) );
  rpc_get_reply_data( inc, &pcxt->rdata );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = inc->msg.u.call.prog;
  hcall.vers = inc->msg.u.call.vers;
  hcall.proc = inc->msg.u.call.proc;
  hcall.donecb = hrauth_proxy_udp_cb;
  hcall.cxt = pcxt;
  hcall.timeout = 500;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_udp_async( &hcall, args, NULL );
  if( sts ) {
    hrauth_log( LOG_LVL_DEBUG, "hrauth_call_udp_async failed" );
    free( pcxt );
  }

  return sts;
}

/* ------ an equivalent for TCP is MUCH harder. ------ */

struct hrauth_conn {
  uint64_t hostid;  /* host id */
  uint64_t connid;  /* current rpc connection id, if any */
  uint32_t state;   /* state */
#define HRAUTH_CONN_DISCONNECTED 0
#define HRAUTH_CONN_CONNECTED    1
#define HRAUTH_CONN_CONNECTING   2
  struct hrauth_context hcxt;  /* authentication context */
  struct sockaddr_storage addr; /* connection address */
  int addrlen;
  uint32_t pingtimeout; /* keep alive timeout - call rpcbind.null if this expires */
};

static struct {
  struct hrauth_conn conn[RPC_MAX_CONN];
  int nconn;
} conndata;

/* lookup the connection for this host */
static struct hrauth_conn *hrauth_conn_by_hostid( uint64_t hostid ) {
  int i;
  for( i = 0; i < conndata.nconn; i++ ) {
    if( conndata.conn[i].hostid == hostid ) return &conndata.conn[i];
  }
  return NULL;
}

static struct hrauth_conn *hrauth_conn_by_connid( uint64_t connid ) {
  int i;
  for( i = 0; i < conndata.nconn; i++ ) {
    if( conndata.conn[i].connid == connid ) return &conndata.conn[i];
  }
  return NULL;
}

static int hrauth_connect( struct hrauth_conn *hc );


static void hrauth_conncb( rpc_conn_event_t evt, struct rpc_conn *conn ) {
  struct hrauth_conn *hc;
  int sts;
  
  hc = hrauth_conn_by_connid( conn->connid );
  if( !hc ) return;
  
  switch( evt ) {
  case RPC_CONN_CLOSE:
    /* clear connid from structure */
    if( hc->state == HRAUTH_CONN_CONNECTING ) {
      /* connect operation failed */
      hrauth_log( LOG_LVL_ERROR, "Connection failed hostid=%"PRIx64"", hc->hostid );
    } else {
      /* connection dropped */
      hrauth_log( LOG_LVL_ERROR, "Connection dropped hostid=%"PRIx64"", hc->hostid );      
    }
    
    hc->connid = 0;
    hc->state = HRAUTH_CONN_DISCONNECTED;

    /* attempt reconnect immediately */
    hrauth_log( LOG_LVL_INFO, "Attempting reconnect" );
    sts = hrauth_connect( hc );
    if( sts ) {
      hrauth_log( LOG_LVL_ERROR, "Failed to connect hostid=%"PRIx64"", hc->hostid );
      hc->state = HRAUTH_CONN_DISCONNECTED;      
    }
    
    break;
  case RPC_CONN_CONNECT:
    /* set connected state */
    hrauth_log( LOG_LVL_INFO, "Connect succeeded hostid=%"PRIx64"", hc->hostid );
    hc->state = HRAUTH_CONN_CONNECTED;
    break;
  }
  
}

static int hrauth_connect( struct hrauth_conn *hc ) {
  /* initiate connection */
  int sts;

  if( hc->state != HRAUTH_CONN_DISCONNECTED ) return -1;

  sts = hrauth_init( &hc->hcxt, hc->hostid );
  if( sts ) return sts;

  hc->state = HRAUTH_CONN_CONNECTING;
  
  sts = rpc_connect( (struct sockaddr *)&hc->addr, hc->addrlen, hrauth_conncb, NULL, &hc->connid );
  if( sts ) {
    hrauth_log( LOG_LVL_ERROR, "rpc_connect failed %s", rpc_strerror( rpc_errno() ) );
    hc->state = HRAUTH_CONN_DISCONNECTED;
    return sts;
  }

  return 0;
}


/* register to maintain a connection to this host */
int hrauth_conn_register( uint64_t hostid, struct hrauth_conn_opts *opts ) {
  int i, sts;
  struct hrauth_conn *hc;
  
  for( i = 0; i < conndata.nconn; i++ ) {
    if( conndata.conn[i].hostid == hostid ) return 0;
  }
  if( conndata.nconn >= RPC_MAX_CONN ) return -1;

  hc = (struct hrauth_conn *)&conndata.conn[conndata.nconn];
  memset( hc, 0, sizeof(*hc) );
  hc->hostid = hostid;
  hc->state = HRAUTH_CONN_DISCONNECTED;

  if( opts && opts->mask & HRAUTH_CONN_OPT_ADDR ) {
    memcpy( &hc->addr, &opts->addr, opts->addrlen );
    hc->addrlen = opts->addrlen;
  } else {
    struct sockaddr_in *sinp = (struct sockaddr_in *)&hc->addr;
    struct rpc_listen *listen;
    int port;
    struct hostreg_host host;
    
    sts = hostreg_host_by_id( hostid, &host );
    if( sts ) {
      hrauth_log( LOG_LVL_ERROR, "Failed to find host" );
      return -1;
    }

    listen = rpcd_listen_by_type( RPC_LISTEN_TCP );
    if( listen ) port = ntohs( listen->addr.sin.sin_port );
    else port = 8000; // XXX get default port from somewhere? 
    
    memset( sinp, 0, sizeof(*sinp) );
    sinp->sin_family = AF_INET;
    sinp->sin_port = htons( port );
    sinp->sin_addr.s_addr = host.addr[0]; // XXX What if there are no address? 
    hc->addrlen = sizeof(*sinp);
  }

  if( opts && opts->mask & HRAUTH_CONN_OPT_PINGTIMEOUT ) {
    hc->pingtimeout = opts->pingtimeout;
  } else {
    hc->pingtimeout = HRAUTH_CONN_PINGTIMEOUT;
  }
  
  /* initiate connection */
  sts = hrauth_connect( &conndata.conn[conndata.nconn] );
  if( sts ) {
    hrauth_log( LOG_LVL_ERROR, "hrauth_connect failed" );
    return sts;
  }
  
  conndata.nconn++;  

  return 0;  
}

/* unregister this host */
int hrauth_conn_unregister( uint64_t hostid ) {
  int i;
			  
  for( i = 0; i < conndata.nconn; i++ ) {
    if( conndata.conn[i].hostid == hostid ) {
      /* close connection */
      if( conndata.conn[i].connid ) {
	struct rpc_conn *conn;	
	conn = rpc_conn_by_connid( conndata.conn[i].connid );
	if( conn ) rpc_conn_close( conn );
      }
      
      /* delete from list */
      if( i != conndata.nconn - 1 ) conndata.conn[i] = conndata.conn[conndata.nconn - 1];
      conndata.nconn--;
      return 0;	
    }
  }
  
  return -1;
}

static void hrauth_conn_call_cb( struct rpc_waiter *w, struct rpc_inc *inc ) {
  struct hrauth_call_cxt *cxt;
  struct hrauth_conn *hc;

  cxt = (struct hrauth_call_cxt *)w->cxt;
  hc = hrauth_conn_by_hostid( cxt->hcall.hostid );
  if( !hc ) {
    hrauth_log( LOG_LVL_WARN, "hrauth_conn_call_cb unknown connection hostid=%"PRIx64"", cxt->hcall.hostid );
    return;
  }

  /* do anything required with the connection context */
  
  hrauth_call_cb( w, inc );
}

/* send an rpc to this host and possibly await a reply */
int hrauth_call_tcp_async( struct hrauth_call *hcall, struct xdr_s *args ) {
  int handle;
  struct hrauth_conn *hc;
  struct rpc_conn *conn;
  struct rpc_inc inc;
  struct hrauth_context *hcxt;
  struct rpc_waiter *w;
  struct hrauth_call_cxt *hcallp;
  
  /* start by getting connection for this host */
  hc = hrauth_conn_by_hostid( hcall->hostid );
  if( !hc ) return -1;

  /* check connected */
  if( !hc->connid ) return -1;
  if( hc->state != HRAUTH_CONN_CONNECTED ) return -1;
  
  /* get rpc connection */
  conn = rpc_conn_by_connid( hc->connid );
  if( !conn ) {
    hc->connid = 0;
    hc->state = HRAUTH_CONN_DISCONNECTED;
    return -1;
  }

  /* if connection is not sitting idle then can't do anything */
  /* TODO: queue this operation and do it later once the current operation has completed */
  if( conn->cstate != RPC_CSTATE_RECVLEN ) return -1;
  
  /* encode call into connection buffer */
  memset( &inc, 0, sizeof(inc) );
  xdr_init( &inc.xdr, conn->buf, conn->count );
  
  hcxt = &hc->hcxt;
  if( hcall->service != hcxt->service ) {
    /* can't change service level once context established */
    hrauth_init( hcxt, hcall->hostid );
    hcxt->service = hcall->service;
  }
  inc.pvr = hrauth_provider();
  inc.pcxt = hcxt;
  
  rpc_init_call( &inc, hcall->prog, hcall->vers, hcall->proc, &handle );
  if( args ) xdr_encode_fixed( &inc.xdr, args->buf, args->offset );
  rpc_complete_call( &inc, handle );

  rpc_send( conn, inc.xdr.offset );

  /* if no completion set then return immediately */
  if( !hcall->donecb ) return 0;
  
  /* await reply */
  w = (struct rpc_waiter *)malloc( sizeof(*w) + sizeof(*hcallp) );
  hcallp = (struct hrauth_call_cxt *)(((char *)w) + sizeof(*w));
  hcallp->hcall = *hcall;
  xdr_init( &hcallp->args, NULL, 0 );
  
  memset( w, 0, sizeof(*w) );  
  w->xid = inc.msg.xid;
  w->timeout = rpc_now() + hcall->timeout;
  w->cb = hrauth_conn_call_cb;
  w->cxt = hcallp;
  w->pvr = hrauth_provider();
  w->pcxt = hcxt;
  rpc_await_reply( w );

  return 0;
}


/* send a reply message back to this host on that host's connection */
int hrauth_reply_tcp( struct hrauth_context *hcxt, uint32_t xid, int acceptstat, struct xdr_s *res ) {
  int handle;
  struct hrauth_conn *hc;
  struct rpc_conn *conn;
  struct rpc_inc inc;
  
  /* start by getting connection for this host */
  hc = hrauth_conn_by_hostid( hcxt->remoteid );
  if( !hc ) return -1;

  /* check connected */
  if( !hc->connid ) return -1;
  if( hc->state != HRAUTH_CONN_CONNECTED ) return -1;
  
  /* get rpc connection */
  conn = rpc_conn_by_connid( hc->connid );
  if( !conn ) {
    hc->connid = 0;
    hc->state = HRAUTH_CONN_DISCONNECTED;
    return -1;
  }

  /* if connection is not sitting idle then can't do anything */
  /* TODO: queue this operation and do it later once the current operation has completed */
  if( conn->cstate != RPC_CSTATE_RECVLEN ) return -1;
  
  /* encode call into connection buffer */
  memset( &inc, 0, sizeof(inc) );
  xdr_init( &inc.xdr, conn->buf, conn->count );
  
  inc.pvr = hrauth_provider();
  inc.pcxt = hcxt;
  
  rpc_init_accept_reply( &inc, xid, acceptstat, NULL, &handle );
  if( res ) xdr_encode_fixed( &inc.xdr, res->buf, res->offset );
  rpc_complete_accept_reply( &inc, handle );

  rpc_send( conn, inc.xdr.offset );

  return 0;
}





static void hrauth_ping_cb( struct xdr_s *xdr, void *cxt ) {
  hrauth_log( LOG_LVL_TRACE, "hrauth ping %s", xdr ? "timeout" : "success" );
}

static void hrauth_send_ping( struct hrauth_conn *hc ) {
  struct hrauth_call hcall;
  int sts;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hc->hostid;
  hcall.prog = HRAUTH_RPC_PROG;
  hcall.vers = HRAUTH_RPC_VERS;
  hcall.proc = 3; /* nullping proc */
  hcall.donecb = hrauth_ping_cb;
  hcall.cxt = NULL;
  
  sts = hrauth_call_tcp_async( &hcall, NULL );
  if( sts ) {
    hrauth_log( LOG_LVL_ERROR, "hrauth ping failed hostid=%"PRIx64"", hc->hostid );
  }
}

static void hrauth_conn_iter_cb( struct rpc_iterator *it ) {
  /* check all connections are alive, attempt reconnect if any are disconnected */
  int sts, i;
  struct hrauth_conn *hc;
  struct rpc_conn *conn;
  uint64_t now;

  for( i = 0; i < conndata.nconn; i++ ) {
    hc = &conndata.conn[i];
    conn = rpc_conn_by_connid( hc->connid );
    
    hrauth_log( LOG_LVL_INFO, "hrauth conn hostid=%"PRIx64" state=%u connid=%"PRIu64" conn=%p", hc->hostid, hc->state, hc->connid, conn );
    
    if( hc->state == HRAUTH_CONN_DISCONNECTED ) {
      hrauth_log( LOG_LVL_INFO, "Reconnecting to hostid=%"PRIx64"", hc->hostid );
      sts = hrauth_connect( hc );
      if( sts ) {
	hrauth_log( LOG_LVL_ERROR, "Reconnect attempt failed hostid=%"PRIx64"", hc->hostid );
      }
    }
    
    /* check rpc connection's timestamp. if it is getting close to the connection timeout do something */
    if( hc->state == HRAUTH_CONN_CONNECTED ) {      
      conn = rpc_conn_by_connid( hc->connid );
      now = rpc_now();
      if( conn && ((now - conn->timestamp) >= hc->pingtimeout) ) {
	hrauth_log( LOG_LVL_INFO, "Connection getting stale, sending ping hostid=%"PRIx64"", hc->hostid );

	hrauth_send_ping( hc );	
      }
    }
  }
}

static struct rpc_iterator hrauth_conn_iter =
{
 NULL,
 0,
 5000,
 hrauth_conn_iter_cb,
 NULL
};

static void hrauth_conn_init( void ) {
  int sts;
  struct freg_entry entry;
  uint64_t hkey, hostid, id;
  
  /* register the connection iterator */
  rpc_iterator_register( &hrauth_conn_iter );

  /* register configured connections */
  sts = freg_subkey( NULL, 0, "/fju/hrauth/connect", FREG_CREATE, &hkey );
  if( !sts ) {
    id = 0;
    sts = freg_next( NULL, hkey, id, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
	sts = freg_get_by_name( NULL, entry.id, "hostid", FREG_TYPE_UINT64, (char *)&hostid, sizeof(hostid), NULL );
	if( !sts ) {
	  sts = hrauth_conn_register( hostid, NULL );
	  if( sts ) hrauth_log( LOG_LVL_ERROR, "Failed to register host %s (%"PRIx64")", entry.name, hostid );
	}
      }
      id = entry.id;
      sts = freg_next( NULL, hkey, id, &entry );
    }
    
  }

    
}

/* ---------------------- synchronous calls ------------------ */

static int hrauth_call( int tcp, struct hrauth_call *hcall, struct xdr_s *args, struct xdr_s *res, struct hrauth_call_opts *opts ) {
    int sts;
    struct rpc_call_pars pars;
    struct hrauth_context hcxt;
    struct sockaddr_in sin;
    char *tmpbuf = NULL;
    int port;
    struct hostreg_host host;
    struct rpc_listen *listen;
    
    port = opts && opts->mask & HRAUTH_CALL_OPT_PORT ? opts->port : 0;
    if( !port ) {
	listen = rpcd_listen_by_type( RPC_LISTEN_UDP );
	if( !listen ) return -1;
	port = ntohs( listen->addr.sin.sin_port );
    }

    memset( &sin, 0, sizeof(sin) );
    sin.sin_family = AF_INET;
    sin.sin_port = htons( port );

    if( (hcall->hostid == 0) || (hcall->hostid == hostreg_localid()) ) {
	sin.sin_addr.s_addr = htonl( INADDR_LOOPBACK );
    } else {
	sts = hostreg_host_by_id( hcall->hostid, &host );
	if( sts ) return sts;   
	sin.sin_addr.s_addr = host.addr[0];
    }      

    memset( &pars, 0, sizeof(pars) );
    pars.prog = hcall->prog;
    pars.vers = hcall->vers;
    pars.proc = hcall->proc;
    if( hcall->service != -1 ) {
	sts = hrauth_init( &hcxt, hcall->hostid );
	hcxt.service = hcall->service;
	pars.pvr = hrauth_provider();
	pars.pcxt = &hcxt;
    }
    memcpy( &pars.raddr, &sin, sizeof(sin) );
    pars.raddr_len = sizeof(sin);
    pars.timeout = hcall->timeout ? hcall->timeout : 1000;

    if( opts && opts->mask & HRAUTH_CALL_OPT_TMPBUF ) {
      pars.buf = opts->tmpbuf;
    } else {
      tmpbuf = malloc( 32*1024 );
      xdr_init( &pars.buf, (uint8_t *)tmpbuf, 32*1024 );
    }
    
    if( tcp ) sts = rpc_call_tcp( &pars, args, res );
    else sts = rpc_call_udp( &pars, args, res );

    if( tmpbuf ) {
      xdr_init( res, NULL, 0 ); /* if no tmpbuf passed in, we cannot return results */
      free( tmpbuf );
    }

    return sts;
}

int hrauth_call_udp( struct hrauth_call *hcall, struct xdr_s *args, struct xdr_s *res, struct hrauth_call_opts *opts ) {
  return hrauth_call( 0, hcall, args, res, opts );
}

int hrauth_call_tcp( struct hrauth_call *hcall, struct xdr_s *args, struct xdr_s *res, struct hrauth_call_opts *opts ) {
  return hrauth_call( 1, hcall, args, res, opts );
}
