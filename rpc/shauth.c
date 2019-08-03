/*
 * MIT License
 * 
 * Copyright (c) 2018 Frank James
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

/*
 * This file defines a custom authentication provider shauth ("shared secret authentication").
 * It is more an example than something which should be used seriously, although 
 * it should be sufficiently secure to be used reasonably confidently. 
 * The basic premise is both client and server have knowledge of a shared secret.
 * The client generates a random session key and encrypts a header containing 
 * this session key and sends this to the server in the initial handshake message.
 * The server decrypts this message using the secret key and verifies successful
 * decryption by comparing a timestamp against the current time. The timestamp 
 * must be within a specified clock skew (e.g. 5 minutes). The timestamp 
 * must also match a timestamp verifier (timestamp - 1). 
 * Once authenticated, the client is assigned a random nickname (uint32_t) 
 * which the client can use for subsequent communication without having to 
 * reauthenticate. 
 * This is similar to how "real" auth flavours work, e.g. RPC_AUTH_DES or RPC_AUTH_GSS.
 */

#include "shauth.h"


#ifdef WIN32

struct _sha_iovec {
	uint8_t *buf;
	int n;
};

static void _sha1( uint8_t *hash, struct _sha_iovec *iov, int n ) {
	BCRYPT_ALG_HANDLE handle;
	BCRYPT_HASH_HANDLE hhash;
	int i;

	BCryptOpenAlgorithmProvider( &handle, BCRYPT_SHA1_ALGORITHM, NULL, 0 );
	BCryptCreateHash( handle, &hhash, NULL, 0, NULL, 0, 0 );

	for( i = 0; i < n; i++ ) {
		BCryptHashData( hhash, iov[i].buf, iov[i].n, 0 );
	}
	BCryptFinishHash( hhash, hash, 20, 0 );
	BCryptDestroyHash( hhash );
	BCryptCloseAlgorithmProvider( handle, 0 );
}

static void sha1_hmac( uint8_t *hash, uint8_t *key, uint8_t *buf, int n ) {
	/* HMAC = Hash( (key XOR opad) || Hash( (key XOR ipad) || message ) ) */
	uint8_t opad[16], ipad[16];
	int i;
	uint8_t rhash[32];
	struct _sha_iovec iov[2];

	memset( hash, 0, 32 );

	/* XOR key with ipad and opad */
	for( i = 0; i < 16; i++ ) {
		opad[i] = 0x5c ^ key[i];
		ipad[i] = 0x36 ^ key[i];
	}

	/* Hash ipad||message */
	iov[0].buf = ipad;
	iov[0].n = 16;
	iov[1].buf = buf;
	iov[1].n = n;
	_sha1( rhash, iov, 2 );

	/* Hash opad || Hash(ipad||message) */
	iov[0].buf = opad;
	iov[0].n = 16;
	iov[1].buf = rhash;
	iov[1].n = 16;
	_sha1( hash, iov, 2 );
}

static void aes_encrypt( uint8_t *key, uint8_t *buf, int n ) {
	BCRYPT_ALG_HANDLE handle;
	BCRYPT_KEY_HANDLE hkey;
	uint8_t iv[16];
	ULONG result;
	uint8_t *outp;

	BCryptOpenAlgorithmProvider( &handle, BCRYPT_AES_ALGORITHM, NULL, 0 );
	BCryptGenerateSymmetricKey( handle, &hkey, NULL, 0, key, 16, 0 );

	memset( iv, 0, sizeof(iv) );
	outp = malloc( n );
	BCryptEncrypt( hkey, buf, n, NULL, iv, 16, outp, n, &result, 0 );
	memcpy( buf, outp, n );
	free( outp );

	BCryptDestroyKey( hkey );
	BCryptCloseAlgorithmProvider( handle, 0 );
}


static void aes_decrypt( uint8_t *key, uint8_t *buf, int n ) {
	BCRYPT_ALG_HANDLE handle;
	BCRYPT_KEY_HANDLE hkey;
	uint8_t iv[16];
	ULONG result;
	uint8_t *outp;

	BCryptOpenAlgorithmProvider( &handle, BCRYPT_AES_ALGORITHM, NULL, 0 );
	BCryptGenerateSymmetricKey( handle, &hkey, NULL, 0, key, 16, 0 );

	memset( iv, 0, sizeof(iv) );
	outp = malloc( n );
	BCryptDecrypt( hkey, buf, n, NULL, iv, 16, outp, n, &result, 0 );
	memcpy( buf, outp, n );
	free( outp );

	BCryptDestroyKey( hkey );
	BCryptCloseAlgorithmProvider( handle, 0 );
}

static void shauth_rand( void *buf, int n ) {
	BCRYPT_ALG_HANDLE handle;

	BCryptOpenAlgorithmProvider( &handle, BCRYPT_RNG_ALGORITHM, NULL, 0 );
	BCryptGenRandom( handle, buf, n, 0 );
	BCryptCloseAlgorithmProvider( handle, 0 );
}

#else

#include <openssl/evp.h>
#include <openssl/conf.h>
#include <openssl/evp.h>
#include <openssl/err.h>
#include <openssl/sha.h>
#include <openssl/opensslv.h>

#if ((OPENSSL_VERSION_NUMBER & 0x0ff00000) >= 0x00100000)
#define SHAUTH_OPENSSL_1_1
#endif


#include <fcntl.h>

/* provider methods follow */

static void init_crypto( void ) {
  static int initialized = 0;
  if( !initialized ) {
    ERR_load_crypto_strings();
    OpenSSL_add_all_algorithms();
#ifdef SHAUTH_OPENSSL_1_1
    OPENSSL_init_crypto( OPENSSL_INIT_ADD_ALL_CIPHERS|
			 OPENSSL_INIT_ADD_ALL_DIGESTS |
			 OPENSSL_INIT_LOAD_CRYPTO_STRINGS,
			 NULL );
#else
    OPENSSL_config( NULL );
#endif
    initialized = 1;
  }
}


static void sha1_hmac( uint8_t *hash, uint8_t *key, uint8_t *buf, int n ) {
  /* HMAC = Hash( (key XOR opad) || Hash( (key XOR ipad) || message ) ) */

  uint8_t opad[16], ipad[16];
  int i;
  SHA_CTX cxt;
  uint8_t rhash[32];

  init_crypto();

  memset( hash, 0, 32 );

  /* XOR key with ipad and opad */
  for( i = 0; i < 16; i++ ) {
    opad[i] = 0x5c ^ key[i];
    ipad[i] = 0x36 ^ key[i];
  }

  /* Hash ipad||message */
  SHA1_Init( &cxt );
  SHA1_Update( &cxt, ipad, 16 );
  SHA1_Update( &cxt, buf, n );
  SHA1_Final( rhash, &cxt );

  /* Hash opad || Hash(ipad||message) */
  SHA1_Init( &cxt );
  SHA1_Update( &cxt, opad, 16 );
  SHA1_Update( &cxt, rhash, 16 );
  SHA1_Final( hash, &cxt );
}

static void aes_encrypt( uint8_t *key, uint8_t *buf, int n ) {
  uint8_t iv[32];
  EVP_CIPHER_CTX *ctx;
  int clen, len;
  uint8_t *ciphertext;

  init_crypto();

  memset( iv, 0, sizeof(iv) );
  ctx = EVP_CIPHER_CTX_new();
  EVP_CIPHER_CTX_init( ctx );

  ciphertext = malloc( n + 16 );
  EVP_EncryptInit_ex( ctx, EVP_aes_128_cbc(), NULL, key, iv );
  EVP_CIPHER_CTX_set_padding( ctx, 0 );
  EVP_EncryptUpdate( ctx, ciphertext, &len, buf, n );
  clen = len;
  EVP_EncryptFinal_ex( ctx, ciphertext + len, &len );
  clen += len;
  memcpy( buf, ciphertext, n );
  free( ciphertext );
  EVP_CIPHER_CTX_cleanup( ctx );
  EVP_CIPHER_CTX_free( ctx );
}


static void aes_decrypt( uint8_t *key, uint8_t *buf, int n ) {
  EVP_CIPHER_CTX *ctx;
  int len, plen;
  uint8_t iv[32];
  uint8_t *plaintext;

  init_crypto();

  memset( iv, 0, sizeof(iv) );
  ctx = EVP_CIPHER_CTX_new();
  
  plaintext = malloc( n + 16 );

  EVP_CIPHER_CTX_init( ctx );
  EVP_DecryptInit_ex( ctx, EVP_aes_128_cbc(), NULL, key, iv );
  EVP_CIPHER_CTX_set_padding( ctx, 0 );
  EVP_DecryptUpdate( ctx, plaintext, &len, buf, n );
  plen = len;
  EVP_DecryptFinal_ex( ctx, plaintext + len, &len );
  plen += len;
  EVP_CIPHER_CTX_cleanup( ctx );
  memcpy( buf, plaintext, n );
  free( plaintext );
  EVP_CIPHER_CTX_free( ctx );
}

static void shauth_rand( void *buf, int n ) {
  static int fd;
  if( !fd ) {
    fd = open( "/dev/urandom", O_RDONLY, 0600 );
  }
  read( fd, buf, n );
}
#endif




static uint8_t shauth_shared_key[32];

void shauth_init( struct shauth_context *cxt, uint8_t *key ) {
  memset( cxt, 0, sizeof(*cxt) );
  memcpy( cxt->key, key, sizeof(cxt->key) );
  cxt->cipher = SHAUTH_CIPHER_AES128|SHAUTH_DIGEST_SHA1;
  cxt->window = 500;
  cxt->service = SHAUTH_SERVICE_NONE;
}

static int shauth_encrypt( uint8_t *buf, int size, uint8_t *key, int cipher ) {
  
  switch( cipher & SHAUTH_CIPHER_MASK ) {
  case SHAUTH_CIPHER_AES128:
    aes_encrypt( key, buf, size );
    break;
  default:
    return -1;
  }
  
  return 0;
}
static int shauth_decrypt( uint8_t *buf, int size, uint8_t *key, int cipher ) {
    
  switch( cipher & SHAUTH_CIPHER_MASK ) {
  case SHAUTH_CIPHER_AES128:
    aes_decrypt( key, buf, size );
    break;
  default:
    return -1;
  }
  
  return 0;

}

static int shauth_digest( uint8_t *hash, uint8_t *buf, int size, uint8_t *key, int digest ) {

  switch( digest & SHAUTH_DIGEST_MASK ) {
  case SHAUTH_DIGEST_SHA1:
    sha1_hmac( hash, key, buf, size );
    break;
  default:
    return -1;
  }

  return 0;
}

/* xdr encoders */
static int shauth_encode_auth( struct xdr_s *xdr, struct shauth_auth *x ) {
  xdr_encode_int32( xdr, x->tag );
  switch( x->tag ) {
  case SHAUTH_NICKNAME:
    xdr_encode_uint32( xdr, x->u.nickname );
    break;
  case SHAUTH_FULL:
    xdr_encode_uint32( xdr, x->u.full.cipher );
    xdr_encode_fixed( xdr, x->u.full.ecred, sizeof(x->u.full.ecred) );
    break;
  default:
    return -1;
  }
  return 0;
}
static int shauth_decode_auth( struct xdr_s *xdr, struct shauth_auth *x ) {
  int sts;
  xdr_decode_int32( xdr, &x->tag );
  switch( x->tag ) {
  case SHAUTH_NICKNAME:
    sts = xdr_decode_uint32( xdr, &x->u.nickname );
    if( sts ) return sts;
    break;
  case SHAUTH_FULL:
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
static int shauth_encode_verf( struct xdr_s *xdr, struct shauth_verf *x ) {
  xdr_encode_uint32( xdr, x->nonce );
  xdr_encode_uint32( xdr, x->timestamp );
  xdr_encode_uint32( xdr, x->tverf );
  xdr_encode_uint32( xdr, x->nickname );
  return 0;
}
static int shauth_decode_verf( struct xdr_s *xdr, struct shauth_verf *x ) {
  xdr_decode_uint32( xdr, &x->nonce );
  xdr_decode_uint32( xdr, &x->timestamp );
  xdr_decode_uint32( xdr, &x->tverf );
  xdr_decode_uint32( xdr, &x->nickname );
  return 0;
}
static int shauth_encode_cred( struct xdr_s *xdr, struct shauth_cred *x ) {
  xdr_encode_uint32( xdr, x->nonce );
  xdr_encode_fixed( xdr, x->session_key, 32 );
  xdr_encode_uint32( xdr, x->service );
  xdr_encode_uint32( xdr, x->window );
  xdr_encode_uint32( xdr, x->cipher );
  return 0;
}
static int shauth_decode_cred( struct xdr_s *xdr, struct shauth_cred *x ) {
  xdr_decode_uint32( xdr, &x->nonce );
  xdr_decode_fixed( xdr, x->session_key, 32 );
  xdr_decode_uint32( xdr, &x->service );
  xdr_decode_uint32( xdr, &x->window );
  xdr_decode_uint32( xdr, &x->cipher );
  return 0;
}






static int shauth_cauth( struct rpc_provider *pvr, struct rpc_msg *msg, void *pcxt ) {
  /* generate client authenticator, write to msg->u.call.auth */

  struct shauth_context *sa = (struct shauth_context *)pcxt;
  struct shauth_auth auth;
  struct xdr_s tmpx;
  struct shauth_verf verf;
  
  memset( &auth, 0, sizeof(auth) );  
  if( sa->nickname ) {
    auth.tag = SHAUTH_NICKNAME;
    auth.u.nickname = sa->nickname;
  } else {
    struct shauth_cred cred;
    
    auth.tag = SHAUTH_FULL;
    auth.u.full.cipher = sa->cipher;
    
    /* generate and encode credential */
    memset( &cred, 0, sizeof(cred) );
    shauth_rand( &cred.nonce, 4 );
    shauth_rand( sa->session_key, sizeof(sa->session_key) );
    memcpy( cred.session_key, sa->session_key, sizeof(cred.session_key) );
    cred.service = sa->service;
    cred.window = sa->window;
    cred.cipher = sa->cipher;
    xdr_init( &tmpx, auth.u.full.ecred, 64 );
    shauth_encode_cred( &tmpx, &cred );

    /* encrypt */
    shauth_encrypt( tmpx.buf, 64, sa->key, sa->cipher );
  }

  /* encode to authenticator */
  xdr_init( &tmpx, msg->u.call.auth.data, RPC_MAX_OPAQUE_AUTH );
  shauth_encode_auth( &tmpx, &auth );
  msg->u.call.auth.flavour = RPC_AUTH_SHAUTH;
  msg->u.call.auth.len = tmpx.offset;

  /* generate verifier */
  memset( &verf, 0, sizeof(verf) );
  shauth_rand( &verf.nonce, 4 );
  sa->timestamp = (uint32_t)time( NULL );
  verf.timestamp = sa->timestamp;
  verf.tverf = verf.timestamp - 1;
  verf.nickname = 0;
  xdr_init( &tmpx, msg->u.call.verf.data, sizeof(msg->u.call.verf.data) );
  shauth_encode_verf( &tmpx, &verf );
  shauth_encrypt( tmpx.buf, tmpx.offset, sa->session_key, sa->cipher );
  msg->u.call.verf.flavour = RPC_AUTH_SHAUTH;
  msg->u.call.verf.len = tmpx.offset;
  
  return 0;
}

static int shauth_cverf( struct rpc_provider *pvr, struct rpc_msg *msg, void *pcxt ) {
  /* recevive client verifier, parse msg->u.reply.u.accept.u.verf */
  struct shauth_context *sa = (struct shauth_context *)pcxt;   
  int sts;
  struct shauth_verf verf;
  struct xdr_s tmpx;
  
  xdr_init( &tmpx, msg->u.reply.u.accept.verf.data, msg->u.reply.u.accept.verf.len );
  /* decrypt */
  shauth_decrypt( tmpx.buf, tmpx.count, sa->session_key, sa->cipher );

  /* decode */
  sts = shauth_decode_verf( &tmpx, &verf );
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

static int shauth_taste( struct rpc_provider *pvr, int flavour ) {
  /* server taste flavour. return 0 if we like it, -1 otherwise */
  if( flavour == RPC_AUTH_SHAUTH ) return 1;
  return 0;
}



static struct shauth_context shcxttab[32];
static struct shauth_context *shauth_context_by_nickname( uint32_t nickname ) {
  int i;
  for( i = 0; i < 32; i++ ) {
    if( shcxttab[i].nickname == nickname ) return &shcxttab[i];
  }
  return NULL;
}
static struct shauth_context *shauth_context_alloc( void ) {
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


static int shauth_sauth( struct rpc_provider *pvr, struct rpc_msg *msg, void **pcxt ) {
  /* server auth. validate client authenticator and assign authentication context */
  struct shauth_context *sa;
  struct xdr_s tmpx;
  int sts;
  struct shauth_auth auth;
  struct shauth_verf verf;
  uint32_t now;
  struct shauth_cred cred;
  
  xdr_init( &tmpx, msg->u.call.auth.data, msg->u.call.auth.len );
  sts = shauth_decode_auth( &tmpx, &auth );
  if( sts ) return sts;
  switch( auth.tag ) {
  case SHAUTH_NICKNAME:
    /* lookup existing context */
    sa = shauth_context_by_nickname( auth.u.nickname );
    if( !sa ) return -1;
    rpc_log( RPC_LOG_DEBUG, "shauth: nickname=%d", auth.u.nickname );
    break;
  case SHAUTH_FULL:
    /* allocate new context */
    sa = shauth_context_alloc();
    sa->cipher = auth.u.full.cipher;
    xdr_init( &tmpx, auth.u.full.ecred, 64 );
    shauth_decrypt( tmpx.buf, tmpx.count, shauth_shared_key, sa->cipher );
    shauth_decode_cred( &tmpx, &cred );
    memcpy( sa->session_key, cred.session_key, 32 );
    sa->service = cred.service;
    sa->window = cred.window;
    sa->cipher = cred.cipher;
    shauth_rand( &sa->nickname, 4 );
    rpc_log( RPC_LOG_DEBUG, "shauth: full service=%d window=%d cipher=%08x nickname=%d",
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
  shauth_decrypt( tmpx.buf, tmpx.count, sa->session_key, sa->cipher );
  shauth_decode_verf( &tmpx, &verf );

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
  shauth_rand( &verf.nonce, 4 );
  verf.nickname = sa->nickname;
  xdr_init( &tmpx, (uint8_t *)&pvr->rverf.data, sizeof(pvr->rverf.data) );
  shauth_encode_verf( &tmpx, &verf );
  shauth_encrypt( tmpx.buf, tmpx.offset, sa->session_key, sa->cipher );
  pvr->rverf.flavour = RPC_AUTH_SHAUTH;
  pvr->rverf.len = tmpx.offset;

  *pcxt = sa;
  
  return 0;
}


/* ----------- arg/res modification functions ------------- */

static int shauth_mout( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  struct shauth_context *sa;
  uint8_t hash[32];
  int count;

  (void)(pvr);

  sa = (struct shauth_context *)pcxt;

  if( sa->service == SHAUTH_SERVICE_NONE ) return 0;

  count = end - start;
  
  if( sa->service == SHAUTH_SERVICE_PRIV ) {
    /* pad to multiple of 16 */
    if( count % 16 ) {
      memset( xdr->buf + start + count, 0, 16 - (count % 16) );
      end += 16 - (count % 16);
      xdr->offset = end;

      count = end - start;
    }

    /* encrypt */
    shauth_encrypt( xdr->buf + start, count, sa->session_key, sa->cipher );
  }

  /* now prepend the hash */
  shauth_digest( hash, xdr->buf + start, count, sa->session_key, sa->cipher );
  memmove( xdr->buf + start + sizeof(hash), xdr->buf + start, count );
  memcpy( xdr->buf + start, hash, sizeof(hash) );
  xdr->offset += sizeof(hash);

  return 0;
}
static int shauth_min( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  struct shauth_context *sa = (struct shauth_context *)pcxt;
  uint8_t hash[32];
  int count;

  (void)(pvr);

  if( sa->service == SHAUTH_SERVICE_NONE ) return 0;

  if( start != xdr->offset ) return -1;

  count = end - xdr->offset;
  if( count < (int)sizeof(hash) ) return -1;

  /* Remove the hash */
  xdr->offset += sizeof(hash);
  count -= sizeof(hash);

  /* Check hash */
  shauth_digest( hash, xdr->buf + xdr->offset, count, sa->session_key, sa->cipher );
  if( memcmp( xdr->buf + start, hash, sizeof(hash) ) != 0 ) return -1;

  if( sa->service == SHAUTH_SERVICE_PRIV ) {
    /* decrypt payload */
    if( count % 16 ) return -1;
    shauth_decrypt( xdr->buf + xdr->offset, count, sa->session_key, sa->cipher );
  }

  return 0;
}

int shauth_priv_header( struct shauth_context *sa, struct xdr_s *xdr, int start, int end ) {
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
  shauth_encrypt( xdr->buf + data_start, count, sa->session_key, sa->cipher );

  /* now prepend the hash */
  shauth_digest( xdr->buf + start, xdr->buf + data_start, count, sa->session_key, sa->cipher );

  return 0;
}

static int shauth_cmargs( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  return shauth_mout( pvr, xdr, start, end, pcxt );
}
static int shauth_cmres( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  return shauth_min( pvr, xdr, start, end, pcxt );
}

static int shauth_smargs( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  return shauth_min( pvr, xdr, start, end, pcxt );
}
static int shauth_smres( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt ) {
  return shauth_mout( pvr, xdr, start, end, pcxt );
}


static struct rpc_provider shauthpvr = {
  NULL,
  RPC_AUTH_SHAUTH,

  shauth_cauth,
  shauth_cverf,
  shauth_cmargs,
  shauth_cmres,
  
  shauth_taste,
  shauth_sauth,
  shauth_smargs,
  shauth_smres
};

struct rpc_provider *shauth_provider( void ) {
  return &shauthpvr;
}

void shauth_set_shared( uint8_t *key ) {
  memcpy( shauth_shared_key, key, 32 );
}

void shauth_register( uint8_t *key ) {
  if( key ) shauth_set_shared( key );
  rpc_provider_register( shauth_provider() );
}
