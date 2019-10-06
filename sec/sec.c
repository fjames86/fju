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
#include <ntstatus.h>
#define WIN32_NO_STATUS /* stop windows.h complaining about multiple definitions of ntstatus values */
#include <WinSock2.h>
#include <Windows.h>
#include <bcrypt.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>
#include <time.h>

#ifndef WIN32
#include <openssl/evp.h>
#include <openssl/ec.h>
#include <openssl/bn.h>
#include <openssl/sha.h>
#include <openssl/conf.h>
#include <openssl/err.h>
#include <openssl/opensslv.h>

#include <unistd.h>
#include <fcntl.h>
#endif

#include <fju/sec.h>

void sec_buf_init( struct sec_buf *sbuf, char *buf, int len ) {
	sbuf->buf = buf;
	sbuf->len = len;
}




#define SEC_ECDH_KEYLEN 32 

#ifdef WIN32
int ecdh_generate( struct sec_buf *local_priv, struct sec_buf *local_public ) {  
  BCRYPT_ALG_HANDLE handle;
  BCRYPT_KEY_HANDLE hkey;
  char pout[sizeof(BCRYPT_ECCKEY_BLOB) + SEC_ECDH_KEYLEN*3];
  int outlen;
  NTSTATUS sts;
  BCRYPT_ECCKEY_BLOB *eccp;
  char *p;
  
  sts = BCryptOpenAlgorithmProvider( &handle, BCRYPT_ECDH_P256_ALGORITHM, NULL, 0 );
  
  sts = BCryptGenerateKeyPair( handle, &hkey, 256, 0 );
  sts = BCryptFinalizeKeyPair( hkey, 0 );
  
  outlen = sizeof(*eccp) + SEC_ECDH_KEYLEN*3;
  sts = BCryptExportKey( hkey, NULL, BCRYPT_ECCPRIVATE_BLOB, pout, outlen, &outlen, 0 );
  p = pout + sizeof(*eccp) + SEC_ECDH_KEYLEN;
  eccp = (BCRYPT_ECCKEY_BLOB *)pout;	
  memcpy( local_priv->buf, pout + sizeof(*eccp) + 2*SEC_ECDH_KEYLEN, SEC_ECDH_KEYLEN );
  local_priv->len = SEC_ECDH_KEYLEN;
  
  outlen = sizeof(*eccp) + 2*SEC_ECDH_KEYLEN;
  sts = BCryptExportKey( hkey, NULL, BCRYPT_ECCPUBLIC_BLOB, pout, outlen, &outlen, 0 );
  eccp = (BCRYPT_ECCKEY_BLOB *)pout;	
  memcpy( local_public->buf, pout + sizeof(*eccp), SEC_ECDH_MAX_PUBKEY );
  local_public->len = SEC_ECDH_MAX_PUBKEY;
  
  BCryptDestroyKey( hkey );
  sts = BCryptCloseAlgorithmProvider( handle, 0 );
  
  return 0;
}


int ecdh_common( struct sec_buf *local_priv, struct sec_buf *remote_public, struct sec_buf *common ) {
  BCRYPT_ALG_HANDLE handle;
  BCRYPT_KEY_HANDLE hkey, hrkey;
  BCRYPT_SECRET_HANDLE skey;
  int outlen;
  NTSTATUS sts;
  BCRYPT_ECCKEY_BLOB *eccp;
  char pout[3*SEC_ECDH_KEYLEN + sizeof(BCRYPT_ECCKEY_BLOB)];
  int nbytes;

  if( local_priv->len != (SEC_ECDH_KEYLEN) ) return -1;
  if( remote_public->len != (SEC_ECDH_KEYLEN*2) ) return -1;

  sts = BCryptOpenAlgorithmProvider( &handle, BCRYPT_ECDH_P256_ALGORITHM, NULL, 0 );
  if( sts ) {
      printf( "BCryptOpenAlgorithmProvider failed %u (%x)\n", sts, sts );
      return -1;
  }

  outlen = sizeof(*eccp) + 3*SEC_ECDH_KEYLEN;
  eccp = (BCRYPT_ECCKEY_BLOB *)pout;
  eccp->dwMagic = BCRYPT_ECDH_PRIVATE_P256_MAGIC;
  eccp->cbKey = SEC_ECDH_KEYLEN;
  memcpy( pout + sizeof( *eccp ), remote_public->buf, 2 * SEC_ECDH_KEYLEN );
  memcpy( pout + sizeof(*eccp) + 2*SEC_ECDH_KEYLEN, local_priv->buf, SEC_ECDH_KEYLEN );
  sts = BCryptImportKeyPair( handle, NULL, BCRYPT_ECCPRIVATE_BLOB, &hkey, pout, outlen, 0 );
  if( sts ) {
      printf( "BCryptImportKeyPair priv failed %u (%x)\n", sts, sts );
      return -1;
  }
  
  outlen = sizeof(*eccp) + 2*SEC_ECDH_KEYLEN;
  eccp = (BCRYPT_ECCKEY_BLOB *)pout;
  eccp->dwMagic = BCRYPT_ECDH_PUBLIC_P256_MAGIC;
  eccp->cbKey = SEC_ECDH_KEYLEN;
  memcpy( pout + sizeof(*eccp), remote_public->buf, SEC_ECDH_MAX_PUBKEY );
  sts = BCryptImportKeyPair( handle, NULL, BCRYPT_ECCPUBLIC_BLOB, &hrkey, pout, outlen, 0 );
  if( sts ) {
      printf( "BCryptImportKeyPair pub failed %u (%x)\n", sts, sts );
      return -1;
  }
  
  /* derive common key */
  sts = BCryptSecretAgreement( hkey, hrkey, &skey, 0 );
  if( sts ) {
      printf( "BCryptSecretAgreement failed %u (%x)\n", sts, sts );
      return -1;
  }
  
  sts = BCryptDeriveKey( skey, BCRYPT_KDF_HASH, NULL, common->buf, SEC_ECDH_MAX_COMMON, &nbytes, 0 );
  if( sts ) {
      printf( "BCryptDeriveKey failed %u (%x)\n", sts, sts );
      return -1;
  }
  
  common->len = SEC_ECDH_MAX_COMMON;
  
  BCryptDestroyKey( hkey );
  BCryptDestroyKey( hrkey );
  BCryptDestroySecret( skey );
  BCryptCloseAlgorithmProvider( handle, 0 );
   
  return 0;
}

#else

int ecdh_generate( struct sec_buf *local_priv, struct sec_buf *local_public ) {
  EC_KEY *hkey;
  EC_POINT *ecp_public;
  BIGNUM *bn_secret;
  int nbytes;
  BN_CTX *bncxt;
  char tmpkey[2*SEC_ECDH_KEYLEN+1];

  hkey = EC_KEY_new_by_curve_name( NID_X9_62_prime256v1 );
  EC_KEY_generate_key( hkey );
  bn_secret = (BIGNUM *)EC_KEY_get0_private_key( hkey );
  
  nbytes = BN_num_bytes( bn_secret );
  BN_bn2bin( bn_secret, (uint8_t *)local_priv->buf );
  local_priv->len = SEC_ECDH_KEYLEN;

  ecp_public = (EC_POINT *)EC_KEY_get0_public_key( hkey );
  bncxt = BN_CTX_new();
  nbytes = EC_POINT_point2oct( EC_KEY_get0_group( hkey ), 
			       ecp_public,
			       POINT_CONVERSION_UNCOMPRESSED, 
			       (uint8_t *)tmpkey, 2*SEC_ECDH_KEYLEN + 1, 
			       bncxt );

  memcpy( local_public->buf, tmpkey + 1, 2*SEC_ECDH_KEYLEN );
  local_public->len = 2*SEC_ECDH_KEYLEN;
  BN_CTX_free( bncxt );

  EC_KEY_free( hkey );
  return 0;
}

int ecdh_common( struct sec_buf *local_priv, struct sec_buf *remote_public, struct sec_buf *common ) {
  EC_KEY *hkey, *hrkey;
  BIGNUM *bn_local_secret;
  EC_POINT *ecp_remote_public;
  int klen;
  EC_GROUP *group;
  BN_CTX *bncxt;
  char tmpkey[2*SEC_ECDH_KEYLEN+1];
  SHA_CTX sha1cxt;

  if( local_priv->len != (SEC_ECDH_KEYLEN) ) return -1;
  if( remote_public->len != (SEC_ECDH_KEYLEN*2) ) return -1;


  hkey = EC_KEY_new_by_curve_name( NID_X9_62_prime256v1 );
  bn_local_secret = BN_bin2bn( (uint8_t *)local_priv->buf, SEC_ECDH_KEYLEN, NULL );
  EC_KEY_set_private_key( hkey, bn_local_secret );

  hrkey = EC_KEY_new_by_curve_name( NID_X9_62_prime256v1 );
  bncxt = BN_CTX_new();
  group = (EC_GROUP *)EC_KEY_get0_group( hrkey );
  ecp_remote_public = EC_POINT_new( group );
  tmpkey[0] = POINT_CONVERSION_UNCOMPRESSED;
  memcpy( tmpkey + 1, remote_public->buf, 2*SEC_ECDH_KEYLEN );
  EC_POINT_oct2point( group, ecp_remote_public, (uint8_t *)tmpkey, 2*SEC_ECDH_KEYLEN + 1, bncxt );
  BN_CTX_free( bncxt );
  EC_KEY_set_public_key( hrkey, ecp_remote_public );

  klen = ECDH_compute_key( (uint8_t *)tmpkey, SEC_ECDH_KEYLEN, EC_KEY_get0_public_key( hrkey ), hkey, NULL );
  
  /* 
   * this seems odd, but we have to do it for windows compatibility. instead of computing the common key
   * we instead compute the sha1 hash of the common secret. this is because the windows API cannot/does not (wtf!?) 
   * give us this info, it can only give us a derived value e.g. a sha1 hash 
   */
  SHA1_Init( &sha1cxt );
  SHA1_Update( &sha1cxt, tmpkey, klen ); 
  SHA1_Final( (uint8_t *)common->buf, &sha1cxt );
  common->len = SEC_ECDH_MAX_COMMON;

  EC_KEY_free( hkey );
  EC_KEY_free( hrkey );

  return 0;
}
#endif


#ifdef WIN32
void sha1( uint8_t *hash, struct sec_buf *iov, int n ) {
	BCRYPT_ALG_HANDLE handle;
	BCRYPT_HASH_HANDLE hhash;
	int i;

	BCryptOpenAlgorithmProvider( &handle, BCRYPT_SHA1_ALGORITHM, NULL, 0 );
	BCryptCreateHash( handle, &hhash, NULL, 0, NULL, 0, 0 );

	for( i = 0; i < n; i++ ) {
		BCryptHashData( hhash, iov[i].buf, iov[i].len, 0 );
	}
	BCryptFinishHash( hhash, hash, SEC_SHA1_MAX_HASH, 0 );
	BCryptDestroyHash( hhash );
	BCryptCloseAlgorithmProvider( handle, 0 );
}
#else
void sha1( uint8_t *hash, struct sec_buf *iov, int n ) { 
  int i;
  SHA_CTX cxt;

  SHA1_Init( &cxt );
  for( i = 0; i < n; i++ ) {
    SHA1_Update( &cxt, iov[i].buf, iov[i].len );
  }
  SHA1_Final( hash, &cxt );
}
#endif

void sha1_hmac( uint8_t *hash, uint8_t *key, struct sec_buf *iov, int n ) {
	/* HMAC = Hash( (key XOR opad) || Hash( (key XOR ipad) || message ) ) */
	uint8_t opad[SEC_AES_MAX_KEY], ipad[SEC_AES_MAX_KEY];
	int i;
	uint8_t rhash[SEC_SHA1_MAX_HASH];
	struct sec_buf *piov;

	if( n < 1 ) return;

	memset( hash, 0, SEC_SHA1_MAX_HASH );

	piov = malloc( sizeof(*piov) * (n + 1) );

	/* XOR key with ipad and opad */
	for( i = 0; i < SEC_AES_MAX_KEY; i++ ) {
		opad[i] = 0x5c ^ key[i];
		ipad[i] = 0x36 ^ key[i];
	}

	/* Hash ipad||message */
	piov[0].buf = (char *)ipad;
	piov[0].len = SEC_AES_MAX_KEY;
	for( i = 0; i < n; i++ ) {
	  piov[i + 1].buf = iov[i].buf;
	  piov[i + 1].len = iov[i].len;
	}
	sha1( rhash, piov, n + 1 );

	/* Hash opad || Hash(ipad||message) */
	piov[0].buf = (char *)opad;
	piov[0].len = SEC_AES_MAX_KEY;
	piov[1].buf = (char *)rhash;
	piov[1].len = SEC_SHA1_MAX_HASH;
	sha1( hash, piov, 2 );

	free( piov );
}



#ifdef WIN32
void aes_encrypt( uint8_t *key, uint8_t *buf, int n ) {
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


void aes_decrypt( uint8_t *key, uint8_t *buf, int n ) {
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
#else
void aes_encrypt( uint8_t *key, uint8_t *buf, int n ) {
  uint8_t iv[32];
  EVP_CIPHER_CTX *ctx;
  int clen, len;
  uint8_t *ciphertext;

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

void aes_decrypt( uint8_t *key, uint8_t *buf, int n ) {
  EVP_CIPHER_CTX *ctx;
  int len, plen;
  uint8_t iv[32];
  uint8_t *plaintext;

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

#endif

#ifdef WIN32
void sec_rand( void *buf, int n ) {
	BCRYPT_ALG_HANDLE handle;

	BCryptOpenAlgorithmProvider( &handle, BCRYPT_RNG_ALGORITHM, NULL, 0 );
	BCryptGenRandom( handle, buf, n, 0 );
	BCryptCloseAlgorithmProvider( handle, 0 );
}
#else
void sec_rand( void *buf, int n ) {
  static int fd;
  if( !fd ) {
    fd = open( "/dev/urandom", O_RDONLY, 0600 );
  }
  read( fd, buf, n );
}
#endif

uint32_t sec_rand_uint32( void ) {
    uint32_t u32;
    sec_rand( &u32, sizeof(u32) );
    return u32;
}

char *sec_timestr( uint64_t now, char *str ) {
  struct tm *tm;
  time_t t;

  t = (time_t)now;
  tm = localtime( &t );
  strftime( str, 64, "%Y-%m-%d %H:%M:%S", tm );
  return str;
}

