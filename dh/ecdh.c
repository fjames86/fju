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
 * This file defines a small program showing how to use the ECDH functions 
 * to generate keys and derive the common secret 
 */

#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <ntstatus.h>
#include <WinSock2.h>
#include <Windows.h>
#include <bcrypt.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#ifndef WIN32
#include <openssl/evp.h>
#include <openssl/ec.h>
#include <openssl/bn.h>
#endif

static void usage( char *fmt, ... ) {
  if( fmt ) {
    va_list args;
    printf( "Error: " );
    va_start( args, fmt );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
    exit( 1 );
  }

  printf( "ecdh [-p public -s secret]\n" );
  exit( 0 );
}

#define KEYLEN 32 
struct ecdh_keybuf {
	uint8_t buf[4*KEYLEN];
	int len;
};

#ifdef WIN32
static int ecdh_generate( struct ecdh_keybuf *local_secret, struct ecdh_keybuf *local_public ) {  
	BCRYPT_ALG_HANDLE handle;
	BCRYPT_KEY_HANDLE hkey;
	char pout[sizeof(BCRYPT_ECCKEY_BLOB) + KEYLEN*3];
	int outlen;
	NTSTATUS sts;
	BCRYPT_ECCKEY_BLOB *eccp;
	char *p;

	sts = BCryptOpenAlgorithmProvider( &handle, BCRYPT_ECDH_P256_ALGORITHM, NULL, 0 );

	sts = BCryptGenerateKeyPair( handle, &hkey, 256, 0 );
	sts = BCryptFinalizeKeyPair( hkey, 0 );

	outlen = sizeof(*eccp) + KEYLEN*3;
	sts = BCryptExportKey( hkey, NULL, BCRYPT_ECCPRIVATE_BLOB, pout, outlen, &outlen, 0 );
	p = pout + sizeof(*eccp) + KEYLEN;
	eccp = (BCRYPT_ECCKEY_BLOB *)pout;	
	memcpy( local_secret->buf, pout + sizeof(*eccp), 3*KEYLEN );
	local_secret->len = 3*KEYLEN;

	outlen = sizeof(*eccp) + 2*KEYLEN;
	sts = BCryptExportKey( hkey, NULL, BCRYPT_ECCPUBLIC_BLOB, pout, outlen, &outlen, 0 );
	eccp = (BCRYPT_ECCKEY_BLOB *)pout;	
	memcpy( local_public->buf, pout + sizeof(*eccp), 2*KEYLEN );
	local_public->len = 2*KEYLEN;

	BCryptDestroyKey( hkey );
	sts = BCryptCloseAlgorithmProvider( handle, 0 );

	return 0;
}
static int ecdh_common( struct ecdh_keybuf *local_secret, struct ecdh_keybuf *remote_public, struct ecdh_keybuf *common ) {
	BCRYPT_ALG_HANDLE handle;
	BCRYPT_KEY_HANDLE hkey, hrkey;
	BCRYPT_SECRET_HANDLE skey;
	int outlen;
	NTSTATUS sts;
	BCRYPT_ECCKEY_BLOB *eccp;
	char pout[3*KEYLEN + sizeof(BCRYPT_ECCKEY_BLOB)];
	int nbytes;

	sts = BCryptOpenAlgorithmProvider( &handle, BCRYPT_ECDH_P256_ALGORITHM, NULL, 0 );

	outlen = sizeof(*eccp) + 3*KEYLEN;
	eccp = (BCRYPT_ECCKEY_BLOB *)pout;
	eccp->dwMagic = BCRYPT_ECDH_PRIVATE_P256_MAGIC;
	eccp->cbKey = KEYLEN;
	memcpy( pout + sizeof(*eccp), local_secret->buf, 3*KEYLEN );
	sts = BCryptImportKeyPair( handle, NULL, BCRYPT_ECCPRIVATE_BLOB, &hkey, pout, outlen, 0 );

	outlen = sizeof(*eccp) + 2*KEYLEN;
	eccp = (BCRYPT_ECCKEY_BLOB *)pout;
	eccp->dwMagic = BCRYPT_ECDH_PUBLIC_P256_MAGIC;
	eccp->cbKey = KEYLEN;
	memcpy( pout + sizeof(*eccp), remote_public->buf, 2*KEYLEN );
	sts = BCryptImportKeyPair( handle, NULL, BCRYPT_ECCPUBLIC_BLOB, &hrkey, pout, outlen, 0 );

	/* derive common key */
	sts = BCryptSecretAgreement( hkey, hrkey, &skey, 0 );

	sts = BCryptDeriveKey( skey, BCRYPT_KDF_HASH, NULL, common->buf, sizeof(common->buf), &nbytes, 0 );
	common->len = nbytes;

	BCryptDestroyKey( hkey );
	BCryptDestroyKey( hrkey );
	BCryptDestroySecret( skey );
	BCryptCloseAlgorithmProvider( handle, 0 );


	return -1;
}
#else
static int ecdh_generate( struct ecdh_keybuf *local_secret, ecdh_keybuf *local_public ) {
  EC_KEY *hkey;
  EC_POINT *ecp_public;
  BIGNUM *bn_secret;
  int nbytes;
  BN_CTX *bncxt;
  char tmpkey[2*KEYLEN+1];

  hkey = EC_KEY_new_by_curve_name( NID_X9_62_prime256v1 );
  EC_KEY_generate_key( hkey );
  bn_secret = (BIGNUM *)EC_KEY_get0_private_key( hkey );
  
  nbytes = BN_num_bytes( bn_secret );
  BN_bn2bin( bn_secret, local_secret->buf );
  local_secret->len = KEYLEN;

  ecp_public = (EC_POINT *)EC_KEY_get0_public_key( hkey );
  bncxt = BN_CTX_new();
  nbytes = EC_POINT_point2oct( EC_KEY_get0_group( hkey ), 
			       ecp_public,
			       POINT_CONVERSION_UNCOMPRESSED, 
			       tmpkey, 2*KEYLEN + 1, 
			       bncxt );
  memcpy( local_public->buf, tmpkey + 1, 2*KEYLEN );
  local_public->len = 2*KEYLEN;
  BN_CTX_free( bncxt );

  EC_KEY_free( hkey );
  return 0;
}

static int ecdh_common( struct ecdh_keybuf *local_secret, struct ecdk_keybuf *remote_public, struct ecdh_keybuf *common ) {
  EC_KEY *hkey, *hrkey;
  BIGNUM *bn_local_secret;
  EC_POINT *ecp_remote_public;
  int klen;
  EC_GROUP *group;
  BN_CTX *bncxt;
  char tmpkey[2*KEYLEN+1];

  hkey = EC_KEY_new_by_curve_name( NID_X9_62_prime256v1 );
  bn_local_secret = BN_bin2bn( local_secret->buf, KEYLEN, NULL );
  EC_KEY_set_private_key( hkey, bn_local_secret );

  hrkey = EC_KEY_new_by_curve_name( NID_X9_62_prime256v1 );
  bncxt = BN_CTX_new();
  group = (EC_GROUP *)EC_KEY_get0_group( hrkey );
  ecp_remote_public = EC_POINT_new( group );
  tmpkey[0] = POINT_CONVERSION_UNCOMPRESSED;
  memcpy( tmpkey + 1, remote_public->buf, 2*KEYLEN );
  EC_POINT_oct2point( group, ecp_remote_public, tmpkey, 2*KEYLEN + 1, bncxt );
  BN_CTX_free( bncxt );
  EC_KEY_set_public_key( hrkey, ecp_remote_public );

  klen = ECDH_compute_key( common->buf, KEYLEN, EC_KEY_get0_public_key( hrkey ), hkey, NULL );
  common->len = KEYLEN;

  EC_KEY_free( hkey );
  EC_KEY_free( hrkey );

  return 0;
}
#endif

static void hex2bn( char *hex, struct ecdh_keybuf *buf ) {
  int i, j;
  unsigned char x;
  for( i = 0; i < sizeof(buf->buf); i++ ) {
    x = 0;
    j = 2 * i;
    if( hex[j] != '\0' ) {
      if( hex[j] >= '0' && hex[j] <= '9' ) x = hex[j] - '0';
      else if( hex[j] >= 'a' && hex[j] <= 'f' ) x = 10 + (hex[j] - 'a');
      else if( hex[j] >= 'A' && hex[j] <= 'F' ) x = 10 + (hex[j] - 'A');
      else usage( "Unable to parse \"%s\"", hex );
    }
    x = x << 4;
    if( hex[2*i + 1] != '\0' ) {
      j = (2*i) + 1;
      if( hex[j] >= '0' && hex[j] <= '9' ) x |= hex[j] - '0';
      else if( hex[j] >= 'a' && hex[j] <= 'f' ) x |= 10 + (hex[j] - 'a');
      else if( hex[j] >= 'A' && hex[j] <= 'F' ) x |= 10 + (hex[j] - 'A');
      else usage( "Unable to parse \"%s\"", hex );
    }
    buf->buf[i] = x;
    if( (hex[2*i] == '\0') || (hex[(2*i)+1] == '\0') ) break;
  }
  buf->len = i;
}

static void bn2hex( char *bn, char *hex, int len ) {
  int i;
  unsigned int x;
  strcpy( hex, "" );
  for( i = 0; i < len; i++ ) {
    x = (unsigned int)((unsigned char)bn[i]);
    sprintf( hex + 2*i, "%02x", x );
  }
  hex[len*2] = '\0';
}

int main( int argc, char **argv ) {
  int i;
  struct ecdh_keybuf secret, public, common;
  int sp, pp;
  char hex[256];

  memset( &secret, 0, sizeof(secret) );
  memset( &public, 0, sizeof(public) );
  memset( &common, 0, sizeof(common) );
  sp = 0;
  pp = 0;

  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-s" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      hex2bn( argv[i], &secret );
      sp = 1;
    } else if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      hex2bn( argv[i], &public );
      pp = 1;
    } else usage( NULL );
    i++;
  }

#if 0
  {
	  struct ecdh_keybuf secret1, secret2;
	  struct ecdh_keybuf public1, public2;
	  struct ecdh_keybuf common1, common2;
	  memset( &secret1, 0, sizeof(secret1) );
	  memset( &secret2, 0, sizeof(secret2) );
	  memset( &public1, 0, sizeof(public1) );
	  memset( &public2, 0, sizeof(public2) );
	  memset( &common1, 0, sizeof(common1) );
	  memset( &common2, 0, sizeof(common2) );
		ecdh_generate( &secret1, &public1 );
		ecdh_generate( &secret2, &public2 );
		ecdh_common( &secret1, &public2, &common1 );
		ecdh_common( &secret2, &public1, &common2 );

		bn2hex( common1.buf, hex, common1.len );
		printf( "%s\n", hex );
		bn2hex( common2.buf, hex, common2.len );
		printf( "%s\n", hex );

  }
#endif

  if( pp && sp ) {
    ecdh_common( &secret, &public, &common );
    bn2hex( &common, hex, common.len );
    printf( "COMMON %s\n", hex );
  } else {
    ecdh_generate( &secret, &public );
    bn2hex( secret.buf, hex, secret.len );
    printf( "SECRET %s\n", hex );
    bn2hex( public.buf, hex, public.len );
    printf( "PUBLIC %s\n", hex );
  }

  return 0;
}

