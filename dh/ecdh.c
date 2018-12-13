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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include <openssl/evp.h>
#include <openssl/ec.h>
#include <openssl/bn.h>


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

static int ecdh_generate( uint8_t *local_secret, uint8_t *local_public ) {
  EC_KEY *hkey;
  EC_POINT *ecp_public;
  BIGNUM *bn_secret;
  int nbytes;
  BN_CTX *bncxt;
  char tmpkey[KEYLEN+1];

  hkey = EC_KEY_new_by_curve_name( NID_X9_62_prime256v1 );
  EC_KEY_generate_key( hkey );
  bn_secret = (BIGNUM *)EC_KEY_get0_private_key( hkey );
  
  nbytes = BN_num_bytes( bn_secret );
  BN_bn2bin( bn_secret, local_secret );

  ecp_public = (EC_POINT *)EC_KEY_get0_public_key( hkey );
  bncxt = BN_CTX_new();
  EC_POINT_point2oct( EC_KEY_get0_group( hkey ), 
		      ecp_public,
		      POINT_CONVERSION_COMPRESSED, 
		      tmpkey, KEYLEN + 1, 
		      bncxt );
  memcpy( local_public, tmpkey + 1, KEYLEN );
  BN_CTX_free( bncxt );

  EC_KEY_free( hkey );
  return 0;
}

static int ecdh_common( uint8_t *local_secret, uint8_t *remote_public, uint8_t *common ) {
  EC_KEY *hkey, *hrkey;
  BIGNUM *bn_local_secret;
  EC_POINT *ecp_remote_public;
  int klen;
  EC_GROUP *group;
  BN_CTX *bncxt;
  char tmpkey[KEYLEN+1];

  hkey = EC_KEY_new_by_curve_name( NID_X9_62_prime256v1 );
  bn_local_secret = BN_bin2bn( local_secret, KEYLEN, NULL );
  EC_KEY_set_private_key( hkey, bn_local_secret );

  hrkey = EC_KEY_new_by_curve_name( NID_X9_62_prime256v1 );
  bncxt = BN_CTX_new();
  group = (EC_GROUP *)EC_KEY_get0_group( hrkey );
  ecp_remote_public = EC_POINT_new( group );
  tmpkey[0] = POINT_CONVERSION_COMPRESSED;
  memcpy( tmpkey + 1, remote_public, KEYLEN );
  EC_POINT_oct2point( group, ecp_remote_public, tmpkey, KEYLEN + 1, bncxt );
  BN_CTX_free( bncxt );
  EC_KEY_set_public_key( hrkey, ecp_remote_public );

  klen = ECDH_compute_key( common, KEYLEN, EC_KEY_get0_public_key( hrkey ), hkey, NULL );

  EC_KEY_free( hkey );
  EC_KEY_free( hrkey );

  return 0;
}

static void hex2bn( char *hex, unsigned char *bn ) {
  int i;
  unsigned char x;
  for( i = 0; i < KEYLEN; i++ ) {
    x = 0;
    if( hex[i] != '\0' ) {
      if( hex[i] >= '0' && hex[i] <= '9' ) x = hex[i] - '0';
      else if( hex[i] >= 'a' && hex[i] <= 'f' ) x = hex[i] - 'a';
      else if( hex[i] >= 'A' && hex[i] <= 'F' ) x = hex[i] - 'A';
      else usage( "Unable to parse \"%s\"", hex );
    }
    if( hex[i + 1] != '\0' ) {
      x >>= 4;
      if( hex[i+1] >= '0' && hex[i+1] <= '9' ) x |= hex[i+1] - '0';
      else if( hex[i+1] >= 'a' && hex[i+1] <= 'f' ) x |= hex[i+1] - 'a';
      else if( hex[i+1] >= 'A' && hex[i+1] <= 'F' ) x |= hex[i+1] - 'A';
      else usage( "Unable to parse \"%s\"", hex );
    }
    bn[i] = x;
    if( (hex[i] == '\0') || (hex[i+1] == '\0') ) break;
  }
}

static void bn2hex( char *bn, char *hex ) {
  int i;
  strcpy( hex, "" );
  for( i = 0; i < KEYLEN; i++ ) {
    sprintf( hex + strlen( hex ), "%02x", (unsigned int)((unsigned char)bn[i]) );
  }
}

int main( int argc, char **argv ) {
  int i;
  char secret[KEYLEN];
  char public[KEYLEN];
  char common[KEYLEN];
  int sp, pp;
  char hex[256];

  memset( secret, 0, sizeof(secret) );
  memset( public, 0, sizeof(public) );
  memset( common, 0, sizeof(common) );
  sp = 0;
  pp = 0;

  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-s" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      hex2bn( argv[i], secret );
      sp = 1;
    } else if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      hex2bn( argv[i], public );
      pp = 1;
    } else usage( NULL );
    i++;
  }

  if( pp && sp ) {
    ecdh_common( secret, public, common );
    bn2hex( common, hex );
    printf( "COMMON %s\n", hex );
  } else {
    ecdh_generate( secret, public );
    bn2hex( secret, hex );
    printf( "SECRET %s\n", hex );
    bn2hex( public, hex );
    printf( "PUBLIC %s\n", hex );
  }

  return 0;
}

