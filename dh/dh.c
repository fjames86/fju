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
 * This file defines a simple Diffie-Hellman CLI tool. It can generate 
 * the DH parameters (prime/generator) or derive a common key from a 
 * public/private key pair. 
 */


#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>

#include <openssl/dh.h>
#include <openssl/bn.h>


static void usage( char *fmt, ... ) {
  va_list args;
  printf( "Usage: dh [-p public] [-s secret]\n"
	  "          [-m prime] [-g 2|5] [-L prime_len]\n"
	  " Provide public, secret and prime to generate common key.\n"
	  " Otherwise will generate a prime and public/private keys.\n"
	  "\n" );
  if( fmt ) {
    printf( "Error: " );
    va_start( args, fmt );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
  exit( 0 );
}

int main( int argc, char **argv ) {
  char *public_str = NULL;
  char *secret_str = NULL;
  char *prime_str = NULL;
  char *g_str = NULL;
  DH *dh;
  int codes;
  BIGNUM *pub_key;
  uint8_t *common;
  int prime_len = 2048, generator = 2;
  int i, sts;
  
  /* parse command line */
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      public_str = argv[i];
    } else if( strcmp( argv[i], "-s" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      secret_str = argv[i];
    } else if( strcmp( argv[i], "-m" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      prime_str = argv[i];
    } else if( strcmp( argv[i], "-g" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      generator = strtoul( argv[i], NULL, 10 );
      if( (generator != 2) && (generator != 5) ) usage( "Generator must be 2 or 5" );
      g_str = argv[i];
    } else if( strcmp( argv[i], "-L" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      prime_len = strtoul( argv[i], NULL, 10 );
    } else {
      usage( NULL );
    }
    i++;
  }

  dh = DH_new();

  if( !(public_str && secret_str && prime_str) ) {
    char *p;
    
    if( prime_str ) {
      sts = BN_hex2bn( &dh->p, prime_str );
      if( sts == 0 ) usage( "Failed to parse prime" );
      sts = BN_hex2bn( &dh->g, g_str ? g_str : "02" );
      if( sts == 0 ) usage( "Failed to parse g" );
    } else {
      sts = DH_generate_parameters_ex( dh, prime_len, generator, NULL );
      if( sts == 0 ) usage( "Failed to generate parameters" );
    }
    
    if( secret_str ) {
      sts = BN_hex2bn( &dh->priv_key, secret_str );
      if( sts == 0 ) usage( "Failed to parse secret key" );
    }
    
    sts = DH_generate_key( dh );
    if( sts == 0 ) usage( "Faled to generate key" );

    /* print out keys */
    printf( "\n" );
    if( !prime_str ) {
      p = BN_bn2hex( dh->p );
      printf( "prime %s\n\n", p );
      OPENSSL_free( p );
      p = BN_bn2hex( dh->g );
      printf( "g %s\n\n", p );
      OPENSSL_free( p );
    }
    if( !secret_str ) {
      p = BN_bn2hex( dh->priv_key );
      printf( "secret %s\n\n", p );
      OPENSSL_free( p );
    }
    if( !public_str ) {
      p = BN_bn2hex( dh->pub_key );
      printf( "public %s\n\n", p );
      OPENSSL_free( p );
    }
    
    return 0;
  }
  
  /* compute shared key between the secret and public keys */
  if( !(public_str && secret_str && prime_str) ) usage( "Must provide public, secret, prime and g" );

  /* get public key */
  sts = BN_hex2bn( &pub_key, public_str );
  if( sts == 0 ) usage( "Failed to parse public key" );

  /* set dh secret and parameters */
  sts = BN_hex2bn( &dh->priv_key, secret_str );
  if( sts == 0 ) usage( "Failed to parse secret key" );
  sts = BN_hex2bn( &dh->p, prime_str );
  if( sts == 0 ) usage( "Failed to parse prime" );
  sts = BN_hex2bn( &dh->g, g_str ? g_str : "02" );
  if( sts == 0 ) usage( "Failed to parse g" );

  sts = DH_check( dh, &codes );
  if( sts == 0 ) usage( "DH_check failed" );
  if( codes ) usage( "DH_check found invalid parameters" );

  common = malloc( DH_size( dh ) );
  sts = DH_compute_key( common, pub_key, dh );
  if( sts == -1 ) usage( "Failed to compute common key" );
  for( i = 0; i < sts; i++ ) {
    printf( "%02x", (uint32_t)common[i] );
  }
  printf( "\n" );
  
  free( common );
  BN_free( pub_key );
  DH_free( dh );
  
  return 0;
}
