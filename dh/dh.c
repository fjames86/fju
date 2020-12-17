
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
#include <openssl/opensslv.h>

/* openssl 1.1 is totally different to 1.0 */
#if ((OPENSSL_VERSION_NUMBER & 0x0ff00000) >= 0x00100000)
#define USING_OPENSSL_1_1
#endif

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
  DH *dh;
  char *public_str = NULL;
  char *secret_str = NULL;
  char *prime_str = NULL;
  char *g_str = NULL;
  int codes;
  BIGNUM *pub_key;
#ifdef USING_OPENSSL_1_1
  BIGNUM *bn, *bn2;
#endif
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
#ifdef USING_OPENSSL_1_1
      bn = NULL;
      sts = BN_hex2bn( &bn, prime_str );
#else
      sts = BN_hex2bn( &dh->p, prime_str );
#endif
      if( sts == 0 ) usage( "Failed to parse prime" );

#ifdef USING_OPENSSL_1_1
      bn2 = NULL;
      sts = BN_hex2bn( &bn2, g_str ? g_str : "02" );
#else
      sts = BN_hex2bn( &dh->g, g_str ? g_str : "02" );
#endif
      if( sts == 0 ) usage( "Failed to parse g" );

#ifdef USING_OPENSSL_1_1
      sts = DH_set0_pqg( dh, bn, NULL, bn2 );
      if( !sts ) usage( "DH_set0_pqg" );
#endif
      
    } else {
      sts = DH_generate_parameters_ex( dh, prime_len, generator, NULL );
      if( sts == 0 ) usage( "Failed to generate parameters" );
    }
    
    if( secret_str ) {
#ifdef USING_OPENSSL_1_1
      bn = NULL;
      sts = BN_hex2bn( &bn, secret_str );
#else
      sts = BN_hex2bn( &dh->priv_key, secret_str );
#endif
      if( sts == 0 ) usage( "Failed to parse secret key" );

#ifdef USING_OPENSSL_1_1
      sts = DH_set0_key( dh, NULL, bn );
      if( !sts ) usage( "DH_set0_key" );
#endif
      
    }
    
    sts = DH_generate_key( dh );
    if( sts == 0 ) usage( "Failed to generate key" );

    /* print out keys */
    printf( "\n" );
    if( !prime_str ) {
#ifdef USING_OPENSSL_1_1
      DH_get0_pqg( dh, (const BIGNUM **)&bn, NULL, NULL );      
      p = BN_bn2hex( bn );
#else
      p = BN_bn2hex( dh->p );
#endif
      printf( "prime %s\n\n", p );
      OPENSSL_free( p );

#ifdef USING_OPENSSL_1_1
      DH_get0_pqg( dh, NULL, NULL, (const BIGNUM **)&bn );
      p = BN_bn2hex( bn );
#else
      p = BN_bn2hex( dh->g );
#endif
      printf( "g %s\n\n", p );
      OPENSSL_free( p );
    }
    if( !secret_str ) {
#ifdef USING_OPENSSL_1_1
      DH_get0_key( dh, NULL, (const BIGNUM **)&bn );
      p = BN_bn2hex( bn );
#else
      p = BN_bn2hex( dh->priv_key );
#endif
      printf( "secret %s\n\n", p );
      OPENSSL_free( p );
    }
    if( !public_str ) {
#ifdef USING_OPENSSL_1_1
      DH_get0_key( dh, (const BIGNUM **)&bn, NULL );
      p = BN_bn2hex( bn );
#else
      p = BN_bn2hex( dh->pub_key );
#endif
      printf( "public %s\n\n", p );
      OPENSSL_free( p );
    }

    DH_free( dh );
    return 0;
  }
  
  /* compute shared key between the secret and public keys */
  if( !(public_str && secret_str && prime_str) ) usage( "Must provide public, secret, prime and g" );

  /* get public key */
  pub_key = NULL;
  sts = BN_hex2bn( &pub_key, public_str );
  if( sts == 0 ) usage( "Failed to parse public key" );

  /* set dh secret and parameters */
#ifdef USING_OPENSSL_1_1
  bn = NULL;
  sts = BN_hex2bn( &bn, secret_str );
#else
  sts = BN_hex2bn( &dh->priv_key, secret_str );
#endif  
  if( sts == 0 ) usage( "Failed to parse secret key" );
#ifdef USING_OPENSSL_1_1
  sts = DH_set0_key( dh, NULL, bn );
  if( !sts ) usage( "DH_set0_key" );
#endif
  
#ifdef USING_OPENSSL_1_1
  bn = NULL;
  sts = BN_hex2bn( &bn, prime_str );
#else
  sts = BN_hex2bn( &dh->p, prime_str );
#endif
  if( sts == 0 ) usage( "Failed to parse prime" );

#ifdef USING_OPENSSL_1_1
  bn2 = NULL;
  sts = BN_hex2bn( &bn2, g_str ? g_str : "02" );
#else
  sts = BN_hex2bn( &dh->g, g_str ? g_str : "02" );
#endif
  if( sts == 0 ) usage( "Failed to parse g" );
#ifdef USING_OPENSSL_1_1
  sts = DH_set0_pqg( dh, bn, NULL, bn2 );
  if( !sts ) usage( "DH_set0_pqg" );
#endif
  
  sts = DH_check( dh, &codes );
  if( sts == 0 ) usage( "DH_check failed" );
  if( codes ) usage( "DH_check found invalid parameters codes=%d (0x%x)", codes, codes );

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
