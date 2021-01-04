

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>

#include <fju/sec.h>


static void usage( char *fmt, ... ) {
  printf( "Usage: shamir [-n n] [-k k] split secret\n"
	  "              join share ...\n"
	  );
  if( fmt ) {
    va_list args;
    printf( "Error: " );
    va_start( args, fmt );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
  
  exit( 1 );
}


int shamir_main( int argc, char **argv ) {
  int argi;
  int n = 3, k = 2;

  argi = 1;
  while( argi < argc ) {
    if( strcmp( argv[argi], "-n" ) == 0 ) {
      argi++;
      if( argi >= argc ) usage( NULL );
      n = strtoul( argv[argi], NULL, 10 );
    } else if( strcmp( argv[argi], "-k" ) == 0 ) {
      argi++;
      if( argi >= argc ) usage( NULL );
      k = strtoul( argv[argi], NULL, 10 );
    } else break;
    argi++;
  }
      
  
  if( argi >= argc ) usage( NULL );

  if( strcmp( argv[argi], "split") == 0 ) {
    int secretlen, i, row;
    char *buf;
    struct sec_shamir_share *shares;
    char *hex;
    
    argi++;
    if( argi >= argc ) usage( NULL );
    buf = argv[argi];
    
    secretlen = strlen( buf );
    shares = malloc( sizeof(*shares) * n );
    for( i = 0; i < n; i++ ) {
      shares[i].flags = 0;
      shares[i].xval = 0;
      shares[i].sharebuf = malloc( secretlen + 1 );
    }
    sec_shamir_split( (uint8_t *)buf, secretlen, shares, n, k );

    hex = malloc( (4*secretlen) / 3 + 5 );
    for( row = 0; row < n; row++ ) {
      base64_encode( (char *)shares[row].sharebuf, secretlen + 1, hex );
      printf( "%s\n", hex );
      free( shares[row].sharebuf );
    }
    free( shares );
    free( hex );
  } else if( strcmp( argv[argi], "join" ) == 0 ) {
    int k, secretlen, i, sl;
    struct sec_shamir_share *shares;
    uint8_t *secret;

    argi++;
    if( argi >= argc ) usage( NULL );
    k = argc - argi;
    secretlen = -1; /* get secretlen from length of first share */
    shares = malloc( k * sizeof(*shares) );

    for( i = 0; i < k; i++ ) {
      sl = strlen( argv[argi] );
      sl = (4*sl) / 3 + 5;
      shares[i].sharebuf = malloc( sl );
      memset( shares[i].sharebuf, 0, sl );
      sl = base64_decode( (char *)shares[i].sharebuf, sl, argv[argi] );
      if( sl < 0 ) usage( "Bad share %d", i );
      if( secretlen == -1 ) secretlen = sl;
      else if( sl != secretlen ) usage( "Share %d length mismatch %d != %d", i, sl, secretlen );

      argi++;
    }

    secret = malloc( secretlen );
    sec_shamir_join( secret, secretlen, shares, k );

    printf( "%s\n", secret );
  } else usage( NULL );

  return 0;
}


