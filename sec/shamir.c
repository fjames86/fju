

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>

#include <fju/sec.h>


static void usage( char *fmt, ... ) {
  printf( "Usage: shamir OPTIONS [split <secret>] | [join <share> ...]\n"
	  "    Where OPTIONS:\n"
	  "     -n     number of shares\n"
	  "     -k     share threshold\n"
	  "     -b     binary mode, expect secret as base64 and emit share as base64\n"
	  "\n"
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
  int binarymode = 0;
  
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
    } else if( strcmp( argv[argi], "-b" ) == 0 ) {
      binarymode = 1;
    } else break;
    argi++;
  }
      
  
  if( argi >= argc ) usage( NULL );

  if( strcmp( argv[argi], "split") == 0 ) {
    int secretlen, i, row, buflen, sl;
    char *buf;
    struct sec_shamir_share *shares;
    char *hex;
    
    argi++;
    if( argi >= argc ) usage( NULL );

    if( binarymode ) {
      sl = (3*strlen( argv[argi] )) + 5;
      buf = malloc( sl );
      buflen = base64_decode( buf, sl, argv[argi] );
      if( buflen < 0 ) usage( "Failed to decode input buffer" );
    } else {
      buf = argv[argi];
      buflen = strlen( argv[argi] ) + 1;
    }
    
    secretlen = buflen;
    shares = malloc( sizeof(*shares) * n );
    for( i = 0; i < n; i++ ) {
      shares[i].flags = 0;
      shares[i].xval = 0;
      shares[i].sharebuf = malloc( secretlen );
    }
    sec_shamir_split( (uint8_t *)buf, secretlen, shares, n, k );

    hex = malloc( (4*secretlen) / 3 + 5 );
    for( row = 0; row < n; row++ ) {
      base64_encode( (char *)shares[row].sharebuf, secretlen, hex );
      printf( "%s\n", hex );
      free( shares[row].sharebuf );
    }
    free( shares );
    free( hex );
    if( binarymode ) free( buf );
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

    if( binarymode ) {
      char *sstr = malloc( (4*secretlen) / 3 + 5 );
      base64_encode( (char *)secret, secretlen, sstr );
      printf( "%s\n", sstr );
      free( sstr );
    } else {
      printf( "%s\n", secret );
    }
  } else usage( NULL );

  return 0;
}


