

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>

#include <fju/sec.h>


static char *u8_to_hex( uint8_t *u8, int len, char *hex ) {
  int i;
  strcpy( hex, "" );
  for( i = 0; i < len; i++ ) {
    sprintf( hex + strlen( hex ), "%02x", u8[i] );
  }
  return hex;
}

static uint8_t *hex_to_u8( char *hex, uint8_t *u8 ) {
  char tmp[4];
  int i, len;
  len = strlen( hex ) / 2;
  for( i = 0; i < len; i++ ) {
    tmp[0] = hex[2*i];
    tmp[1] = hex[2*i + 1];
    tmp[2] = 0;
    u8[i] = strtoul( tmp, NULL, 16 );
  }
  return u8;
}


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


int main( int argc, char **argv ) {
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

    hex = malloc( (secretlen + 1) * 2 + 1 );
    for( row = 0; row < n; row++ ) {
      u8_to_hex( shares[row].sharebuf, secretlen + 1, hex );
      printf( "%s\n", hex );
      free( shares[row].sharebuf );
    }
    free( shares );
    free( hex );
  } else if( strcmp( argv[argi], "join" ) == 0 ) {
    int k, secretlen, i;
    struct sec_shamir_share *shares;
    uint8_t *secret;

    argi++;
    if( argi >= argc ) usage( NULL );
    k = argc - argi;
    secretlen = strlen(argv[argi]) - 1;
    shares = malloc( k * sizeof(*shares) );

    for( i = 0; i < k; i++ ) {
      shares[i].sharebuf = malloc( strlen( argv[argi] ) / 2 );;
      hex_to_u8( argv[argi], shares[i].sharebuf );
      argi++;
    }

    secret = malloc( secretlen );
    sec_shamir_join( secret, secretlen, shares, k );

    printf( "%s\n", secret );
  } else usage( NULL );

  return 0;
}
