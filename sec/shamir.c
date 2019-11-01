

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>

#include <fju/sec.h>


static char *arr_to_hex_str( uint8_t *arr, int arr_size ) {
  char *out = malloc(2 * arr_size + 1);
  for (int pos = 0; pos < arr_size; pos++) {
    sprintf(out + 2*pos, "%02x", arr[pos]);
  }
  out[2 * arr_size + 1] = 0x00;
  return out;
}

static uint8_t * hex_str_to_arr(const char *s) {
  // / 2 ?
  uint8_t *res = malloc(strlen(s) * sizeof(uint8_t));
  char buff[3] = {0x00, 0x00, 0x00};
  for (int pos = 0; pos < strlen(s); pos++) {
    strncpy(buff, s + pos*2, 2);
    res[pos] = strtoul(buff, NULL, 16);
  }
  return res;
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


int main(int argc, char *argv[]) {
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
    int secret_size, i, row;
    char *buf;
    struct sec_shamir_share *shares;
    
    argi++;
    if( argi >= argc ) usage( NULL );
    buf = argv[argi];
    
    secret_size = strlen( buf );
    shares = malloc( sizeof(*shares) * n );
    for( i = 0; i < n; i++ ) {
      shares[i].flags = 0;
      shares[i].xval = 0;
      shares[i].sharebuf = malloc( secret_size + 1 );
    }
    sec_shamir_split( (uint8_t *)buf, secret_size, shares, n, k );
    
    for( row = 0; row < n; row++ ) {
      printf("%s\n", arr_to_hex_str(shares[row].sharebuf, secret_size + 1));
      free( shares[row].sharebuf );
    }
    free( shares );
  } else if (strcmp(argv[argi], "join") == 0) {
    int k, secret_size, i;
    struct sec_shamir_share *shares;
    uint8_t *reconstructed_secret;

    argi++;
    if( argi >= argc ) usage( NULL );
    k = argc - argi;
    secret_size = strlen(argv[argi]) - 1;
    shares = malloc(k * sizeof(*shares) );

    for( i = 0; i < k; i++ ) {
      shares[i].sharebuf = hex_str_to_arr(argv[argi]);
      argi++;
    }

    reconstructed_secret = malloc( secret_size );
    sec_shamir_join( reconstructed_secret, secret_size, shares, k );

    printf("%s\n", (char *) reconstructed_secret);
  } else usage( NULL );

  return 0;
}
