
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include <fju/sec.h>
#include <fju/lic.h>

static void usage( char *fmt, ... ) {
  printf( "Usage: fjlic hostid [-v version] [-D days]\n" 
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

static uint8_t privkeybuf[] =
  {
   0xee, 0xf0, 0x99, 0x48, 0xef, 0x76, 0x72, 0xb9, 0xbd, 0xe1, 0xaa,
    0x09, 0xdf, 0x53, 0x54, 0xac, 0x07, 0xee, 0xe5, 0xa5, 0x60, 0x68,
    0x64, 0x8d, 0x43, 0xa0, 0x42, 0x84, 0x0c, 0x6f, 0x58, 0x5a
  };

int main( int argc, char **argv ) {
  int sts, i, version, days;
  struct lic_s lic;
  struct sec_buf privkey, iov[1], sig;
  uint64_t hostid;
  char *term;
  char str[256];
  uint8_t *p;
  
  if( argc < 2 ) usage( NULL );
  hostid = strtoull( argv[1], &term, 16 );
  if( *term ) usage( "Failed to parse hostid" );
  
  version = 1;
  days = 365;
  i = 2;
  while( i < argc ) {
    if( strcmp( argv[i], "-v" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      version = strtoul( argv[i], NULL, 0 );
    } else if( strcmp( argv[i], "-D" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      days = strtoul( argv[i], NULL, 0 );
    } else usage( NULL );
    i++;
  }
  
  memset( &lic, 0, sizeof(lic) );
  lic.hostid = hostid;
  lic.expire = time( NULL ) + (60*60*24*days);
  lic.version = version;

  privkey.buf = (char *)privkeybuf;
  privkey.len = sizeof(privkeybuf);
  iov[0].buf = (char *)&lic;
  iov[0].len = 32;
  sig.buf = lic.verf;
  sig.len = sizeof(lic.verf);
  sts = sec_sign( &privkey, iov, 1, &sig );
  if( sts ) usage( "Failed to sign" );
  lic.nverf = sig.len;
  
  p = (uint8_t *)&lic;
  strcpy( str, "" );
  for( i = 0; i < sizeof(lic); i++ ) {
    sprintf( str + strlen( str ), "%02x", (uint32_t)p[i] );
  }
  
  printf( "put /fju/lic opaque %s\n", str );
  
  return 0;
}
