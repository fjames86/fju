
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#define strcasecmp _stricmp
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <fju/fvm.h>
#include <fju/rpc.h>
#include <fju/freg.h>

static void usage( char *fmt, ... ) {
  va_list args;

  if( fmt == NULL ) {
    printf( "fvm OPTIONS file...\n"
	    "\n"
	    "\n OPTIONS:\n"
	    "   -m      module\n"
	    "   -s      start symbol\n"
	    "   -n      max steps\n"
	    "   -v      Verbose\n"
	    "   --u32 u32 | --u64 u64 | --str str  Set arg values\n" 
	    "\n" );
  }
  
  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
   
  exit( 1 );
}

static uint8_t argbuf[FVM_MAX_STACK];

int main( int argc, char **argv ) {
  int i, sts;
  char mname[64], sname[64];
  int nsteps = -1;
  uint32_t progid = 0, procid = 0;
  struct xdr_s argxdr;
  char *resbuf;
  struct fvm_module *module;
  
  xdr_init( &argxdr, argbuf, sizeof(argbuf) );
  
  memset( mname, 0, sizeof(mname) );
  memset( sname, 0, sizeof(sname) );
  
  i = 1;
  if( i >= argc ) usage( NULL );

  while( i < argc ) {
    if( strcmp( argv[i], "-h" ) == 0 ) {
      usage( NULL );
    } else if( strcmp( argv[i], "--help" ) == 0 ) {
      usage( NULL );
    } else if( strcmp( argv[i], "-m" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      strncpy( mname, argv[i], 63 );
    } else if( strcmp( argv[i], "-s" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      strncpy( sname, argv[i], 63 );
    } else if( strcmp( argv[i], "-n" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      nsteps = strtoul( argv[i], NULL, 10 );
    } else if( strcmp( argv[i], "-v" ) == 0 ) {
    } else if( strcmp( argv[i], "--u32" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      xdr_encode_uint32( &argxdr, strtoul( argv[i], NULL, 0 ) );
    } else if( strcmp( argv[i], "--u64" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      xdr_encode_uint64( &argxdr, strtoull( argv[i], NULL, 0 ) );
    } else if( strcmp( argv[i], "--str" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      xdr_encode_string( &argxdr, argv[i] );
    } else break;
    
    i++;
  }
  
  while( i < argc ) {

    sts = fvm_module_load_file( argv[i], NULL );
    if( sts ) usage( "Failed to load module \"%s\"", argv[i] );
    i++;
  }

  freg_open( NULL, NULL );
  
  sts = fvm_run( module, 0, NULL, NULL );
  if( sts ) usage( "Failed to run" );

  return 0;
}

