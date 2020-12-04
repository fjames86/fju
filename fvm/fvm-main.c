
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

#include "fvm-private.h"

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
  static struct fvm_s state;
    
  int i, sts;
  char mname[64], sname[64];
  int nsteps = -1;
  uint32_t progid = 0, procid = 0;
  struct xdr_s argxdr;
  char *resbuf;
  
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
      fvm_debug( 1 );
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
    fvm_printf( "Loading module %s\n", argv[i] );
    sts = fvm_module_load_file( argv[i], NULL );
    if( sts ) usage( "Failed to load module \"%s\"", argv[i] );
    i++;
  }

  {
    struct fvm_module_info *minfo;
    int n, m;

    fvm_printf( "-------------- Modules --------------\n" );
    n = fvm_module_list( NULL, 0 );
    if( n < 0 ) usage( "Failed to get modules" );
    minfo = malloc( sizeof(*minfo) * n );
    m = fvm_module_list( minfo, n );
    if( m < n ) m = n;
    for( m = 0; m < n; m++ ) {
      fvm_printf( "Module %s %u:%u DataSize %u TextSize %u\n", minfo[m].name, minfo[m].progid, minfo[m].versid, minfo[m].datasize, minfo[m].textsize );
      if( !mname[0] ) strcpy( mname, minfo[m].name );
      if( strcasecmp( mname, minfo[m].name ) == 0 ) progid = minfo[m].progid;
    }
    free( minfo );
    fvm_printf( "--------------------------------------\n" );
  }

  if( !mname[0] ) usage( "No module" );
  fvm_printf( "Initializing to module %s\n", mname );

  {
    struct fvm_symbol *sym;
    int n, m;

    fvm_printf( " ------------- Symbols -------------- \n" );
    
    n = fvm_module_symbols( fvm_progid_by_name( mname ), NULL, 0 );
    if( n < 0 ) usage( "Failed to get module symbols" );
    if( n > 0 ) {
      sym = malloc( sizeof(*sym) * n );
      m = fvm_module_symbols( fvm_progid_by_name( mname ), sym, n );
      if( m < n ) n = m;
      for( m = 0; m < n; m++ ) {
	fvm_printf( "%-2u %-16s = 0x%04x\n", m, sym[m].name, sym[m].addr );
	if( !sname[0] ) strcpy( sname, sym[m].name );
	if( strcasecmp( sym[m].name, sname ) == 0 ) procid = m;
      }
      free( sym );
    }

    fvm_printf( " ------------------------------------ \n" );
  }
  
  if( !sname[0] ) usage( "No function" );
  fvm_printf( "Calling function %s procid %u\n", sname, procid );
  
  sts = fvm_state_init( &state, progid, procid );
  if( sts ) usage( "Failed to initialize" );

  freg_open( NULL, NULL );
  
  if( argxdr.offset > 0 ) {
    fvm_set_args( &state, (char *)argbuf, argxdr.offset );
  }

  sts = fvm_run( &state, nsteps );
  if( sts ) usage( "Failed to run" );

  resbuf = NULL;
  sts = fvm_get_res( &state, &resbuf );
  if( resbuf && sts > 0 ) {
    for( i = 0; i < sts; i++ ) {
      if( i && (i % 16) == 0 ) {
	printf( "\n" );
      }
      printf( "%02x ", (uint32_t)resbuf[i] );
    }
  }
  
  return 0;
}

