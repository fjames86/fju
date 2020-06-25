

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <fju/fvm2.h>

#include "fvm2-private.h"

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "fvm2 OPTIONS file...\n"
	  "\n"
	  "\n OPTIONS:\n"
	  "   -m      module\n"
	  "   -s      start symbol\n"
	  "   -n      max steps\n"
	  "   -v      Verbose\n"
	  "\n" );
  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
   
  exit( 1 );
}

int main( int argc, char **argv ) {
  static struct fvm2_s state;
    
  int i, sts;
  char mname[64], sname[64];
  int nsteps = -1;
  
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
      fvm2_debug( 1 );
    } else break;
    
    i++;
  }
  
  while( i < argc ) {
    fvm2_printf( "Loading module %s\n", argv[i] );
    sts = fvm2_module_load( argv[i] );
    if( sts ) usage( "Failed to load module \"%s\"", argv[i] );
    i++;
  }

  {
    struct fvm2_module_info *minfo;
    int n, m;

    fvm2_printf( "-------------- Modules --------------\n" );
    n = fvm2_module_list( NULL, 0 );
    if( n < 0 ) usage( "Failed to get modules" );
    minfo = malloc( sizeof(*minfo) * n );
    m = fvm2_module_list( minfo, n );
    if( m < n ) m = n;
    for( m = 0; m < n; m++ ) {
      fvm2_printf( "Module %s %u:%u DataSize %u TextSize %u\n", minfo[m].name, minfo[m].progid, minfo[m].versid, minfo[m].datasize, minfo[m].textsize );
      if( !mname[0] ) strcpy( mname, minfo[m].name );
    }
    free( minfo );
    fvm2_printf( "--------------------------------------\n" );
  }

  if( !mname[0] ) usage( "No module" );
  fvm2_printf( "Initializing to module %s\n", mname );

  {
    struct fvm2_symbol *sym;
    int n, m;

    fvm2_printf( " ------------- Symbols -------------- \n" );
    
    n = fvm2_module_symbols( mname, NULL, 0 );
    if( n < 0 ) usage( "Failed to get module symbols" );
    if( n > 0 ) {
      sym = malloc( sizeof(*sym) * n );
      m = fvm2_module_symbols( mname, sym, n );
      if( m < n ) n = m;
      for( m = 0; m < n; m++ ) {
	fvm2_printf( "%-2u %-16s = 0x%04x\n", m, sym[m].name, sym[m].addr );
	if( !sname[0] ) strcpy( sname, sym[m].name );
      }
      free( sym );
    }

    fvm2_printf( " ------------------------------------ \n" );
  }
  
  if( !sname[0] ) usage( "No function" );
  fvm2_printf( "Calling function %s\n", sname );
  
  sts = fvm2_state_init( mname, sname, NULL, 0, &state );
  if( sts ) usage( "Failed to initialize" );
  
  sts = fvm2_run( &state, nsteps );
  if( sts ) usage( "Failed to run" );
  
  return 0;
}

