

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <fju/fvm2.h>

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "fvm2 OPTIONS file...\n"
	  "\n"
	  "\n OPTIONS:\n"
	  "   -m      module\n"
	  "   -s      start symbol\n"
	  "   -n      max steps\n"
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
  char *mname = "default";
  char *sname = "main";
  int nsteps = -1;
  
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
      mname = argv[i];
    } else if( strcmp( argv[i], "-s" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      sname = argv[i];
    } else if( strcmp( argv[i], "-n" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      nsteps = strtoul( argv[i], NULL, 10 );
    } else break;
    
    i++;
  }
  
  while( i < argc ) {
    printf( "Loading module %s\n", argv[i] );
    sts = fvm2_module_load( argv[i] );
    if( sts ) usage( "Failed to load module \"%s\"", argv[i] );
    i++;
  }

  {
    struct fvm2_module_info *minfo;
    int n, m;

    printf( "------------ Modules ------------\n" );
    n = fvm2_module_list( NULL, 0 );
    if( n < 0 ) usage( "Failed to get modules" );
    minfo = malloc( sizeof(*minfo) * n );
    m = fvm2_module_list( minfo, n );
    if( m < n ) m = n;
    for( m = 0; m < n; m++ ) {
      printf( "Module %s DataSize %u TextSize %u\n", minfo[m].name, minfo[m].datasize, minfo[m].textsize );
    }
    free( minfo );
  }

  printf( "Initializing to module %s function %s\n", mname, sname );

  {
    struct fvm2_symbol *sym;
    int n, m;

    printf( " ------------- Symbols -------------- \n" );
    
    n = fvm2_module_symbols( mname, NULL, 0 );
    if( n < 0 ) usage( "Failed to get module symbols" );
    if( n > 0 ) {
      sym = malloc( sizeof(*sym) * n );
      m = fvm2_module_symbols( mname, sym, n );
      if( m < n ) n = m;
      for( m = 0; m < n; m++ ) {
	printf( "Symbol %-16s = %04x\n", sym[m].name, sym[m].addr );
      }
      free( sym );
    }
  }
  
  sts = fvm2_state_init( mname, sname, NULL, 0, &state );
  if( sts ) usage( "Failed to initialize" );
  
  sts = fvm2_run( &state, nsteps );
  if( sts ) usage( "Failed to run" );
  
  return 0;
}

