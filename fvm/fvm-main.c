
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <fju/mmf.h>
#include <fju/fvm.h>

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "fvm -p program [-v]\n" );
  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
   
  exit( 1 );
}

static struct {
  struct fvm_state fvm;
  int verbose;
} glob;

int main( int argc, char **argv ) {
  char *path = NULL;
  struct mmf_s mmf;
  int sts;
  int i;
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      path = argv[i];
    } else if( strcmp( argv[i], "-v" ) == 0 ) {
      glob.verbose = 1;
    } else usage( NULL );
    i++;
  }
  if( path == NULL ) usage( NULL );
  
  sts = mmf_open( path, &mmf );
  if( sts ) usage( "Failed to open program" );
  sts = mmf_remap( &mmf, mmf.fsize );
  if( sts ) usage( "Failed to map program" );
  fvm_load( &glob.fvm, mmf.file, mmf.fsize );
  if( glob.verbose ) glob.fvm.flags |= FVM_FLAG_VERBOSE;
  mmf_close( &mmf );

  fvm_run( &glob.fvm );

  if( glob.verbose ) {
    printf( ";; R0 %x R1 %x R2 %x R3 %x R4 %x R5 %x R6 %x R7 %x PC %x\n",
	    glob.fvm.reg[0], glob.fvm.reg[1],
	    glob.fvm.reg[2], glob.fvm.reg[3], 
	    glob.fvm.reg[4], glob.fvm.reg[5],
	    glob.fvm.reg[6], glob.fvm.reg[7],
	    glob.fvm.reg[8] );
  }
  
  return 0;
}

