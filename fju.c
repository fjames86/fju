
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

int log_main( int argc, char **argv );
int rpc_main( int argc, char **argv );
int xdr_main( int argc, char **argv );
int raft_main( int argc, char **argv );
int reg_main( int argc, char **argv );
int cht_main( int argc, char **argv );
int fvm_main( int argc, char **argv );
int fvmc_main( int argc, char **argv );
int ecdh_main( int argc, char **argv );
int shamir_main( int argc, char **argv );

static struct {
  char *name;
  int (*main)( int argc, char **argv );
} modules[] = {
    { "log", log_main },
    { "rpc", rpc_main },
    { "xdr", xdr_main },
    { "raft", raft_main },
    { "reg", reg_main },
    { "cht", cht_main },
    { "fvm", fvm_main },
    { "fvmc", fvmc_main },
    { "ecdh", ecdh_main },
    { "shamir", shamir_main },
    { NULL, NULL }	       
};

static void usage( char *fmt, ... ) {
  va_list args;
  int i;
  
  printf( "Usage: fju <modname> ...\n"
	  "    With modname from: " );

  i = 0;
  while( modules[i].name ) {
    printf( "%s ", modules[i].name );
    i++;
  }
		   
  printf( "\n" );
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
  int i;
  
  if( argc < 2 ) usage( NULL );

  i = 0;
  while( modules[i].name ) {
    if( strcasecmp( modules[i].name, argv[1] ) == 0 ) {
      return modules[i].main( argc - 1, argv + 1 );
    }
    i++;
  }

  usage( NULL );

  return 0;
}
