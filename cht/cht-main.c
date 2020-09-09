
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>

#include <fju/cht.h>

static void usage( char *fmt, ... ) {
  if( fmt ) {
    va_list args;
    printf( "Error: " );
    va_start( args, fmt );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
    exit( 1 );
  }

  printf( "cht [-p path] [-f] [-o path] [-r <id>] [-w]\n"
	  "\n Where:\n"
	  "    -p path        Path to database file\n"
	  "    -o path        Path to input/output file\n"
	  "    -f             Operate on whole file (default is single block)\n"
	  "    -r id          Read a given ID\n"
	  "    -w             Write\n"
	  "\n"
	  "If the -f option is provided, reading first reads an entry for the given id.\n"
	  "This contains a list of hashes that are each used to resolve file blocks.\n"
	  "Writing will write an entry for each block of the file and then write an entry containing\n"
	  "all the file block hashes.\n"
	  "Without the -f option, the contents of the block named by ID are resolved.\n" 
	  "\n" );
  exit( 0 );
}

static struct {
  struct cht_s cht;
} glob;

int main( int argc, char **argv ) {
  int i, sts;
  char *path = NULL;
  char *fpath = NULL;
  int opfile = 0;
  int opread = 0;
  int opwrite = 0;
  char *idstr = NULL;
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      path = argv[i];
    } else if( strcmp( argv[i], "-o" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      fpath = argv[i];
    } else if( strcmp( argv[i], "-f" ) == 0 ) {
      opfile = 1;
    } else if( strcmp( argv[i], "-r" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      idstr = argv[i];
      opread = 1;
    } else if( strcmp( argv[i], "-w" ) == 0 ) {
      opwrite = 1;
    } else usage( NULL );
    i++;
  }

  if( path == NULL ) path = mmf_default_path( "cht.dat", NULL );
  sts = cht_open( path, &glob.cht, NULL );
  if( sts ) usage( "Failed to open database" );

  if( opread ) {
    if( opfile ) {
      /* read file descriptor block */

      /* read each entry and write its contents into the output file (or stdout) */
    } else {
      /* read entry for given id */
      sts = cht_read( &glob.cht, key, NULL, glob.buf, sizeof(glob.buf) );

      if( fpath ) {
      } else {
      }
      
    }
  } else if( opwrite ) {
    if( opfile ) {
      /* read from file, writing each block */

      /* write file descriptor block */
    } else {
      /* read from stdin */

      /* write entry */
    }
  } else usage( NULL );

  
  cht_close( &glob.cht );
    
  return 0;
}

