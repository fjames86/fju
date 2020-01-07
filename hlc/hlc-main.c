
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include <fju/hlc.h>

static struct {
  int cmd;
#define CMD_READ 1
#define CMD_WRITE 2
#define CMD_PROP 3
#define CMD_LIST 4
  char *path;
  struct hlc_s hlc;
  uint64_t id;
} glob;

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
  printf( "Usage: hlc [-p path] [-r | -w]\n" );
  exit( 0 );
}
static void cmd_list( void );
static void cmd_write( void );
static void cmd_read( void );

int main( int argc, char **argv ) {
  int i;

  glob.cmd = CMD_LIST;
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-w" ) == 0 ) {
      glob.cmd = CMD_WRITE;
    } else if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.path = argv[i];
    } else if( strcmp( argv[i], "-r" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.id = strtoull( argv[i], NULL, 16 );
      glob.cmd = CMD_READ;
    } else usage( NULL );
    i++;
  }

  if( !glob.path ) usage( "Must specify path" );
  
  hlc_open( glob.path, &glob.hlc );
  switch( glob.cmd ) {
  case CMD_LIST:
    cmd_list();
    break;
  case CMD_READ:
    cmd_read();
    break;
  case CMD_WRITE:
    cmd_write();
    break;
  case CMD_PROP:
    break;
  }
  hlc_close( &glob.hlc );

  return 0;
}

static char *hash_str( hlc_hash_t hash, char *str ) {
    int i;
    strcpy( str, "" );
    for( i = 0; i < sizeof(hlc_hash_t); i++ ) {
	sprintf( str + strlen( str ), "%02x", hash[i] );
    }
    return str;
}

static void cmd_list( void ) {
  int sts, ne;
  struct hlc_entry entry;
  char *buf;
  int buflen;
  uint64_t id;
  char hashstr[64];
  
  buflen = 32*1024;
  buf = malloc( buflen );
  
  printf( "%-16s %-16s %-16s %-8s\n", "ID", "Seq", "PrevHash", "Len" );
  
  id = 0;
  do {
    memset( &entry, 0, sizeof(entry) );
    entry.id = id;
    entry.buf = buf;
    entry.len = buflen;
    sts = hlc_read( &glob.hlc, id, &entry, 1, &ne );
    if( sts || ne == 0 ) break;

    printf( "%-16"PRIx64" %-16"PRIx64" %-16s %-8u\n",
	    entry.id,
	    entry.seq,
	    hash_str( entry.prevhash, hashstr ),
	    entry.len );
    
    id = entry.id;
  } while( 1 );

  free( buf );
}

static void cmd_read( void ) {
  int sts, ne;
  struct hlc_entry entry;

  memset( &entry, 0, sizeof(entry) );
  entry.id = glob.id;
  entry.buf = malloc( 32 * 1024 );
  entry.len = 32 * 1024;
  sts = hlc_read( &glob.hlc, glob.id, &entry, 1, &ne );
  if( sts || ne == 0 ) usage( "Unknown entry" );

#ifdef WIN32
  WriteFile( GetStdHandle( STD_OUTPUT_HANDLE ), entry.buf, entry.len, NULL, NULL );
#else
  write( STDOUT_FILENO, entry.buf, entry.len );
#endif
  
  free( entry.buf );
}

static void cmd_write( void ) {
  char *buf;
  int msglen = 4*4096;
  int offset = 0;
  int sts;
  struct hlc_entry entry;
  
  buf = malloc( msglen );
  do {
    if( (msglen - offset) < 4096 ) {
      msglen += 4096;
      buf = realloc( buf, msglen );
    }
    
#ifdef WIN32
	{
		int b;
		b = ReadFile( GetStdHandle( STD_INPUT_HANDLE ), buf + offset, msglen - offset, (DWORD *)&sts, NULL );
		if( !b ) sts = -1;
	}
#else
    sts = read( STDIN_FILENO, buf + offset, msglen - offset );
#endif
    if( sts <= 0 ) break;
    offset += sts;
  } while( 1 ); 
  
  memset( &entry, 0, sizeof(entry) );
  entry.buf = buf;
  entry.len = offset;
  sts = hlc_write( &glob.hlc, &entry );
  if( sts ) usage( "Failed to write entry" );
  free( buf );
}
