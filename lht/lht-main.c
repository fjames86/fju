
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>
#include "lht.h"

static struct {
  char *filepath;
  int cmd;
#define CMD_LIST      1
#define CMD_PUT       2
#define CMD_GET       3 
#define CMD_REM       4
#define CMD_RESET     5
#define CMD_PROP      6 
  int flags;
  char *key;
  struct lht_s lht;
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

  printf( "lht [-p path] [get KEY [-b] | rem KEY | put KEY | reset | prop ]\n"
	  "\n" );
  exit( 0 );
}

static void cmd_read( void );
static void cmd_write( void );
static void cmd_list( void );

int main( int argc, char **argv ) {
  int i, sts;

  glob.cmd = CMD_LIST;

  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.filepath = argv[i];
    } else break;
    i++;
  }

  if( i < argc ) {
    if( strcmp( argv[i], "get" ) == 0 ) {
      glob.cmd = CMD_GET;
      i++;
      if( i >= argc ) usage( NULL );
      glob.key = argv[i];
      i++;
      while( i < argc ) {
	if( strcmp( argv[i], "-b" ) == 0 ) {
	  glob.flags |= LOG_BINARY;
	} else usage( NULL );
	
	i++;
      }
    } else if( strcmp( argv[i], "rem" ) == 0 ) {
      glob.cmd = CMD_REM;
      i++;
      if( i >= argc ) usage( NULL );
      glob.key = argv[i];
    } else if( strcmp( argv[i], "put" ) == 0 ) {
      glob.cmd = CMD_PUT;
      i++;
      if( i >= argc ) usage( NULL );
      glob.key = argv[i];
    } else if( strcmp( argv[i], "reset" ) == 0 ) {
      glob.cmd = CMD_RESET;
    } else if( strcmp( argv[i], "prop" ) == 0 ) {
      glob.cmd = CMD_PROP;
    } else usage( NULL );
  }
  
  if( !glob.filepath ) glob.filepath = "lht.dat";

  sts = lht_open( glob.filepath, &glob.lht );
  if( sts ) usage( "Failed to open" );

  switch( glob.cmd ) {
  case CMD_LIST:
    cmd_list();
    break;
  case CMD_GET:
    cmd_read();
    break;
  case CMD_PUT:
    cmd_write();
    break;
  case CMD_REM:
    if( !glob.key ) usage( NULL );
    lht_rem( &glob.lht, glob.key );
    break;
  case CMD_RESET:
    lht_reset( &glob.lht );
    break;
  case CMD_PROP:
    {
      struct log_prop prop;
      char *unit = "";
      int mem;
      log_prop( &glob.lht.log, &prop );

      mem = glob.lht.memcount;
      if( mem > 1024 ) {
	mem /= 1024;
	unit = "k";
      }
      if( mem > 1024 ) {
	mem /= 1024;
	unit = "M";
      }
      printf( "Path: %s\n", glob.filepath );
      printf( "LHT EntryCount %d/%d (%d%%) MemCount %d%s (%d bytes)\n",
	      glob.lht.count, glob.lht.nbuckets, (100 * glob.lht.count) / glob.lht.nbuckets,
	      mem, unit, glob.lht.memcount );

      mem = prop.count * LOG_LBASIZE;
      unit = "";
      if( mem > 1024 ) {
	mem /= 1024;
	unit = "k";
      }
      if( mem > 1024 ) {
	mem /= 1024;
	unit = "M";
      }
      printf( "LOG Seq %"PRIu64" LBACount %u (%uM) Count %u (%d%s)\n", 
	      prop.seq,
	      prop.lbacount,
	      (prop.lbacount * LOG_LBASIZE) / (1024*1024),
	      prop.count,
	      mem, unit );
    }
  }

  lht_close( &glob.lht );

  return 0;
}


static void cmd_list( void ) {
  int i;
  struct lht_entry *entry;
  
  for( i = 0; i < glob.lht.nbuckets; i++ ) {
    entry = glob.lht.buckets[i];
    while( entry ) {
      printf( "%-16s %08x %-8d\n", entry->key, entry->hash, entry->len );
      entry = entry->next;
    }
  }
}

static void cmd_read( void ) {
  struct lht_entry *entry;

  if( !glob.key ) usage( NULL );
  
  entry = lht_get( &glob.lht, glob.key );
  if( !entry ) usage( "Failed to find entry \"%s\"", glob.key );
  
  if( glob.flags & LOG_BINARY ) {
#ifdef WIN32
    WriteFile( GetStdHandle( STD_OUTPUT_HANDLE ), entry->buf, entry->len, NULL, NULL );
#else
    write( STDOUT_FILENO, entry->buf, entry->len );
#endif
  } else {
    int i;
    printf( "  0000  " );
    for( i = 0; i < entry->len; i++ ) {
      if( ((i % 8) == 0) && ((i % 16) != 0) ) printf( "  " );
      if( i && ((i % 16) == 0) ) printf( "\n  %04o  ", i );
      
      printf( "%02x ", (uint8_t)entry->buf[i] );
    }
    printf( "\n" );
  }
  
}

static void cmd_write( void ) {
  char *buf;
  int msglen = 4*4096;
  int offset = 0;
  int sts;

  if( !glob.key ) usage( NULL );
    
  buf = malloc( msglen );
  do {
    if( (msglen - offset) < 4096 ) {
      msglen += 4096;
      buf = realloc( buf, msglen );
    }
    
#ifdef WIN32
	{
		int b;
		b = ReadFile( GetStdHandle( STD_INPUT_HANDLE ), buf + offset, msglen - offset, &sts, NULL );
		if( !b ) sts = -1;
	}
#else
    sts = read( STDIN_FILENO, buf + offset, msglen - offset );
#endif
    if( sts <= 0 ) break;
    offset += sts;
  } while( 1 ); 

  sts = lht_put( &glob.lht, glob.key, buf, offset );
  if( sts ) usage( "Failed to put entry" );
  
  free( buf );
}

