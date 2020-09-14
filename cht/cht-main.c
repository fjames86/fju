
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#define strcasecmp _stricmp
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

#include <fju/cht.h>
#include <fju/sec.h>

#include "cht-private.h"

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

  printf( "Usage: cht [OPTIONS] [-r <id> | -d ID | -D | -w | -l]\n"
	  "\n Where OPTIONS:\n"
	  "    -p path        Path to database file\n"
	  "    -o path        Path to input/output file\n"
	  "    -f             Operate on whole file (default is single block)\n"
	  "    -F flags       Set flags on write, valid flags are: sticky\n"
	  "    -E key         Encrypt contents with key\n"
	  "\n" 
	  "   Commands:\n" 
	  "    -r id          Read a given ID\n"
	  "    -d id          Delete a given ID\n"
	  "    -D             Delete all non-sticky entries\n" 
	  "    -w             Write\n"
	  "    -l             List entries\n"
	  "\n" );
  exit( 0 );
}

static struct {
  struct cht_s cht;
  char buf[CHT_BLOCK_SIZE];
} glob;

static void print_entry( struct cht_entry *e, int printinfo );
static void parsekey( char *idstr, char *key );
static uint32_t parseflags( char *str );

int main( int argc, char **argv ) {
  int i, sts;
  char *path = NULL;
  char *fpath = NULL;
  int opfile = 0;
  int opread = 0;
  int opwrite = 0;
  int opdelete = 0;
  int oplist = 0;
  char *idstr = NULL;
  struct cht_entry entry;
  struct cht_opts opts;
  uint32_t flags = 0;
  
  memset( &opts, 0, sizeof(opts) );
  
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
    } else if( strcmp( argv[i], "-c" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      opts.count = strtoul( argv[i], NULL, 0 );
      opts.mask |= CHT_OPT_COUNT;
    } else if( strcmp( argv[i], "-l" ) == 0 ) {
      oplist = 1;
    } else if( strcmp( argv[i], "-d" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      idstr = argv[i];      
      opdelete = 1;
    } else if( strcmp( argv[i], "-D" ) == 0 ) {
      opdelete = 2;
    } else if( strcmp( argv[i], "-F" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      flags = parseflags( argv[i] );
    } else if( strcmp( argv[i], "-E" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      parsekey( argv[i], (char *)opts.ekey );
      opts.mask |= CHT_OPT_ENCRYPT;
    } else usage( NULL );
    i++;
  }

  sts = cht_open( path, &glob.cht, &opts );
  if( sts ) usage( "Failed to open database" );

  if( opread ) {
    if( opfile ) {
      /* read file descriptor block */

      /* read each entry and write its contents into the output file (or stdout) */
      usage( "TODO: implement reading full files" );
    } else {
      /* read entry for given id */
      char key[CHT_KEY_SIZE];
      
      parsekey( idstr, key );
            
      sts = cht_read( &glob.cht, key, &entry, glob.buf, sizeof(glob.buf) );
      if( sts ) usage( "Failed to read entry" );
      fju_writestdout( glob.buf, entry.flags & CHT_SIZE_MASK );
    }
  } else if( opwrite ) {
    if( opfile ) {
      if( !fpath ) usage( "Need file path" );
      
      /* read from file, writing each block */
      struct mmf_s mmf;
      sts = mmf_open2( fpath, &mmf, MMF_OPEN_EXISTING );
      if( sts ) usage( "Failed to open input file" );
      for( i = 0; i < mmf.fsize; i += CHT_BLOCK_SIZE ) {
	sts = mmf_read( &mmf, glob.buf, CHT_BLOCK_SIZE, i );
	sts = cht_write( &glob.cht, glob.buf, sts, flags, &entry );
	if( sts ) usage( "Failed to write entry" );
	print_entry( &entry, 0 );
      }
      mmf_close( &mmf );
      
      /* write file descriptor block? */
      //printf( "TODO: write file descriptor block\n" );
    } else {
      /* read from stdin */
      sts = fju_readstdin( glob.buf, sizeof(glob.buf) );
      
      /* write entry */
      sts = cht_write( &glob.cht, glob.buf, sts, flags, &entry );
      if( sts ) usage( "Failed to write entry" );
      print_entry( &entry, 1 );
    }
  } else if( opdelete == 1 ) {
    char key[CHT_KEY_SIZE];

    parsekey( idstr, key );
    sts = cht_delete( &glob.cht, key );
    if( sts ) usage( "Failed to delete entry" );
  } else if( opdelete == 2 ) {
    sts = cht_purge( &glob.cht, CHT_STICKY, 0 ); /* delete all non-sticky entries */
  } else if( oplist ) {
    for( i = 0; i < glob.cht.count; i++ ) {
      sts = cht_entry_by_index( &glob.cht, i, 0, &entry );
      if( sts == 0 ) print_entry( &entry, 1 );
    }
    
  } else {
    struct cht_prop prop;
    cht_prop( &glob.cht, &prop );
    printf( "Tag %0"PRIx64" Seq %"PRIu64" Count %u/%u (%u%%)\n",
	    prop.tag,
	    prop.seq,
	    prop.fill,
	    prop.count,
	    (100 * prop.fill) / prop.count );
  }
  
  cht_close( &glob.cht );
    
  return 0;
}

static void print_entry( struct cht_entry *e, int printinfo ) {
  int i;
  for( i = 0; i < CHT_KEY_SIZE; i++ ) {
    printf( "%02x", (uint32_t)e->key[i] );
  }
  if( printinfo ) {
    printf( " Seq %-4u Size %-4u Flags 0x%08x %s",
	    e->seq,
	    e->flags & CHT_SIZE_MASK,
	    e->flags & ~CHT_SIZE_MASK,
	    e->flags & CHT_STICKY ? "Sticky " : "" );
  }
  printf( "\n" );
}

static void parsekey( char *idstr, char *key ) {
  int i;
  uint8_t x;
  
  memset( key, 0, CHT_KEY_SIZE );
  
  for( i = 0; i < CHT_KEY_SIZE; i++ ) {
    if( idstr[2*i] == '\0' ) break;
    
    x = 0;
    if( idstr[2*i] >= '0' && idstr[2*i] <= '9' ) x = idstr[2*i] - '0';
    else if( idstr[2*i] >= 'a' && idstr[2*i] <= 'f' ) x = idstr[2*i] - 'a' + 10;
    else if( idstr[2*i] >= 'A' && idstr[2*i] <= 'F' ) x = idstr[2*i] - 'A' + 10;
    else usage( "bad idstring" );
    
    x <<= 4;
    if( idstr[2*i + 1] != '\0' ) {
      if( idstr[2*i + 1] >= '0' && idstr[2*i + 1] <= '9' ) x |= idstr[2*i + 1] - '0';
      else if( idstr[2*i + 1] >= 'a' && idstr[2*i + 1] <= 'f' ) x |= idstr[2*i + 1] - 'a' + 10;
      else if( idstr[2*i + 1] >= 'A' && idstr[2*i + 1] <= 'F' ) x |= idstr[2*i + 1] - 'A' + 10;
      else usage( "bad idstring" );
    }
    
    key[i] = x;
  }  
}

static uint32_t parseflags( char *str ) {
  char flagstr[32];
  char *p;
  uint32_t flags = 0;

  memset( flagstr, 0, sizeof(flagstr) );
  p = flagstr;
  while( 1 ) {
    if( *str == ',' || *str == '\0' ) {
      if( strcasecmp( flagstr, "sticky" ) == 0 ) {
	flags |= CHT_STICKY;
      } else usage( "Unknown flag \"%s\"", flagstr );
      memset( flagstr, 0, sizeof(flagstr) );      
      p = flagstr;

      if( *str == '\0' ) break;
      str++;
      continue;
    }
    *p = *str;
    str++;
    p++;
  }
      
  return flags;    
}
