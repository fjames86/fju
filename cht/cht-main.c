
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
#include <fju/nls.h>

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

  printf( "Usage: cht [OPTIONS] COMMAND\n"
	  "\n Where OPTIONS:\n"
	  "    -p path        Path to database file\n"
	  "    -o path        Path to input/output file\n"
	  "    -I id          Entry ID\n" 
	  "    -f             Operate on whole file (default is single block)\n"
	  "    -F flags       Set flags on write, valid flags are: sticky, readonly\n"
	  "    -c count       Table count\n"
	  "    -A hshare      Set alog hshare\n"
	  "    -C cookie      Set cookie on write\n" 
	  "\n" 
	  "   COMMAND:\n" 
	  "    -r             Read a given ID\n"
	  "    -d             Delete a given ID\n"
	  "    -D             Delete all non-sticky entries\n" 
	  "    -w             Write\n"
	  "    -l             List entries\n"
	  "    -s             Set flags\n"
	  "    -R             Rehash. Pass -o for new table path and -c for new table count\n"
	  "\n"
	  "Tables may be synced remoted with the following process:\n"
	  " - On server machine create an audit log with cookie=CHT. Share it with NLS.\n"
	  " - On server machine create an freg entry /fju/cht/local/xxx str /path/to/cht/database where xxx is the NLS share handle.\n" 
	  " - On client machine, register this remote log with NLS. Ensure its cookie=CHT.\n"
	  " - On client machine, create an freg entry /fju/cht/remote/xxx str /path/to/cht/database where xxx is the NLS share handle.\n"
	  " - From this point, writes to the server's database should be sent to the client machine.\n" 
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
static void cht_rehash( struct cht_s *cht, char *outpath, int newcount );

int main( int argc, char **argv ) {
  int i, sts;
  char *path = NULL;
  char *fpath = NULL;
  int opfile = 0;
  int opread = 0;
  int opwrite = 0;
  int opdelete = 0;
  int oplist = 0;
  int opsetflags = 0;
  int oprehash = 0;
  char *idstr = NULL;
  struct cht_entry entry;
  struct cht_opts opts;
  uint32_t flags = 0;
  uint64_t alog_hshare = 0;
  char cookie[CHT_MAX_COOKIE];
  
  memset( &opts, 0, sizeof(opts) );
  memset( cookie, 0, sizeof(cookie) );
  
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
    } else if( strcmp( argv[i], "-I" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      idstr = argv[i];      
    } else if( strcmp( argv[i], "-r" ) == 0 ) {
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
      opdelete = 1;
    } else if( strcmp( argv[i], "-D" ) == 0 ) {
      opdelete = 2;
    } else if( strcmp( argv[i], "-F" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      flags = parseflags( argv[i] );
    } else if( strcmp( argv[i], "-s" ) == 0 ) {
      opsetflags = 1;
    } else if( strcmp( argv[i], "-R" ) == 0 ) {
      oprehash = 1;
    } else if( strcmp( argv[i], "-A" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      alog_hshare = strtoull( argv[i], NULL, 16 );
    } else if( strcmp( argv[i], "-C" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      strncpy( cookie, argv[i], sizeof(cookie) );
    } else usage( NULL );
    i++;
  }

  nls_open();

  if( alog_hshare ) {
    sts = cht_open( path, &glob.cht, &opts );
    if( sts ) usage( "Failed to open database" );

    cht_set_alog( &glob.cht, alog_hshare );

    cht_close( &glob.cht );
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

      if( !idstr ) usage( "Need ID" );      
      parsekey( idstr, key );
            
      sts = cht_read( &glob.cht, key, glob.buf, sizeof(glob.buf), &entry );
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
	memset( &entry, 0, sizeof(entry) );	
	entry.flags = flags;
	memcpy( entry.cookie, cookie, CHT_MAX_COOKIE );
	sts = cht_write( &glob.cht, &entry, glob.buf, sts );
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
      memset( &entry, 0, sizeof(entry) );
      if( idstr ) parsekey( idstr, (char *)entry.key );
      entry.flags = flags;
      memcpy( entry.cookie, cookie, CHT_MAX_COOKIE );      
      sts = cht_write( &glob.cht, &entry, glob.buf, sts );
      if( sts ) usage( "Failed to write entry" );

      print_entry( &entry, 1 );
    }
  } else if( opdelete == 1 ) {
    char key[CHT_KEY_SIZE];
    if( !idstr ) usage( "Need ID" );
    
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
  } else if( opsetflags ) {
    char key[CHT_KEY_SIZE];
    if( !idstr ) usage( "Need ID" );
    parsekey( idstr, key );
    sts = cht_set_flags( &glob.cht, key, 0xffff0000, flags );
    if( sts ) usage( "Failed to set flags" );
  } else if( oprehash ) {
    if( !fpath ) usage( "Need new table path" );
    if( !(opts.mask & CHT_OPT_COUNT) ) opts.count = (glob.cht.count * 3) / 2;
    cht_rehash( &glob.cht, fpath, opts.count );
  } else {
    struct cht_prop prop;
    cht_prop( &glob.cht, &prop );
    printf( "Tag %0"PRIx64" Seq %"PRIu64" Count %u/%u (%u%%)\n",
	    prop.tag,
	    prop.seq,
	    prop.fill,
	    prop.count,
	    (100 * prop.fill) / prop.count );
    if( prop.alog_hshare ) printf( "Audit log: %"PRIx64"\n", prop.alog_hshare );
  }
  
  cht_close( &glob.cht );

  nls_close();
  
  return 0;
}

static void print_entry( struct cht_entry *e, int printinfo ) {
  int i;
  for( i = 0; i < CHT_KEY_SIZE; i++ ) {
    printf( "%02x", (uint32_t)e->key[i] );
  }
  if( printinfo ) {
    printf( " Seq %-4u Size %-4u Flags 0x%08x %s%s",
	    e->seq,
	    e->flags & CHT_SIZE_MASK,
	    e->flags & ~CHT_SIZE_MASK,
	    e->flags & CHT_STICKY ? "Sticky " : "",
	    e->flags & CHT_READONLY ? "Readonly " : "" );
    if( *((uint64_t *)e->cookie) ) {
      printf( "Cookie " );
      for( i = 0; i < CHT_MAX_COOKIE; i++ ) {
	printf( "%02x", (uint32_t)e->cookie[i] );
      }
      printf( " (" );
      for( i = 0; i < CHT_MAX_COOKIE; i++ ) {
	printf( "%c", e->cookie[i] );
      }
      printf( ")" );
    }
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
      } else if( strcasecmp( flagstr, "readonly" ) == 0 ) {
	flags |= CHT_READONLY;	
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

static void cht_rehash( struct cht_s *cht, char *outpath, int newcount ) {
  int sts;
  struct cht_s cht2;
  struct cht_opts opts;
  int i;
  struct cht_entry entry;
  char buf[CHT_BLOCK_SIZE];
  
  memset( &opts, 0, sizeof(opts) );
  opts.mask = CHT_OPT_COUNT;
  opts.count = newcount;
  sts = cht_open( outpath, &cht2, &opts );
  if( sts ) usage( "Failed to open new file" );

  /* ensure new hashtable is clear */
  cht_purge( &cht2, 0, 0 );

  /* copy old entries into new table */
  for( i = 0; i < cht->count; i++ ) {
    sts = cht_entry_by_index( cht, i, 0, &entry );
    if( sts ) continue;

    sts = cht_read( cht, (char *)entry.key, buf, sizeof(buf), &entry );
    if( sts ) continue;

    cht_write( &cht2, &entry, buf, entry.flags & CHT_SIZE_MASK );
  }
  
  cht_close( &cht2 );
}
