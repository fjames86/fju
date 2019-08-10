
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <time.h>

#include <fju/mmf.h>
#include <fju/ftab.h>

#ifdef WIN32
#define PRIu64 "llu"
#define PRIx64 "llx"
#endif

static struct {
  int cmd;
#define CMD_LIST 0
#define CMD_PROP 1
#define CMD_ALLOC 2
#define CMD_FREE 3
#define CMD_READ 4
#define CMD_WRITE 5
#define CMD_SET 6
#define CMD_RESET 7 
  char *path;
  struct ftab_s ftab;
  int binary;
  uint64_t id;
  uint32_t lbasize;
  uint32_t lbacount;
  uint32_t flags;
  uint64_t nextid;
  int setflags;
  int setnextid;
  uint32_t offset;
} glob;

static void usage( char *fmt, ... ) {
    printf( "Usage: [-p path] [-S lbasize] [-C lbacount] CMD args...\n"
	    "Where CMD:\n"
	    "               list\n"
	    "               prop\n"
	    "               reset\n" 
	    "               alloc\n"
	    "               free ID\n"
	    "               read ID\n"
	    "               write ID [offset=OFFSET]\n"
	    "               set ID [flags=FLAGS] [nextid=NEXT]\n" 
	    "\n"
    );

    if( fmt ) {
        va_list args;
        printf( "Error: " );
        va_start( args, fmt );
        vprintf( fmt, args );
        va_end( args );
        printf( "\n" );
    }
    exit( 0 );
}

static void argval_split( char *instr, char *argname, char **argval ) {
    char *p;

    p = strchr( instr, '=' );
    if( p ) *argval = p + 1;
    else *argval = NULL;

    p = instr;
    while( *p != '0' && *p != '=' ) {
        *argname = *p;
        p++;
        argname++;
    }
    *argname = '\0';
}

static void cmd_list( void );
static void cmd_prop( void );
static void cmd_read( void );
static void cmd_write( void );

int main( int argc, char **argv ) {
  int sts, i;
  struct ftab_opts opts;
  char argname[64], *argval;
      
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.path = argv[i];
    } else if( strcmp( argv[i], "-C" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.lbacount = strtoul( argv[i], NULL, 10 );
    } else if( strcmp( argv[i], "-S" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.lbasize = strtoul( argv[i], NULL, 10 );
    } else break;
    i++;
  }
  
  if( i >= argc ) {
    glob.cmd = CMD_LIST;
  } else if( strcmp( argv[i], "list" ) == 0 ) {
    glob.cmd = CMD_LIST;
  } else if( strcmp( argv[i], "prop" ) == 0 ) {
    glob.cmd = CMD_PROP;
  } else if( strcmp( argv[i], "alloc" ) == 0 ) {
    glob.cmd = CMD_ALLOC;
    i++;
    if( i < argc ) {
	glob.id = strtoull( argv[i], NULL, 16 );
    }
  } else if( strcmp( argv[i], "free" ) == 0 ) {
    glob.cmd = CMD_FREE;
    i++;
    if( i >= argc ) usage( NULL );
    glob.id = strtoull( argv[i], NULL, 16 );
  } else if( strcmp( argv[i], "read" ) == 0 ) {
    glob.cmd = CMD_READ;
    i++;
    if( i >= argc ) usage( NULL );
    glob.id = strtoull( argv[i], NULL, 16 );
    i++;
    while( i < argc ) {
      if( strcmp( argv[i], "-b" ) == 0 ) {
	glob.binary = 1;
      } else usage( NULL );
      i++;
    }
  } else if( strcmp( argv[i], "write" ) == 0 ) {      
    glob.cmd = CMD_WRITE;
    i++;
    if( i >= argc ) usage( NULL );
    glob.id = strtoull( argv[i], NULL, 16 );
    i++;
    while( i < argc ) {
	argval_split( arggv[i], argname, &argval );
	if( strcmp( argname, "offset" ) == 0 ) {
	    glob.offset = strtoul( argval, NULL, 10 );
	} else usage( NULL );
	i++;
    }
  } else if( strcmp( argv[i], "set" ) == 0 ) {
      glob.cmd = CMD_SET;      
      i++;
      if( i >= argc ) usage( NULL );
      glob.id = strtoull( argv[i], NULL, 16 );
      i++;
      
      while( i < argc ) {
	  argval_split( argv[i], argname, &argval );
	  if( strcmp( argname, "flags" ) == 0 ) {
	      glob.flags = strtoul( argval, NULL, 16 );
	      glob.setflags = 1;
	  } else if( strcmp( argname, "nextid" ) == 0 ) {
	      glob.nextid = strtoull( argval, NULL, 16 );
	      glob.setnextid = 1;
	  } else usage( NULL );
	  i++;
      }
  } else if( strcmp( argv[i], "reset" ) == 0 ) {
      glob.cmd = CMD_RESET;
  } else usage( NULL );

  if( !glob.path ) glob.path = "ftab.dat";

  memset( &opts, 0, sizeof(opts) );
  if( glob.lbasize ) {
      opts.mask |= FTAB_OPT_LBASIZE;
      opts.lbasize = glob.lbasize;
  }
  if( glob.lbacount ) {
      opts.mask |= FTAB_OPT_LBACOUNT;
      opts.lbacount = glob.lbacount;
  }
  
  sts = ftab_open( glob.path, &opts, &glob.ftab );
  if( sts ) usage( "Failed to open \"%s\"", glob.path );
  
  switch( glob.cmd ) {
  case CMD_LIST:
    cmd_list();
    break;
  case CMD_PROP:
    cmd_prop();
    break;
  case CMD_ALLOC:
      if( glob.id ) {
	  sts = ftab_acquire( &glob.ftab, glob.id );
      } else {
	  sts = ftab_alloc( &glob.ftab, &glob.id );
      }
      if( sts ) usage( "Alloc failed" );
      printf( "%"PRIx64"\n", glob.id );
      break;
  case CMD_FREE:
    sts = ftab_free( &glob.ftab, glob.id );
    if( sts ) usage( "Failed to free" );
    break;
  case CMD_READ:
    cmd_read();
    break;
  case CMD_WRITE:
    cmd_write();
    break;
  case CMD_SET:
      if( glob.setflags ) {
	  sts = ftab_set_flags( &glob.ftab, glob.id, glob.flags, 0xffffffff );
	  if( sts ) usage( "Failed to set flags" );
      }
      if( glob.setnextid ) {
	  sts = ftab_set_nextid( &glob.ftab, glob.id, glob.nextid );
	  if( sts ) usage( "Failed to set nextid" );
      }
      break;
  case CMD_RESET:
      ftab_reset( &glob.ftab );
      break;
  default:
      break;
  }
  
  ftab_close( &glob.ftab );

  return 0;
}

static void cmd_list( void ) {
  int i, n, m;
  struct ftab_entry *e;

  n = ftab_list( &glob.ftab, NULL, 0 );
  e = malloc( sizeof(*e) * n );
  m = ftab_list( &glob.ftab, e, n );
  if( m < n ) n = m;

  printf( "%-12s %-8s %-8s %-8s %-8s %-12s\n", "id", "blk", "seq", "flags", "refcount", "nextid" );
  for( i = 0; i < n; i++ ) {
    printf( "%-12"PRIx64" %-8u %-8u 0x%04x   %-8u %-12"PRIx64"\n",
	    e[i].id, e[i].blkidx, e[i].seq, e[i].flags, e[i].refcount, e[i].nextid );
  }

  free( e );  
}

static void cmd_prop( void ) {
  struct ftab_prop prop;
  ftab_prop( &glob.ftab, &prop );
  printf( "Version %u Seq %"PRIu64" count %u/%u lbasize %u\n",
	  prop.version, prop.seq, prop.count, prop.max, prop.lbasize );
}
    

static void cmd_read( void ) {
  struct ftab_prop prop;
  char *buf;
  int sts, i;
  
  ftab_prop( &glob.ftab, &prop );
  buf = malloc( prop.lbasize );

  sts = ftab_read( &glob.ftab, glob.id, buf, prop.lbasize, 0 );
  if( sts < 0 ) usage( "Failed to read" );
  
  if( glob.binary ) {
#ifdef WIN32
    WriteFile( GetStdHandle( STD_OUTPUT_HANDLE ), buf, prop.lbasize, NULL, NULL );
#else
    write( STDOUT_FILENO, buf, prop.lbasize );
#endif
  } else {
    printf( "  0000  " );
    for( i = 0; i < prop.lbasize; i++ ) {
      if( ((i % 8) == 0) && ((i % 16) != 0) ) printf( "  " );
      if( i && ((i % 16) == 0) ) printf( "\n  %04o  ", i );
      
      printf( "%02x ", (uint8_t)buf[i] );
    }
    printf( "\n" );
  }

}

static void cmd_write( void ) {
  char *buf;
  int offset = 0;
  int sts;
  struct ftab_prop prop;

  ftab_prop( &glob.ftab, &prop );
  
  buf = malloc( prop.lbasize );
  memset( buf, 0, prop.lbasize );
  
  offset = 0;
  do {
#ifdef WIN32
    {
      int b;
      b = ReadFile( GetStdHandle( STD_INPUT_HANDLE ), buf + offset, prop.lbasize - offset, (DWORD *)&sts, NULL );
      if( !b ) sts = -1;
    }
#else
    sts = read( STDIN_FILENO, buf + offset, prop.lbasize - offset );
#endif
    if( sts <= 0 ) break;
    offset += sts;    
  } while( offset < prop.lbasize ); 

  sts = ftab_write( &glob.ftab, glob.id, buf, offset, glob.offset );
  if( sts < 0 ) usage( "Failed to write" );
  free( buf );
}
