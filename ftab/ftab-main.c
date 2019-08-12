
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

#include "ftab-private.h"

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
#define CMD_COOKIE 8 
  char *path;
  struct ftab_s ftab;
  int binary;
  uint64_t id;
  uint32_t lbasize;
  uint32_t lbacount;
  uint32_t offset;
  uint32_t npop;
  char cookie[FTAB_MAX_COOKIE];
} glob;

static void usage( char *fmt, ... ) {
    printf( "Usage: [-p path] [-S lbasize] [-C lbacount] CMD args...\n"
	    "Where CMD:\n"
	    "               list\n"
	    "               prop\n"
	    "               reset\n" 
	    "               alloc [count=COUNT]\n"
	    "               free ID\n"
	    "               read ID\n"
	    "               write ID [offset=OFFSET]\n"
	    "               cookie ...\n" 
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
  int sts, i, j;
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
    while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "count" ) == 0 ) {
	glob.npop = strtoul( argval, NULL, 10 );
      } else usage( NULL );
      i++;
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
	argval_split( argv[i], argname, &argval );
	if( strcmp( argname, "offset" ) == 0 ) {
	    glob.offset = strtoul( argval, NULL, 10 );
	} else usage( NULL );
	i++;
    }
  } else if( strcmp( argv[i], "reset" ) == 0 ) {
      glob.cmd = CMD_RESET;
  } else if( strcmp( argv[i], "cookie" ) == 0 ) {
    glob.cmd = CMD_COOKIE;
    i++;
    for( j = 0; j < FTAB_MAX_COOKIE; j++ ) {
      if( i >= argc ) break;
      glob.cookie[j] = strtoul( argv[i], NULL, 16 );
      i++;
    }
  } else usage( NULL );

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
    if( !glob.npop ) glob.npop = 1;
    for( i = 0; i < glob.npop; i++ ) {
      sts = ftab_alloc( &glob.ftab, &glob.id );
      if( sts ) usage( "Alloc failed" );
      printf( "%"PRIx64"\n", glob.id );
    }
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
  case CMD_RESET:
      ftab_reset( &glob.ftab );
      break;
  case CMD_COOKIE:
      ftab_set_cookie( &glob.ftab, glob.cookie );
      break;
  default:
      break;
  }
  
  ftab_close( &glob.ftab );

  return 0;
}

static void cmd_list( void ) {
  int i, n, m;
  uint64_t *id;
    
  n = ftab_list( &glob.ftab, NULL, 0 );
  id = malloc( sizeof(*id) * n );
  m = ftab_list( &glob.ftab, id, n );
  if( m < n ) n = m;

  printf( "%-4s %-12s %-8s %-8s\n", "idx", "id", "blk", "seq" );
  for( i = 0; i < n; i++ ) {
    printf( "%-4u %-12"PRIx64" %-8u %-8u\n", i, id[i], (uint32_t)(id[i] & 0xffffffff), (uint32_t)(id[i] >> 32) );
  }

  free( id );
}

static void cmd_prop( void ) {
  struct ftab_prop prop;
  int i;
  ftab_prop( &glob.ftab, &prop );
  printf( "Version %u Seq %"PRIu64" count %u/%u lbasize %u\n",
	  prop.version, prop.seq, prop.count, prop.max, prop.lbasize );
  printf( "Cookie " );
  for( i = 0; i < FTAB_MAX_COOKIE; i++ ) {
    printf( "%02x ", (uint32_t)(uint8_t)prop.cookie[i] );
  }
  printf( "\n" );
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

