
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
#include <fju/fdtab.h>

#ifdef WIN32
#define PRIu64 "llu"
#define PRIx64 "llx"
#endif

static struct {
  int cmd;
#define CMD_ALLOC 2
#define CMD_FREE 3
#define CMD_READ 4
#define CMD_WRITE 5
  char *path;
  struct fdtab_s ftab;
  int binary;
  uint64_t id;
  uint32_t lbasize;
  uint32_t lbacount;
  uint32_t offset;
  uint32_t npop;
  int size;
} glob;

static void usage( char *fmt, ... ) {
    printf( "Usage: [-p path] [-S lbasize] [-C lbacount] CMD args...\n"
	    "Where CMD:\n"
	    "               alloc [count=COUNT] [size=SIZE]\n"
	    "               free ID\n"
	    "               read ID\n"
	    "               write ID [offset=OFFSET]\n"
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
    glob.cmd = CMD_ALLOC;
  } else if( strcmp( argv[i], "alloc" ) == 0 ) {
    glob.cmd = CMD_ALLOC;
    i++;
    while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "count" ) == 0 ) {
	glob.npop = strtoul( argval, NULL, 10 );
      } else if( strcmp( argname, "size" ) == 0 ) {
	glob.size = strtoul( argval, NULL, 10 );
      } else if( strcmp( argname, "id" ) == 0 ) {
	glob.id = strtoull( argval, NULL, 16 );
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
  
  sts = fdtab_open( glob.path, &opts, &glob.ftab );
  if( sts ) usage( "Failed to open \"%s\"", glob.path );
  
  switch( glob.cmd ) {
  case CMD_ALLOC:
    if( glob.id ) {
      sts = fdtab_realloc( &glob.ftab, glob.id, glob.size );
      if( sts < 0 ) usage( "Failed to realloc" );
    } else {
      if( !glob.npop ) glob.npop = 1;
      for( i = 0; i < glob.npop; i++ ) {
	sts = fdtab_alloc( &glob.ftab, glob.size, &glob.id );
	if( sts ) usage( "Alloc failed" );
	printf( "%"PRIx64"\n", glob.id );
      }
    }
    break;
  case CMD_FREE:
    sts = fdtab_free( &glob.ftab, glob.id );
    if( sts ) usage( "Failed to free" );
    break;
  case CMD_READ:
    cmd_read();
    break;
  case CMD_WRITE:
    cmd_write();
    break;
  default:
      break;
  }
  
  fdtab_close( &glob.ftab );

  return 0;
}

static void cmd_read( void ) {
  char *buf;
  int sts, i, size;

  size = fdtab_size( &glob.ftab, glob.id );
  if( size < 0 ) usage( "Failed to get size" );
  buf = malloc( size );
  
  sts = fdtab_read( &glob.ftab, glob.id, buf, size, 0 );
  if( sts < 0 ) usage( "Failed to read" );

  if( glob.binary ) {
#ifdef WIN32
    WriteFile( GetStdHandle( STD_OUTPUT_HANDLE ), buf, size, NULL, NULL );
#else
    write( STDOUT_FILENO, buf, size );
#endif
  } else {
    printf( "  0000  " );
    for( i = 0; i < size; i++ ) {
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
  int sts, size;

  size = 32*1024;
  buf = malloc( size );
  memset( buf, 0, size );
  
  offset = 0;
  do {
#ifdef WIN32
    {
      int b;
      b = ReadFile( GetStdHandle( STD_INPUT_HANDLE ), buf + offset, size - offset, (DWORD *)&sts, NULL );
      if( !b ) sts = -1;
    }
#else
    sts = read( STDIN_FILENO, buf + offset, size - offset );
#endif
    if( sts <= 0 ) break;
    offset += sts;    
  } while( offset < size ); 

  sts = fdtab_write( &glob.ftab, glob.id, buf, offset, glob.offset );
  if( sts < 0 ) usage( "Failed to write" );
  free( buf );
}

