
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <inttypes.h>

#include "log.h"

static struct {
  char *filepath;
  int cmd;
#define CMD_READ 1
#define CMD_WRITE 2
#define CMD_TAIL 3 
#define CMD_RESET 4
#define CMD_PROP 5
  struct log_s log;
  int flags;
} fju;

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

  printf( "fjlog [-p path] [-f | -w | -r]\n" 
	  "  Where:\n"
	  "     -p path          Use log file by path name.\n"
	  "     -f               Follow log.\n"
	  "     -w               Write entry, data from stdin.\n"
	  "     -r               Reset log\n" 
	  "     -u               Show properties\n" 
	  "\n" );
  exit( 0 );
}

static void cmd_read( uint64_t id, uint64_t *newid );
static void cmd_write( void );
static void cmd_tail( void );

int main( int argc, char **argv ) {
  int i;
  int sts;

  fju.cmd = CMD_READ;

  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      fju.filepath = argv[i];
    } else if( strcmp( argv[i], "-b" ) == 0 ) {
      fju.flags |= LOG_BINARY;
    } else if( strcmp( argv[i], "-w" ) == 0 ) {
      fju.cmd = CMD_WRITE;
    } else if( strcmp( argv[i], "-f" ) == 0 ) {
      fju.cmd = CMD_TAIL;
    } else if( strcmp( argv[i], "-r" ) == 0 ) {
      fju.cmd = CMD_RESET;
    } else if( strcmp( argv[i], "-u" ) == 0 ) {
      fju.cmd = CMD_PROP;
    } else {
      usage( NULL );
    }
    i++;
  }

  if( !fju.filepath ) fju.filepath = "fju.log";

  sts = log_open( fju.filepath, NULL, &fju.log );
  if( sts ) usage( "Failed to open" );

  switch( fju.cmd ) {
  case CMD_READ:
    cmd_read( 0, NULL );
    break;
  case CMD_WRITE:
    cmd_write();
    break;
  case CMD_TAIL:
    cmd_tail();
    break;
  case CMD_RESET:
    log_reset( &fju.log );
    break;
  case CMD_PROP:
    {
      struct log_prop prop;
      log_prop( &fju.log, &prop );
      printf( "Version %d Seq %"PRIu64" LBACount %u Start %u Count %u\n", 
	      prop.version, prop.seq, prop.lbacount, prop.start, prop.count );
    }
  }

  log_close( &fju.log );

  return 0;
}

static void cmd_read( uint64_t id, uint64_t *newid ) {
  int sts, n;
  struct log_entry entry;
  char *msg;
  time_t ut;
  struct tm *tm;
  char timestr[128];
  int msglen;

  memset( &entry, 0, sizeof(entry) );
  msglen = 1024;
  msg = malloc( msglen );
  do {
    entry.msglen = 0;
    sts = log_read( &fju.log, id, &entry, 1, &n );
    if( sts ) break;
    if( n == 0 ) break;

    if( entry.msglen > msglen ) {
      msglen = entry.msglen + 1;
      msg = realloc( msg, msglen );
    }
    
    entry.msg = msg;
    entry.msglen = msglen;
    sts = log_read( &fju.log, id, &entry, 1, &n );
    if( sts ) break;
    if( n == 0 ) break;

    ut = (time_t)entry.timestamp;
    tm = localtime( (time_t *)&ut );
    strftime( timestr, sizeof(timestr), "%Y-%m-%d %H:%M:%S", tm );

    if( entry.flags & LOG_BINARY ) {
      int i;
      printf( "%s %u:%x\n  0000  ", timestr, entry.pid, entry.flags & LOG_LVL_MASK );
      for( i = 0; i < entry.msglen; i++ ) {
	if( ((i % 8) == 0) && ((i % 16) != 0) ) printf( "  " );
	if( i && ((i % 16) == 0) ) printf( "\n  %04o  ", i + 1 );

	printf( "%02x ", (uint8_t)entry.msg[i] );
      }
      printf( "\n" );
    } else {
      printf( "%s %u:%x %s\n", timestr, entry.pid, entry.flags & LOG_LVL_MASK, entry.msg );
    }
    id = entry.id;
  } while( 1 );
  
  if( newid ) *newid = id;

  free( msg );
}

static void cmd_write( void ) {
  char buf[1024];
  int offset = 0;
  int sts;
  struct log_entry entry;

  do {
    sts = read( STDIN_FILENO, buf + offset, sizeof(buf) - offset );
    if( sts <= 0 ) break;
    offset += sts;
  } while( offset < sizeof(buf) );

  memset( &entry, 0, sizeof(entry) );
  entry.msglen = offset;
  entry.msg = buf;
  entry.flags = fju.flags | LOG_LVL_INFO;
  sts = log_write( &fju.log, &entry );
  if( sts ) usage( "Failed to write entry" );
}

static void cmd_tail( void ) {
  int sts;
  struct log_prop prop;
  struct log_entry entry;
  char msg[256];
  uint64_t seq;
  uint64_t id, newid;

  seq = 0;
  id = 0;
  do {
    sts = log_prop( &fju.log, &prop );
    if( sts ) break;
    if( prop.seq != seq ) {
      /* read all new messages */
      cmd_read( id, &newid );
      id = newid;

      seq = prop.seq;
    }

    sleep( 1 );
  } while( 1 );
  
}
