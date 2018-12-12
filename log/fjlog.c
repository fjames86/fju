/*
 * MIT License
 *
 * Copyright (c) 2018 Frank James
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

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
#define CMD_READ      1
#define CMD_WRITE     2
#define CMD_TAIL      3 
#define CMD_RESET     4
#define CMD_PROP      5
  struct log_s log;
  int flags;
  uint64_t start_id;
  int nmsgs;
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

  printf( "fjlog OPTIONS [-b] [-i id] [-n nmsgs]      Read entry.\n"
	  "              -w [-b]                      Write entry, data from stdin.\n" 
          "              -f                           Follow log.\n"
	  "              -r                           Reset log, clearing all messages.\n"
	  "              -u                           Show log properties.\n" 
	  "\n" 
	  "  OPTIONS\n"
	  "     -p path          Use log file by path name.\n"
	  "\n" 
	  "  Where:\n"
	  "     -b               Read/write binary.\n" 
	  "     -i id            Read starting from msg ID\n" 
	  "     -n nmsgs         Read no more than nmsgs\n" 
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
    } else if( strcmp( argv[i], "-i" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      fju.start_id = strtoull( argv[i], NULL, 16 );
    } else if( strcmp( argv[i], "-n" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      fju.nmsgs = strtoul( argv[i], NULL, 10 );
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
    cmd_read( fju.start_id, NULL );
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
    break;
  }

  log_close( &fju.log );

  return 0;
}

static char *lvlstr( int lvl ) {
  switch( lvl ) {
  case LOG_LVL_TRACE: return "TRACE";
  case LOG_LVL_DEBUG: return "DEBUG";
  case LOG_LVL_INFO: return "INFO ";
  case LOG_LVL_WARN: return "WARN ";
  case LOG_LVL_ERROR: return "ERROR";
  }
  return "ERROR";
}

static void cmd_read( uint64_t id, uint64_t *newid ) {
  int sts, n;
  struct log_entry entry;
  char *msg;
  time_t ut;
  struct tm *tm;
  char timestr[128];
  int msglen;
  int nmsgs;

  nmsgs = 0;
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

    if( fju.flags & LOG_BINARY ) {
      write( STDOUT_FILENO, entry.msg, entry.msglen );
    } else {
      ut = (time_t)entry.timestamp;
      tm = localtime( (time_t *)&ut );
      strftime( timestr, sizeof(timestr), "%Y-%m-%d %H:%M:%S", tm );
      
      if( entry.flags & LOG_BINARY ) {
	int i;
	printf( "%s %-4u:%s %"PRIx64"\n  0000  ", timestr, entry.pid, lvlstr( entry.flags & LOG_LVL_MASK ), entry.id );
	for( i = 0; i < entry.msglen; i++ ) {
	  if( ((i % 8) == 0) && ((i % 16) != 0) ) printf( "  " );
	  if( i && ((i % 16) == 0) ) printf( "\n  %04o  ", i );
	  
	  printf( "%02x ", (uint8_t)entry.msg[i] );
	}
	printf( "\n" );
      } else {
	printf( "%s %-4u:%s %"PRIx64" %s\n", timestr, entry.pid, lvlstr( entry.flags & LOG_LVL_MASK ), entry.id, entry.msg );
      }
    }
    id = entry.id;

    nmsgs++;
    if( fju.nmsgs && (nmsgs >= fju.nmsgs) ) break;
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
