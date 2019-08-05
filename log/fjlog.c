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

#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <inttypes.h>

#include <fju/log.h>

#ifdef WIN32
#define strcasecmp _stricmp
#endif

uint64_t rpc_now( void ) {
#ifdef WIN32
  return GetTickCount();
#else
  struct timespec tm;
  clock_gettime( CLOCK_MONOTONIC, &tm );
  return (uint64_t)((tm.tv_sec * 1000ULL) + (tm.tv_nsec / (1000ULL * 1000ULL)));
#endif
}
static char *lvlstr( int lvl );

static struct {
  char *filepath;
  int cmd;
#define CMD_READ      1
#define CMD_WRITE     2
#define CMD_TAIL      3 
#define CMD_RESET     4
#define CMD_PROP      5
#define CMD_TEST      6 
  struct log_s log;
  int flags;
  uint64_t start_id;  
  int nmsgs;
  int read_reverse;
  int print_quiet;
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
	  "              -F                           Create a fixed (non-circular) log.\n"
	  "              -G                           Create a growing (non-circular) log.\n"
	  "              -s size                      Create with size\n"
	  "              -L lvl                       Set log lvl\n" 
	  "\n" 
	  "  OPTIONS\n"
	  "     -p path          Use log file by path name.\n"
	  "     -R               Read reverse.\n" 
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
static void cmd_test( void );

int main( int argc, char **argv ) {
  int i;
  int sts;
  int logsize = 2*1024*1024;
  struct log_opts opts;
  uint32_t logflags = 0;
  int setlvlflags = 0, lvlflags;
  
  memset( &opts, 0, sizeof(opts) );
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
    } else if( strcmp( argv[i], "-s" ) == 0 ) {
      char *terminator;
      i++;
      if( i >= argc ) usage( NULL );
      logsize = strtoul( argv[i], &terminator, 10 );
      switch( *terminator ) {
      case 'M':
      case 'm':
	logsize *= 1024;
      case 'k':
      case 'K':
	logsize *= 1024;	
      }
    } else if( strcmp( argv[i], "-R" ) == 0 ) {
      fju.read_reverse = 1;
    } else if( strcmp( argv[i], "-q" ) == 0 ) {
      fju.print_quiet = 1;
    } else if( strcmp( argv[i], "-t" ) == 0 ) {
      fju.cmd = CMD_TEST;
    } else if( strcmp( argv[i], "-F" ) == 0 ) {
      logflags |= LOG_FLAG_FIXED;
    } else if( strcmp( argv[i], "-G" ) == 0 ) {
      logflags |= LOG_FLAG_FIXED|LOG_FLAG_GROW;
    } else if( strcmp( argv[i], "-L" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );

      setlvlflags = 1;
      if( strcasecmp( argv[i], "trace" ) == 0 ) lvlflags = LOG_LVL_TRACE;
      else if( strcasecmp( argv[i], "debug" ) == 0 ) lvlflags = LOG_LVL_DEBUG;
      else if( strcasecmp( argv[i], "info" ) == 0 ) lvlflags = LOG_LVL_INFO;
      else if( strcasecmp( argv[i], "warn" ) == 0 ) lvlflags = LOG_LVL_WARN;
      else if( strcasecmp( argv[i], "error" ) == 0 ) lvlflags = LOG_LVL_ERROR;
      else if( strcasecmp( argv[i], "fatal" ) == 0 ) lvlflags = LOG_LVL_FATAL;
      else usage( NULL );
    } else {
      usage( NULL );
    }
    i++;
  }

  opts.mask = LOG_OPT_LBACOUNT;
  opts.mask |= LOG_OPT_FLAGS;
  opts.lbacount = logsize / LOG_LBASIZE;
  opts.flags = logflags;
  sts = log_open( fju.filepath, &opts, &fju.log );
  if( sts ) usage( "Failed to open" );

  if( setlvlflags ) log_set_lvl( &fju.log, lvlflags );
  
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
      printf( "Version %d Tag %"PRIx64" Seq %"PRIu64" LBACount %u (%uMB) Start %u Count %u LastID %"PRIx64" Flags 0x%04x MinLvl=%s\n", 
	      prop.version,
	      prop.tag, 
	      prop.seq,
	      prop.lbacount,
	      (prop.lbacount * LOG_LBASIZE) / (1024*1024),
	      prop.start,
	      prop.count,
	      prop.last_id,
	      prop.flags,
	      lvlstr( (prop.flags & LOG_FLAG_LVLMASK) >> 4 ) );
    }
    break;
  case CMD_TEST:
    cmd_test();
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
  case LOG_LVL_FATAL: return "FATAL";
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
  int msglen, nmsgs;
  struct log_iov iov[1];

  nmsgs = 0;
  memset( &entry, 0, sizeof(entry) );
  msglen = 1024;
  msg = malloc( msglen );
  do {
    entry.niov = 0;
    sts = fju.read_reverse ?
      log_read_end( &fju.log, id, &entry, 1, &n ) : 
      log_read( &fju.log, id, &entry, 1, &n );
    if( sts < 0 ) break;
    if( n == 0 ) break;

    if( entry.msglen > msglen ) {
      msglen = entry.msglen + 1;
      msg = realloc( msg, msglen );
    }
    
    entry.iov = iov;
    entry.iov[0].buf = msg;
    entry.iov[0].len = msglen;
    entry.niov = 1;
    sts = fju.read_reverse ?
      log_read_end( &fju.log, id, &entry, 1, &n ) : 
      log_read( &fju.log, id, &entry, 1, &n );
    if( sts < 0 ) break;
    if( n == 0 ) break;

    if( fju.flags & LOG_BINARY ) {
      if( !fju.print_quiet ) {
#ifdef WIN32
		  WriteFile( GetStdHandle( STD_OUTPUT_HANDLE ), msg, entry.msglen, NULL, NULL );
#else
		  write( STDOUT_FILENO, msg, entry.msglen );
#endif
	  }
    } else {
      ut = (time_t)entry.timestamp;
      tm = localtime( (time_t *)&ut );
      strftime( timestr, sizeof(timestr), "%Y-%m-%d %H:%M:%S", tm );
      
      if( entry.flags & LOG_BINARY ) {
	int i;
	printf( "%s %6u:%s %"PRIx64"\n", timestr, entry.pid, lvlstr( entry.flags & LOG_LVL_MASK ), entry.id );
	if( !fju.print_quiet ) {
	  printf( "  0000  " );
	  for( i = 0; i < entry.msglen; i++ ) {
	    if( ((i % 8) == 0) && ((i % 16) != 0) ) printf( "  " );
	    if( i && ((i % 16) == 0) ) printf( "\n  %04o  ", i );
	    
	    printf( "%02x ", (uint8_t)msg[i] );
	  }
	  printf( "\n" );
	}
      } else {
	if( !fju.print_quiet ) printf( "%s %6u:%s %"PRIx64" %s\n", timestr, entry.pid, lvlstr( entry.flags & LOG_LVL_MASK ), entry.id, msg );
      }
    }
    if( fju.read_reverse ) {
      id = entry.prev_id;
      if( id == 0 ) break;
    } else {
      id = entry.id;
    }
    
    nmsgs++;
    if( fju.nmsgs && (nmsgs >= fju.nmsgs) ) break;
  } while( 1 );
  
  if( newid ) *newid = id;

  free( msg );
}

static void cmd_write( void ) {
  char *buf;
  int msglen = 4*4096;
  int offset = 0;
  int sts;
  struct log_entry entry;
  struct log_iov iov[1];
  
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
  entry.iov = iov;
  entry.iov[0].len = offset;
  entry.iov[0].buf = buf;
  entry.niov = 1;
  entry.flags = fju.flags | LOG_LVL_INFO;
  sts = log_write( &fju.log, &entry );
  if( sts ) usage( "Failed to write entry" );
  free( buf );
}

static void cmd_tail( void ) {
  int sts;
  struct log_prop prop;
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

#ifdef WIN32
    Sleep( 1000 );
#else
    sleep( 1 );
#endif
  } while( 1 );
  
}

static void cmd_test( void ) {
  uint64_t start_time, end_time;
  int niters = 1000000;
  struct log_entry en;
  struct log_iov iov[1];
  char tmpbuf[32];
  int i;
  
  memset( tmpbuf, 12, 32 );
  iov[0].buf = tmpbuf;
  iov[0].len = 32;
  en.iov = iov;
  en.niov = 1;
  en.flags = LOG_BINARY;
  
  start_time = rpc_now();
  for( i = 0; i < niters; i++ ) {
    log_write( &fju.log, &en );
  }
  end_time = rpc_now();
  printf( "niters=%d total=%dms writes/s=%d  per-write=%lf ms\n",
	  niters,
	  (int)(end_time - start_time),
	  (int)((double)niters / ((double)(end_time - start_time) / 1000.0)),
	  ((double)(end_time - start_time)) / (double)niters );  
}
