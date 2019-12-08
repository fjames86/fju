/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
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
#include <Winsock2.h>
#include <Windows.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <fju/mmf.h>
#include <fju/fvm.h>
#include <fju/log.h>
#include <fju/rpc.h>
#include <fju/nls.h>
#include <fju/hostreg.h>

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "fvm program\n"
	  "     [-v] [-n nsteps] [-t timeout]\n"
	  "     [-i inlog-path] [-o outlog-path]\n"
	  "     [-w word] [-wa arg]* [-wr int|string]\n"
	  "\n"
	  "  Where:\n"
	  "     -v [-v]            Verbose mode\n"
	  "     -n nsteps          Limit runtime by number of clock ticks.\n"
	  "     -t timeout         Limit runtime by number of milliseconds.\n"
	  "     -i inlog -o outlog Set in/out log files.\n"
	  "     -w word            Start at a given word index\n"
	  "     -wa arg            Push arg onto stack (use with -w)\n"
	  "     -wr int|string     Pop result value from stack and print (use with -w)\n"
	  "\n" );
  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
   
  exit( 1 );
}

static struct {
  struct fvm_state fvm;
  int verbose;
  int nsteps;
  int timeout;
  char *inlog;
  char *outlog;
  struct log_s logs[2];
  int word;
  int wordargtype[32];
  uint16_t wordarg[32];
  int nargs;
  int wordrestype[32];
  char *wordargdata[32];
#define RES_INT 0
#define RES_STR 1
  uint16_t wordres[32];
  int nres;
} glob;

int main( int argc, char **argv ) {
  char *path = NULL;
  struct mmf_s mmf;
  int sts, i, len;
  uint64_t start, end;

#ifdef WIN32
  {
      WSADATA wsadata;
      WSAStartup( MAKEWORD( 2, 2 ), &wsadata );
  }
#endif

  nls_open();
  hostreg_open();
  
  i = 1;
  if( i >= argc ) usage( NULL );
  if( (strcmp( argv[i], "-h" ) == 0) ||
      (strcmp( argv[i], "--help" ) == 0) ) {
    usage( NULL );
  }
  
  path = argv[i];
  i++;
  while( i < argc ) {
    if( strcmp( argv[i], "-v" ) == 0 ) {
      glob.verbose++;
    } else if( strcmp( argv[i], "-n" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );      
      glob.nsteps = strtoul( argv[i], NULL, 10 );
    } else if( strcmp( argv[i], "-t" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.timeout = strtoul( argv[i], NULL, 10 );
    } else if( strcmp( argv[i], "-i" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.inlog = argv[i];
    } else if( strcmp( argv[i], "-o" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.outlog = argv[i];
    } else if( strcmp( argv[i], "-w" ) == 0 ) {
	i++;
	if( i >= argc ) usage( NULL );
	glob.word = strtoul( argv[i], NULL, 10 );
	glob.word &= 0x07ff;
	glob.word |= 0x8000;
    } else if( strcmp( argv[i], "-wa" ) == 0 ) {
        char *terminator;
	i++;
	if( i >= argc ) usage( NULL );
	if( glob.nargs >= 32 ) usage( "Max args = 32" );
	glob.wordargtype[i] = RES_INT;
	glob.wordarg[glob.nargs] = strtoul( argv[i], &terminator, 10 );
	if( *terminator ) {
	  glob.wordarg[glob.nargs] = strtoul( argv[i], &terminator, 16 );
	  if( *terminator ) {
	    /* can't parse as base-10 or base-16 int so assume string. */
	    glob.wordargtype[glob.nargs] = RES_STR;
	    glob.wordargdata[glob.nargs] = argv[i];
	  }
	}
	glob.nargs++;
    } else if( strcmp( argv[i], "-wr" ) == 0 ) {
	i++;
	if( i >= argc ) usage( NULL );
	if( glob.nres >= 32 ) usage( "Max results = 32" );
	if( strcmp( argv[i], "int" ) == 0 ) glob.wordrestype[glob.nres] = RES_INT;
	else if( strcmp( argv[i], "str" ) == 0 ) glob.wordrestype[glob.nres] = RES_STR;
	else usage( "Result type must be int or str" );
	glob.nres++;
    } else usage( NULL );
    i++;
  }
  if( path == NULL ) usage( NULL );
  
  sts = mmf_open2( path, &mmf, MMF_OPEN_EXISTING );
  if( sts ) usage( "Failed to open program file \"%s\"", path );
  sts = mmf_remap( &mmf, mmf.fsize );
  if( sts ) usage( "Failed to map program" );
  sts = fvm_load( &glob.fvm, mmf.file, mmf.fsize );
  if( sts ) usage( "Failed to load program\n" );
  if( glob.verbose > 1 ) glob.fvm.flags |= FVM_FLAG_VERBOSE;
  mmf_close( &mmf );

  if( glob.inlog ) {
    sts = log_open( glob.inlog, NULL, &glob.logs[0] );
    if( sts ) usage( "Failed to open inlog" );
    glob.fvm.inlog = &glob.logs[0];
  }
  if( glob.outlog ) {
    sts = log_open( glob.outlog, NULL, &glob.logs[1] );
    if( sts ) usage( "Failed to open outlog" );
    glob.fvm.outlog = &glob.logs[1];
  }

  start = rpc_now();
  if( glob.word & 0x8000 ) {
    for( i = 0; i < glob.nargs; i++ ) {
      switch( glob.wordargtype[i] ) {
      case RES_INT:
	break;
      case RES_STR:
	/* copy the string into program memory at bottom of stack, set arg to address */
	len = strlen( glob.wordargdata[i] ) + 1;
	memcpy( (char *)&glob.fvm.mem[glob.fvm.bos], glob.wordargdata[i], len );
	glob.wordarg[i] = glob.fvm.bos;
	glob.fvm.bos += (len / 2) + (len % 2);
	break;
      }
      
      fvm_call_word( &glob.fvm, glob.word & ~0x8000, glob.wordarg, glob.nargs, glob.wordres, glob.nres );
      for( i = 0; i < glob.nres; i++ ) {
	switch( glob.wordrestype[i] ) {
	case RES_INT:
	  printf( "%04x\n", glob.wordres[i] );
	  break;
	case RES_STR:
	  printf( "%s\n", (char *)&glob.fvm.mem[glob.wordres[i]] );
	  break;
	}
      }
    }
  }
  else if( glob.nsteps ) fvm_run_nsteps( &glob.fvm, glob.nsteps );
  else if( glob.timeout ) fvm_run_timeout( &glob.fvm, glob.timeout );
  else {
      do {
	  fvm_run( &glob.fvm );
	  if( glob.fvm.sleep_timeout ) {
	      uint64_t now;
	      now = rpc_now();
#ifdef WIN32
	      if( glob.fvm.sleep_timeout > now ) Sleep( glob.fvm.sleep_timeout - now );
#else
	      if( glob.fvm.sleep_timeout > now ) usleep( (glob.fvm.sleep_timeout - now) * 1000 );
#endif
	      glob.fvm.sleep_timeout = 0;
	      glob.fvm.flags |= FVM_FLAG_RUNNING;
	  }	  
      } while( glob.fvm.flags & FVM_FLAG_RUNNING );
      
      if( (glob.fvm.flags & FVM_FLAG_VERBOSE) && (!(glob.fvm.flags & FVM_FLAG_DONE)) && (!(glob.fvm.flags & FVM_FLAG_RUNNING)) ) {
	  printf( ";; Stopped running but not done\n" );
      }
  }
  end = rpc_now();
      
  if( glob.verbose ) {
      struct fvm_dirty *dirty;
      int nd;
    printf( ";; R0 %x R1 %x R2 %x R3 %x R4 %x R5 %x R6 %x R7 %x PC %x\n",
	    glob.fvm.reg[0], glob.fvm.reg[1],
	    glob.fvm.reg[2], glob.fvm.reg[3], 
	    glob.fvm.reg[4], glob.fvm.reg[5],
	    glob.fvm.reg[6], glob.fvm.reg[7],
	    glob.fvm.reg[8] );
    printf( ";; TickCount %d\n", (int)glob.fvm.tickcount );
    printf( ";; ElapsedTime %dms\n", (int)(end - start) );

    nd = fvm_dirty_regions( &glob.fvm, NULL, 0 );
    printf( "nd = %d\n", nd );
    dirty = malloc( sizeof(*dirty) * nd );
    fvm_dirty_regions( &glob.fvm, dirty, nd );
    for( i = 0; i < nd; i++ ) {
	printf( ";; Dirty 0x%04x - 0x%04x\n", (uint32_t)dirty[i].offset, (uint32_t)(dirty[i].offset + dirty[i].count) );
    }
	
  }

  
  if( glob.inlog ) log_close( &glob.logs[0] );
  if( glob.outlog ) log_close( &glob.logs[1] );
  
  return 0;
}

