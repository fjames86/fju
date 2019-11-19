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

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "fvm -p program\n"
	  "     [-v] [-n nsteps] [-t timeout]\n"
	  "     [-i inlog-path] [-o outlog-path]\n"
	  "     [-w word] [-wa arg]* [-wr nres]\n" );
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
    uint16_t wordargs[32];
    int nargs;
    uint16_t wordres[32];
    int nres;
} glob;

int main( int argc, char **argv ) {
  char *path = NULL;
  struct mmf_s mmf;
  int sts;
  int i;
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      path = argv[i];
    } else if( strcmp( argv[i], "-v" ) == 0 ) {
      glob.verbose = 1;
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
	i++;
	if( i >= argc ) usage( NULL );
	if( glob.nargs >= 32 ) usage( "Max args = 32" );
	glob.wordargs[glob.nargs] = strtoul( argv[i], NULL, 10 );
	glob.nargs++;
    } else if( strcmp( argv[i], "-wr" ) == 0 ) {
	i++;
	if( i >= argc ) usage( NULL );
	glob.nres = strtoul( argv[i], NULL, 10 );
	if( glob.nres > 32 ) usage( "Max results = 32" );
    } else usage( NULL );
    i++;
  }
  if( path == NULL ) usage( NULL );
	
  sts = mmf_open( path, &mmf );
  if( sts ) usage( "Failed to open program" );
  sts = mmf_remap( &mmf, mmf.fsize );
  if( sts ) usage( "Failed to map program" );
  sts = fvm_load( &glob.fvm, mmf.file, mmf.fsize / 2 );
  if( sts ) usage( "Failed to load program\n" );
  if( glob.verbose ) glob.fvm.flags |= FVM_FLAG_VERBOSE;
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

  if( glob.word & 0x8000 ) {
      fvm_call_word( &glob.fvm, glob.word & ~0x8000, glob.wordargs, glob.nargs, glob.wordres, glob.nres );
      for( i = 0; i < glob.nres; i++ ) {
	  printf( "%04x\n", glob.wordres[i] );
      }
  }
  else if( glob.nsteps ) fvm_run_nsteps( &glob.fvm, glob.nsteps );
  else if( glob.timeout ) fvm_run_timeout( &glob.fvm, glob.timeout );
  else fvm_run( &glob.fvm );

  if( glob.verbose ) {
    printf( ";; R0 %x R1 %x R2 %x R3 %x R4 %x R5 %x R6 %x R7 %x PC %x\n",
	    glob.fvm.reg[0], glob.fvm.reg[1],
	    glob.fvm.reg[2], glob.fvm.reg[3], 
	    glob.fvm.reg[4], glob.fvm.reg[5],
	    glob.fvm.reg[6], glob.fvm.reg[7],
	    glob.fvm.reg[8] );
    printf( ";; TickCount %d\n", (int)glob.fvm.tickcount );
  }

  if( glob.inlog ) log_close( &glob.logs[0] );
  if( glob.outlog ) log_close( &glob.logs[1] );
  
  return 0;
}

