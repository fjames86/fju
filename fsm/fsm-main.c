
 
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
#include <fju/sec.h>
#include <fju/raft.h>
#include <fju/rpc.h>
#include <fju/hostreg.h>
#include <fju/hrauth.h>
#include <fju/dmb.h>
#include <fju/freg.h>
#include <fju/fvm.h>
#include <fju/fsm.h>

static void usage( char *fmt, ... ) {
    printf( "Usage:    [-c id] [-d id]\n"
	    "          -c id          create state machine\n"
	    "          -d id          delete state machine\n"
	    "          -w             write command (from stdin) to state machine\n"
	    "          -T seq         truncate state machine log to this seq\n" 
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

int fsm_main( int argc, char **argv ) {
  int i, sts, j, ncmd;
  struct fsm_info fslist[64];
  uint64_t fsmid;
  int nfsm;
  struct fsm_snapshot_info info;
  char timestr[64];
  struct fsm_command_info *clist;
  
  sts = fsm_open();
  if( sts ) usage( "Failed to open" );

  fsmid = 0;
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-c" ) == 0 ) {
      uint64_t fsmid;
      i++;
      if( i >= argc ) usage( NULL );
      fsmid = 0;
      sts = fsm_create( argv[i], &fsmid );
      if( sts ) usage( "Failed to create" );
      printf( "Create %s %"PRIx64"\n", argv[i], fsmid );
    } else if( strcmp( argv[i], "-I" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      fsmid = strtoull( argv[i], NULL, 16 );
    } else if( strcmp( argv[i], "-d" ) == 0 ) {
      fsm_delete( fsmid );
    } else if( strcmp( argv[i], "-w" ) == 0 ) {
      uint64_t seq;
      char *buf;
      int len;
      struct log_iov iov;
      
      buf = malloc( 32*1024 );
      len = 32*1024;
      len = fju_readstdin( buf, len );

      iov.buf = buf;
      iov.len = len;
      sts = fsm_command_save( fsmid, &iov, 1, &seq );
      if( sts < 0 ) usage( "Failed to save command" );
      printf( "Seq %"PRIu64" Len %u\n", seq, len );
    } else if( strcmp( argv[i], "-T" ) == 0 ) {
      uint64_t seq;
      i++;
      if( i >= argc ) usage( NULL );
      seq = strtoull( argv[i], NULL, 10 );
      sts = fsm_command_truncate( fsmid, seq );
      if( sts ) usage( "Failed to truncate" );
    } else usage( NULL );
    i++;
  }
  
  nfsm = fsm_list( fslist, 64 );
  if( nfsm > 64 ) nfsm = 64;
  for( i = 0; i < nfsm; i++ ) {
    fsmid = fslist[i].fsmid;
    sts = fsm_snapshot_load( fsmid, NULL, 0, &info );
    printf( "Name %-16s ID %"PRIx64"\n", fslist[i].name, fsmid );
    if( !sts ) printf( "  Snapshot Seq=%"PRIu64" Len=%u\n", info.seq, info.len );
    printf( "  Log Count=%u/%u LogSeq=%"PRIu64"\n",
	    fslist[i].logprop.count, fslist[i].logprop.lbacount, fslist[i].logprop.seq );
    ncmd = fsm_command_list( fsmid, NULL, 0 );
    if( ncmd > 0 ) { 
      clist = malloc( sizeof(*clist) * ncmd );
      sts = fsm_command_list( fsmid, clist, ncmd );
      if( sts < ncmd ) ncmd = sts;
      for( j = 0; j < ncmd; j++ ) {
	printf( "    Command Seq=%"PRIu64" When=%s len=%u\n", clist[j].seq, sec_timestr( clist[j].when, timestr ), clist[j].len );
      }
      free( clist );
    }
  }
  
  return 0;
}
