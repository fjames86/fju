
 
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
#include <fju/dlm.h>

static void usage( char *fmt, ... ) {
  printf( "Usage:\n" );

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

struct dlm_command {  
  uint64_t lockid;
  uint64_t hostid;
  uint64_t resid;
  uint32_t cmd;
#define DLM_CMD_RELEASE 0
#define DLM_CMD_LOCKEX  1
#define DLM_CMD_LOCKSH  2
#define DLM_CMD_RESERVED 3
#define DLM_CMD_RELEASEALL 4
};

static int dlm_decode_command( struct xdr_s *xdr, struct dlm_command *cmd ) {
  int sts;
  sts = xdr_decode_uint64( xdr, &cmd->lockid );
  if( !sts ) sts = xdr_decode_uint64( xdr, &cmd->hostid );
  if( !sts ) sts = xdr_decode_uint64( xdr, &cmd->resid );
  if( !sts ) sts = xdr_decode_uint32( xdr, &cmd->cmd );
  if( sts ) return -1;
  switch( cmd->cmd ) {
  case DLM_CMD_RELEASE:
  case DLM_CMD_RELEASEALL:
    break;
  case DLM_CMD_LOCKEX:
  case DLM_CMD_LOCKSH:
    break;
  case DLM_CMD_RESERVED:
    break;
  default:
    return -1;
  }
  if( sts ) return sts;
  
  return 0;
}

int dlm_main( int argc, char **argv ) {
  int i;
  uint64_t clid;
  int n, ncmd;
  struct fsm_command_info *clist;
  char cmdbuf[64];
  struct log_iov iov[2];
  uint64_t tt;
  struct xdr_s xdr;
  struct dlm_command cmd;
  int sts;
  char hostname[64];
  struct fsm_snapshot_info sinfo;
  
  raft_open();
  hostreg_open();
  
  clid = raft_clid_by_appid( DLM_RPC_PROG );
  if( !clid ) usage( "No DLM cluster found" );

  memset( &sinfo, 0, sizeof(sinfo) );
  sts = fsm_snapshot_load( clid, NULL, 0, &sinfo );
  
  ncmd = fsm_command_list( clid, NULL, 0 );
  clist = malloc( sizeof(*clist) * ncmd );
  n = fsm_command_list( clid, clist, ncmd );
  if( n < ncmd ) ncmd = n;

  printf( "DLM Commands:\n" );
  for( i = 0; i < ncmd; i++ ) {
    if( sinfo.seq && (clist[i].seq < sinfo.seq) ) continue;    
    if( sinfo.seq && (clist[i].seq == sinfo.seq) ) printf( "%-4"PRIu64" Snapshot\n", sinfo.seq );
    
    iov[0].buf = (char *)&tt;
    iov[0].len = sizeof(tt);
    iov[1].buf = cmdbuf;
    iov[1].len = sizeof(cmdbuf);
    sts = fsm_command_load( clid, clist[i].seq, iov, 2 );
    if( sts < 0 ) usage( "Failed to load command Seq=%"PRIu64"", clist[i].seq );
    sts -= sizeof(tt);

    printf( "%-4"PRIu64" ", clist[i].seq );
    xdr_init( &xdr, (uint8_t *)cmdbuf, sts );
    dlm_decode_command( &xdr, &cmd );
    switch( cmd.cmd ) {
    case DLM_CMD_RELEASE:
      printf( "%-16s LockID=%"PRIx64"\n", "Release", cmd.lockid );
      break;
    case DLM_CMD_LOCKEX:
      hostreg_name_by_hostid( cmd.hostid, hostname );
      printf( "%-16s LockID=%"PRIx64" ResID=%"PRIx64" Host=%-16s\n", "LockEX", cmd.lockid, cmd.resid, hostname );
      break;
    case DLM_CMD_LOCKSH:
      hostreg_name_by_hostid( cmd.hostid, hostname );
      printf( "%-16s LockID=%"PRIx64" ResID=%"PRIx64" Host=%-16s\n", "LockEX", cmd.lockid, cmd.resid, hostname );
      break;
    case DLM_CMD_RELEASEALL:
      hostreg_name_by_hostid( cmd.hostid, hostname );
      printf( "%-16s Host=%s\n", "ReleaseALL", hostname );
      break;
    }
  }
  
  

  
  return 0;
}

