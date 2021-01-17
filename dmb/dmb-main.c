
 
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

static void usage( char *fmt, ... ) {
    printf( "Usage:    \n"
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

int dmb_main( int argc, char **argv ) {
  int i;
  int sts;
  struct freg_entry entry;
  uint64_t hkey, hhosts, hostid, lastid, seq;
  char modname[FVM_MAX_NAME], procname[FVM_MAX_NAME];
  uint32_t category;

  sts = freg_open( NULL, NULL );
  i = 1;
  
  /* get toplevel handle */
  sts = freg_subkey( NULL, 0, "/fju/dmb", FREG_CREATE, &hkey );
  if( sts ) usage( "Failed to open /fju/dmb" );
  
  /* read hosts from reg */
  sts = freg_subkey( NULL, hkey, "hosts", FREG_CREATE, &hhosts );
  if( sts ) return sts;  
  sts = freg_next( NULL, hhosts, 0, &entry );
  while( !sts ) {
    if( ((entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY) ) {
      hostid = 0;
      lastid = 0;
      seq = 0;
      sts = freg_get_by_name( NULL, entry.id, "hostid", FREG_TYPE_UINT64, (char *)&hostid, sizeof(uint64_t), NULL );
      if( !sts ) {
	sts = freg_get_by_name( NULL, entry.id, "lastid", FREG_TYPE_UINT64, (char *)&lastid, sizeof(uint64_t), NULL );
	sts = freg_get_by_name( NULL, entry.id, "seq", FREG_TYPE_UINT64, (char *)&seq, sizeof(uint64_t), NULL );
	
	printf( "Host %"PRIx64" LastID %"PRIx64" Seq %"PRIu64"\n", hostid, lastid, seq );
      }
    }
    
    sts = freg_next( NULL, hhosts, entry.id, &entry );
  }
  
  /* read fvm subscribers */
  sts = freg_subkey( NULL, hkey, "fvm", FREG_CREATE, &hhosts );
  if( sts ) return sts;
  sts = freg_next( NULL, hhosts, 0, &entry );
  while( !sts ) {
    if( ((entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY) ) {
      strcpy( modname, "" );
      strcpy( procname, "" );
      category = 0;
      
      sts = freg_get_by_name( NULL, entry.id, "modname", FREG_TYPE_STRING, modname, FREG_MAX_NAME, NULL );
      if( !sts ) sts = freg_get_by_name( NULL, entry.id, "procname", FREG_TYPE_STRING, procname, FREG_MAX_NAME, NULL );
      if( !sts ) sts = freg_get_by_name( NULL, entry.id, "category", FREG_TYPE_UINT32, (char *)&category, sizeof(uint32_t), NULL );      
      if( !sts ) {
	printf( "FVM Subscriber %s/%s Category %u\n", modname, procname, category );
      }
    }
    
    sts = freg_next( NULL, hhosts, entry.id, &entry );
  }
  
  return 0;
}
