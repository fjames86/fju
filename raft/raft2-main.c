
#include <fju/raft2.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <fju/sec.h>
#include <fju/hostreg.h>

static void usage( char *fmt, ... ) {
    printf( "Usage:    prop                    Show properties\n"
	    "          add clid=*\n" 
	    "          set clid=* [member=*]   Set cluster\n"
	    "          rem clid=*              Remove cluster\n"  
	    "\n" );

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


int main( int argc, char **argv ) {
  int i, n, j, sts;  
  uint64_t clid[RAFT2_MAX_CLUSTER];
  struct raft2_cluster cl;
  
  sts = raft2_open();
  if( sts ) usage( "failed to open database" );

  hostreg_open();
  
  if( argc == 1 ) {  
    n = raft2_clid_list( clid, RAFT2_MAX_CLUSTER );
    for( i = 0; i < n; i++ ) {
      raft2_cluster_by_clid( clid[i], &cl );
      printf( "clid=%"PRIx64" ", cl.clid );
      for( j = 0; j < cl.nmember; j++ ) {
	printf( "member=%"PRIx64" ", cl.member[i].hostid );
      }
      printf( "\n" );
      printf( "    state=%s leader=%"PRIx64" term=%"PRIu64" seq=%"PRIu64"\n",
	      cl.state == RAFT2_STATE_FOLLOWER ? "Follower" :
	      cl.state == RAFT2_STATE_CANDIDATE ? "Candidate" :
	      cl.state == RAFT2_STATE_LEADER ? "Leader" :
	      "other",
	      cl.leaderid, cl.term, cl.seq
	      );
      
    }
    goto done;
  }

  if( strcmp( argv[1], "prop" ) == 0 ) {
    struct raft2_prop prop;
    raft2_prop( &prop );
    printf( "Seq=%"PRIu64" Count=%u/%u Elec=[%u, %u] Term=[%u, %u] RPCTimeout=%u\n",
	    prop.seq, prop.count, RAFT2_MAX_CLUSTER,
	    prop.elec_low, prop.elec_high,
	    prop.term_low, prop.term_high,
	    prop.rpc_timeout );
  } else if( strcmp( argv[1], "add" ) == 0 ) {
    struct raft2_cluster cl;
    char argname[64], *argval;
    
    i = 2;
    memset( &cl, 0, sizeof(cl) );
    sec_rand( &cl.clid, sizeof(cl.clid) );
    while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "clid" ) == 0 ) {
	cl.clid = strtoull( argval, NULL, 16 );
      } else usage( NULL );
      i++;
    }
    raft2_cluster_set( &cl );
  } else if( strcmp( argv[1], "set" ) == 0 ) {
    char argname[64], *argval;
    uint64_t clid = 0;
    struct raft2_cluster cl;

    i = 2;
    while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "clid" ) == 0 ) {
	clid = strtoull( argval, NULL, 16 );
	raft2_cluster_by_clid( clid, &cl );
      } else if( strcmp( argname, "member" ) == 0 ) {
	if( cl.nmember >= RAFT2_MAX_MEMBER ) usage( "Too many members" );
	clid = strtoull( argval, NULL, 16 );
	sts = hostreg_host_by_id( clid, NULL );
	if( sts ) usage( "Unknown hostid" );
	cl.member[cl.nmember].hostid = clid;
	cl.nmember++;
      } else usage( NULL );
      i++;
    }

    raft2_cluster_set( &cl );
  } else if( strcmp( argv[1], "rem" ) == 0 ) {
    uint64_t clid = 0;
    char argname[64], *argval;
    
    i = 2;
    while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "clid" ) == 0 ) {
	clid = strtoull( argval, NULL, 16 );
      } else usage( NULL );
      i++;
    }
    raft2_cluster_rem( clid );
    
  } else usage( NULL );

  
 done:
  raft2_close();
  
  return 0;
}
