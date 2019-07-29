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
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <time.h>
#include <mmf.h>
#include <sec.h>
#include "raft.h"
#include <rpc.h>
#include <hostreg.h>
#include <hrauth.h>

static void usage( char *fmt, ... ) {
    printf( "Usage:    prop\n"
	    "          reset\n" 
            "          add cluster [clid=ID] [typeid=TYPEID] [witness=true|false]\n"
            "          set cluster ID [typeid=TYPEID] [witness=true|false]\n"
            "          rem cluster ID\n"
            "          add member [clid=CLID] [hostid=HOSTID]\n"
            "          set member\n"
            "          rem member clid=CLID hostid=HOSTID\n"
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

static void cmd_list( void );
static void cmd_prop( void );
			   
int main( int argc, char **argv ) {
    int sts, i;

#ifdef WIN32
	{
		WSADATA wsadata;
		WSAStartup( MAKEWORD( 2, 2 ), &wsadata );
	}
#endif
    sts = raft_open();
    if( sts ) usage( "Failed to open" );

    i = 1;
    if( i >= argc ) {
        cmd_list();
    } else if( strcmp( argv[i], "list" ) == 0 ) {
        cmd_list();
    } else if( strcmp( argv[i], "prop" ) == 0 ) {
        cmd_prop();
    } else if( strcmp( argv[i], "reset" ) == 0 ) {
        raft_reset();
    } else if( strcmp( argv[i], "add" ) == 0 ) {
        i++;
        if( i >= argc ) usage( NULL );
        if( strcmp( argv[i], "cluster" ) == 0 ) {
            struct raft_cluster entry;
            char argname[64], *argval;
            memset( &entry, 0, sizeof(entry) );
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
		 if( strcmp( argname, "clid" ) == 0 ) {
		      if( argval ) entry.id = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "typeid" ) == 0 ) {
		   if( argval ) entry.typeid = strtoul( argval, NULL, 16 );
		 } else if( strcmp( argname, "witness" ) == 0 ) {
		   if( argval ) {
		     if( strcmp( argval, "true" ) == 0 ) entry.flags |= RAFT_CLUSTER_WITNESS;
		     else entry.flags &= ~RAFT_CLUSTER_WITNESS;
		   }		   
                 } else {
		     printf( "Unknown field name %s\n", argname ); usage( NULL );
		 }
                 i++;
            }
	    if( !entry.id ) sec_rand( &entry.id, sizeof(entry.id) );
            sts = raft_cluster_set( &entry );
            if( sts ) usage( "Failed to add cluster" );
            printf( "Added cluster ID=%"PRIx64"\n", entry.id );
        } else if( strcmp( argv[i], "member" ) == 0 ) {
            struct raft_member entry;
            char argname[64], *argval;
            memset( &entry, 0, sizeof(entry) );
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "clid" ) == 0 ) {
                      if( argval ) entry.clid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "hostid" ) == 0 ) {
                      if( argval ) entry.hostid = strtoull( argval, NULL, 16 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
            sts = raft_member_set( &entry );
            if( sts ) usage( "Failed to add member" );
            printf( "Added member ID=%"PRIx64"\n", entry.hostid );
        } else usage( NULL );
    } else if( strcmp( argv[i], "rem" ) == 0 ) {
        i++;
        if( i >= argc ) usage( NULL );
        if( strcmp( argv[i], "cluster" ) == 0 ) {
            uint64_t tag;
            i++;
            if( i >= argc ) usage( NULL );
            tag = strtoull( argv[i], NULL, 16 );
            sts = raft_cluster_rem( tag );
            if( sts ) usage( "Failed to rem cluster" );
        } else if( strcmp( argv[i], "member" ) == 0 ) {
	    char argname[64], *argval;
	    uint64_t clid = 0, hostid = 0;
            i++;
	    while( i < argc ) {
	        argval_split( argv[i], argname, &argval );
		if( strcmp( argname, "clid" ) == 0 ) {
		    if( argval ) clid = strtoull( argval, NULL, 16 );
		} else if( strcmp( argname, "hostid" ) == 0 ) {
		    if( argval ) hostid = strtoull( argval, NULL, 16 );
		} else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
		i++;
	    }
            sts = raft_member_rem( clid, hostid );
            if( sts ) usage( "Failed to rem member" );
        } else usage( NULL );
    } else if( strcmp( argv[i], "set" ) == 0 ) {
        uint64_t tag;
        i++;
        if( i >= argc ) usage( NULL );
        if( strcmp( argv[i], "cluster" ) == 0 ) {
	  char argname[64], *argval;
	  uint64_t clid;
	  struct raft_cluster cl;
	  
	  i++;
	  if( i >= argc ) usage( NULL );
	  clid = strtoull( argv[i], NULL, 16 );
	  sts = raft_cluster_by_id( clid, &cl );
	  i++;
	  while( i < argc ) {
	    argval_split( argv[i], argname, &argval );
	    if( strcmp( argname, "state" ) == 0 ) {
	      if( argval ) {
		if( strcmp( argval, "follower" ) == 0 ) cl.state = RAFT_STATE_FOLLOWER;
		else if( strcmp( argval, "candidate" ) == 0 ) cl.state = RAFT_STATE_CANDIDATE;
		else if( strcmp( argval, "leader" ) == 0 ) cl.state = RAFT_STATE_LEADER;
	      }
	    } else if( strcmp( argname, "typeid" ) == 0 ) {
	      if( argval ) cl.typeid = strtoul( argval, NULL, 16 );
	    } else if( strcmp( argname, "witness" ) == 0 ) {
	      if( argval ) {
		if( strcmp( argval, "true" ) == 0 ) cl.flags |= RAFT_CLUSTER_WITNESS;
		else cl.flags &= ~RAFT_CLUSTER_WITNESS;
	      }		   
	    } else usage( NULL );
	    i++;
	  }
	  raft_cluster_set( &cl );	  
	} else if( strcmp( argv[i], "member" ) == 0 ) {
	} else if( strcmp( argv[i], "prop" ) == 0 ) {
            char argname[64], *argval;
	    uint32_t elec_low, elec_high, term_low, term_high;
	    uint32_t rpc_timeout;
	      
	    elec_low = 0;
	    elec_high = 0;
	    term_low = 0;
	    term_high = 0;
	    rpc_timeout = 0;
	    
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "elec_low" ) == 0 ) {
                      if( argval ) elec_low = strtoul( argval, NULL, 10 );
		 } else if( strcmp( argname, "elec_high" ) == 0 ) {
                      if( argval ) elec_high = strtoul( argval, NULL, 10 );
		 } else if( strcmp( argname, "term_low" ) == 0 ) {
                      if( argval ) term_low = strtoul( argval, NULL, 10 );
		 } else if( strcmp( argname, "term_high" ) == 0 ) {
                      if( argval ) term_high = strtoul( argval, NULL, 10 );
		 } else if( strcmp( argname, "rpc" ) == 0 ) {
                      if( argval ) rpc_timeout = strtoul( argval, NULL, 10 );		      
                 } else {
		     printf( "Unknown field name %s\n", argname ); usage( NULL );
		 }
                 i++;
            }
	    raft_set_timeouts( elec_low ? &elec_low : NULL,
			       elec_high ? &elec_high : NULL,
			       term_low ? &term_low : NULL,
			       term_high ? &term_high : NULL );
	    if( rpc_timeout ) raft_set_rpc_timeout( rpc_timeout );
	    
        } else usage( NULL );
    } else usage( NULL );

    raft_close();
    return 0;
}

static void print_cluster( struct raft_cluster *cluster, struct raft_member *member, int nmember ) {
    struct tm *tm;
  time_t now;
  char strflags[128], timestr[128];
  int j;
  
      printf( "cluster id=%"PRIx64" state=%s termseq=%"PRIu64" leader=%"PRIx64" typeid=%x\n"
	      "        flags=0x%x votes=%u votedid=%"PRIx64" commitseq=%"PRIu64" stateseq=%"PRIu64"\n",
	      cluster->id,
	      cluster->state == RAFT_STATE_FOLLOWER ? "Follower" :
	      cluster->state == RAFT_STATE_CANDIDATE ? "Candidate" :
	      cluster->state == RAFT_STATE_LEADER ? "Leader" :
	      "Unknown",
	      cluster->termseq, cluster->leaderid, cluster->typeid,
	      cluster->flags, cluster->votes, cluster->voteid,
	      cluster->commitseq, cluster->stateseq );
	          
      for( j = 0; j < nmember; j++ ) {
	  if( member[j].lastseen ) {
	    now = (time_t)member[j].lastseen;
	    tm = localtime( &now );
	    strftime( timestr, sizeof(timestr), "%Y-%m-%d %H:%M:%S", tm );
	  } else {
	      strcpy( timestr, "Never" );
	  }

	  printf( "    member hostid=%"PRIx64" flags=%x lastseen=%s", 
		  member[j].hostid, member[j].flags, timestr );
	  if( cluster->state == RAFT_STATE_LEADER ) {
	    printf( "nextseq=%"PRIu64" stateseq=%"PRIu64"", member[j].nextseq, member[j].stateseq );
	  }
	  printf( "\n" );
    }  
}

static void cmd_list( void ) {
  int sts, i, n, m, j;
  struct raft_cluster *cluster;
  struct raft_member *member;
  struct tm *tm;
  time_t now;
  char strflags[128], timestr[128];
  
  n = raft_cluster_list( NULL, 0 );
  cluster = (struct raft_cluster *)malloc( sizeof(*cluster) * n );

  member = (struct raft_member *)malloc( sizeof(*member) * 256 );
  
  m = raft_cluster_list( cluster, n );
  if( m < n ) n = m;
  for( i = 0; i < n; i++ ) {
    m = raft_member_list( cluster[i].id, member, 256 );
    print_cluster( &cluster[i], member, m );
  }
  free( cluster );
  printf( "\n" );
}

static void cmd_prop( void ) {
     struct raft_prop prop;
     raft_prop( &prop );
     printf( "seq=%"PRIu64" rpc-timeout=%u\n", prop.seq, prop.rpc_timeout );
     printf( "Timeouts: election=[%d, %d] term=[%d, %d]\n", prop.elec_low, prop.elec_high, prop.term_low, prop.term_high );
     printf( "cluster=%d/%d\n", prop.cluster_count, prop.cluster_max );
     printf( "member=%d/%d\n", prop.member_count, prop.member_max );
}

