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

static void usage( char *fmt, ... ) {
    printf( "Usage:    prop\n"
	    "          set prop [elec_low=*] [elec_high=*] [term_low=*] [term_high=*] [rpc_timeout=*] [snapth=*]\n" 
            "          add cluster [clid=ID] [appid=APPID] [witness=true|false]\n"
            "          set cluster ID [appid=APPID] [witness=true|false] [cookie=*]\n"
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
static void print_cluster( struct raft_cluster *cluster );

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

    hostreg_open();
    
    i = 1;
    if( i >= argc ) {
        cmd_list();
    } else if( strcmp( argv[i], "list" ) == 0 ) {
        cmd_list();
    } else if( strcmp( argv[i], "prop" ) == 0 ) {
        cmd_prop();
    } else if( strcmp( argv[i], "get" ) == 0 ) {
      uint64_t clid = 0;
      uint32_t appid = 0;
      struct raft_cluster cl;
      char argname[64], *argval;
      
      i++;
      while( i < argc ) {
	argval_split( argv[i], argname, &argval );
	if( strcmp( argname, "clid" ) == 0 ) {
	  clid = strtoull( argval, NULL, 16 );
	} else if( strcmp( argname, "appid" ) == 0 ) {
	  appid = strtoul( argval, NULL, 0 );
	} else usage( NULL );
	i++;
      }
      if( appid ) {
	clid = raft_clid_by_appid( appid );
	if( !clid ) usage( "No cluster with appid=%u", appid );
      }
      sts = raft_cluster_by_clid( clid, &cl );
      if( sts ) usage( "Failed to find cluster" );
      print_cluster( &cl );
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
		      if( argval ) entry.clid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "appid" ) == 0 ) {
		   if( argval ) entry.appid = strtoul( argval, NULL, 0 );
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
	    if( !entry.clid ) sec_rand( &entry.clid, sizeof(entry.clid) );
            sts = raft_cluster_set( &entry );
            if( sts ) usage( "Failed to add cluster" );
            printf( "Added cluster ID=%"PRIx64"\n", entry.clid );
        } else if( strcmp( argv[i], "member" ) == 0 ) {
	    struct raft_cluster cl;
	    uint64_t clid = 0, hostid = 0;
            char argname[64], *argval;
            memset( &cl, 0, sizeof(cl) );
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "clid" ) == 0 ) {
                      if( argval ) clid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "hostid" ) == 0 ) {
		   if( argval ) {
		     char *term;
		     hostid = strtoull( argval, &term, 16 );
		     if( *term ) {
		       hostid = hostreg_hostid_by_name( argval );
		       if( !hostid ) usage( "Unknown host \"%s\"", argval );
		     } else {
		       if( hostreg_host_by_id( hostid, NULL ) ) usage( "Unknown hostid" );
		     }
		   }
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
	    sts = raft_cluster_by_clid( clid, &cl );
	    if( sts ) usage( "Unknown cluster" );
	    if( cl.nmember >= RAFT_MAX_MEMBER ) usage( "No more members" );
	    i = cl.nmember;
	    memset( &cl.member[i], 0, sizeof(cl.member[i]) );
	    cl.member[i].hostid = hostid;
	    cl.nmember++;
	    raft_cluster_set( &cl );
            printf( "Added member ID=%"PRIx64"\n", hostid );
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
	    struct raft_cluster cl;
            i++;
	    while( i < argc ) {
	        argval_split( argv[i], argname, &argval );
		if( strcmp( argname, "clid" ) == 0 ) {
		    if( argval ) clid = strtoull( argval, NULL, 16 );
		} else if( strcmp( argname, "hostid" ) == 0 ) {
		  if( argval ) {
		    char *term;
		    hostid = strtoull( argval, &term, 16 );
		    if( *term ) hostid = hostreg_hostid_by_name( argval );
		  }
		} else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
		i++;
	    }
	    sts = raft_cluster_by_clid( clid, &cl );
	    if( sts ) usage( "Unknown cluster" );
	    for( i = 0; i < cl.nmember; i++ ) {
	      if( cl.member[i].hostid == hostid ) {
		if( i != (cl.nmember - 1) ) cl.member[i] = cl.member[cl.nmember - 1];
		cl.nmember--;
		raft_cluster_set( &cl );
		break;
	      }
	    }
        } else usage( NULL );
    } else if( strcmp( argv[i], "set" ) == 0 ) {
        i++;
        if( i >= argc ) usage( NULL );
        if( strcmp( argv[i], "cluster" ) == 0 ) {
	  char argname[64], *argval;
	  uint64_t clid;
	  struct raft_cluster cl;
	  
	  i++;
	  if( i >= argc ) usage( NULL );
	  clid = strtoull( argv[i], NULL, 16 );
	  sts = raft_cluster_by_clid( clid, &cl );
	  if( sts ) usage( "Unknown cluster" );
	  i++;
	  while( i < argc ) {
	    argval_split( argv[i], argname, &argval );
	    if( strcmp( argname, "state" ) == 0 ) {
	      if( argval ) {
		if( strcmp( argval, "follower" ) == 0 ) cl.state = RAFT_STATE_FOLLOWER;
		else if( strcmp( argval, "candidate" ) == 0 ) cl.state = RAFT_STATE_CANDIDATE;
		else if( strcmp( argval, "leader" ) == 0 ) cl.state = RAFT_STATE_LEADER;
	      }
	    } else if( strcmp( argname, "appid" ) == 0 ) {
	      if( argval ) cl.appid = strtoul( argval, NULL, 0 );
	    } else if( strcmp( argname, "witness" ) == 0 ) {
	      if( argval ) {
		if( strcmp( argval, "true" ) == 0 ) cl.flags |= RAFT_CLUSTER_WITNESS;
		else cl.flags &= ~RAFT_CLUSTER_WITNESS;
	      }
	    } else if( strcmp( argname, "cookie" ) == 0 ) {
	      if( argval ) {
		strncpy( cl.cookie, argval, sizeof(cl.cookie) - 1 );
	      }
	    } else usage( NULL );
	    i++;
	  }
	  raft_cluster_set( &cl );	  
	} else if( strcmp( argv[i], "member" ) == 0 ) {
	} else if( strcmp( argv[i], "prop" ) == 0 ) {
            char argname[64], *argval;
	    struct raft_prop prop;
	    uint32_t mask = 0;
	    
	    raft_prop( &prop );
	    
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "elec_low" ) == 0 ) {
		   if( argval ) {
		     prop.elec_low = strtoul( argval, NULL, 10 );
		     mask |= RAFT_PROP_ELEC_LOW;
		   }
		 } else if( strcmp( argname, "elec_high" ) == 0 ) {
		   if( argval ) {
		     prop.elec_high = strtoul( argval, NULL, 10 );
		     mask |= RAFT_PROP_ELEC_HIGH;
		   }
		 } else if( strcmp( argname, "term_low" ) == 0 ) {
		   if( argval ) {
		     prop.term_low = strtoul( argval, NULL, 10 );
		     mask |= RAFT_PROP_TERM_LOW;
		   }
		 } else if( strcmp( argname, "term_high" ) == 0 ) {
		   if( argval ) {
		     prop.term_high = strtoul( argval, NULL, 10 );
		     mask |= RAFT_PROP_TERM_HIGH;
		   }
		 } else if( strcmp( argname, "rpc_timeout" ) == 0 ) {
		   if( argval ) {
		     prop.rpc_timeout = strtoul( argval, NULL, 10 );
		     mask |= RAFT_PROP_RPC_TIMEOUT;
		   }
		 } else if( strcmp( argname, "snapth" ) == 0 ) {
		   if( argval ) {
		     prop.snapth = strtoul( argval, NULL, 10 );
		     if( prop.snapth > 100 ) prop.snapth = 100;
		     mask |= RAFT_PROP_SNAPTH;
		   }
                 } else {
		   printf( "Unknown field name %s\n", argname ); usage( NULL );
		 }
                 i++;
            }
	    raft_prop_set( mask, &prop );
	    
        } else usage( NULL );
    } else usage( NULL );

    raft_close();
    return 0;
}

static void print_cluster( struct raft_cluster *cluster ) {
  char timestr[128], namestr[HOSTREG_MAX_NAME];
  int j, sts;
  uint64_t cseq;
  struct log_prop prop;
  struct log_s log;
  
  raft_command_seq( cluster->clid, NULL, &cseq );
  
  printf( "Cluster ID=%"PRIx64" State=%s Term=%"PRIu64" Leader=%"PRIx64" (%s)\n"
	  "        AppliedSeq=%"PRIu64" CommitSeq=%"PRIu64" StoredSeq=%"PRIu64" AppID=%u Flags=0x%x (%s) VoteID=%"PRIx64"\n"
	  "        Cookie=%s\n", 
	  cluster->clid,
	  cluster->state == RAFT_STATE_FOLLOWER ? "Follower" :
	  cluster->state == RAFT_STATE_CANDIDATE ? "Candidate" :
	  cluster->state == RAFT_STATE_LEADER ? "Leader" :
	  "Unknown",
	  cluster->term,
	  cluster->leaderid, hostreg_name_by_hostid( cluster->leaderid, namestr ),
	  cluster->appliedseq, cluster->commitseq, cseq,
	  cluster->appid, cluster->flags, cluster->flags & RAFT_CLUSTER_WITNESS ? "Witness" : "", 
	  cluster->voteid,
	  cluster->cookie );

  sts = raft_log_open( cluster->clid, &log );
  if( !sts ) {
    log_prop( &log, &prop );
    printf( "    Command Log: %u/%u (%u%%)\n", prop.count, prop.lbacount, (100 * prop.count) / prop.lbacount );
    log_close( &log );
  }
  
  for( j = 0; j < cluster->nmember; j++ ) {
    if( cluster->member[j].lastseen ) {
      sec_timestr( cluster->member[j].lastseen, timestr );
    } else {
      strcpy( timestr, "Never" );
    }

    printf( "    Member HostID=%"PRIx64" (%s) Flags=%s (0x%x) LastSeen=%s ", 
	    cluster->member[j].hostid,
	    hostreg_name_by_hostid( cluster->member[j].hostid, namestr ),
	    cluster->member[j].flags & RAFT_MEMBER_VOTED ? "Voted," : "",
	    cluster->member[j].flags,
	    timestr );
    if( cluster->state == RAFT_STATE_LEADER ) {
      printf( "StoredSeq=%"PRIu64"", cluster->member[j].storedseq );
    }
    printf( "\n" );
  }

  {
    struct raft_snapshot_info info;
    int sts;
    sts = raft_snapshot_info( cluster->clid, &info, NULL );
    if( !sts ) {
      printf( "    Snapshot Size=%u Term=%"PRIu64" Seq=%"PRIu64"\n", info.size, info.term, info.seq );
    }
    sts = raft_snapshot_info( cluster->clid, NULL, &info );
    if( !sts ) {
      printf( "    Snapshot Partial Term=%"PRIu64" Seq=%"PRIu64" Complete=%s\n", info.term, info.seq, info.complete ? "True" : "False" );
    }
  }

#if 0
  {
    struct raft_command_info *clist;
    int n;
    n = raft_command_list( cluster->clid, NULL, 0 );
    if( n > 0 ) {
      clist = malloc( sizeof(*clist) * n );
      raft_command_list( cluster->clid, clist, n );
      for( j = 0; j < n; j++ ) {      
	printf( "    Command Seq=%"PRIu64" Term=%"PRIu64" Len=%u Stored=%s\n", clist[j].seq, clist[j].term, clist[j].len, sec_timestr( clist[j].stored, timestr ) );
      }
      free( clist );
    }
  }
  #endif

  printf( "\n" );
}

static void cmd_list( void ) {
  int i, n, m;
  struct raft_cluster *cluster;
  char hname[HOSTREG_MAX_NAME];
  
  n = raft_cluster_list( NULL, 0 );
  cluster = (struct raft_cluster *)malloc( sizeof(*cluster) * n );

  printf( "%-16s %-10s %-16s %-10s %-8s %-8s\n", "CLID", "State", "Leader", "AppID", "Term", "CommitSeq" );
  m = raft_cluster_list( cluster, n );
  if( m < n ) n = m;
  for( i = 0; i < n; i++ ) {
    strcpy( hname, "" );
    if( cluster[i].leaderid ) hostreg_name_by_hostid( cluster[i].leaderid, hname );
    
    printf( "%"PRIx64" %-10s %-16s %-10u %-8"PRIu64" %-8"PRIu64"\n",
	    cluster[i].clid,
	    cluster[i].state == RAFT_STATE_LEADER ? "Leader" :
	    cluster[i].state == RAFT_STATE_CANDIDATE ? "Candidate" :
	    cluster[i].state == RAFT_STATE_FOLLOWER ? "Follower" :
	    "Other",
	    hname,
	    cluster[i].appid,
	    cluster[i].term,
	    cluster[i].commitseq );
  }

  free( cluster );
  printf( "\n" );
}

static void cmd_prop( void ) {
     struct raft_prop prop;
     raft_prop( &prop );
     printf( "seq=%"PRIu64" rpc-timeout=%u\n", prop.seq, prop.rpc_timeout );
     printf( "Timeouts: election=[%d, %d] term=[%d, %d]\n",
	     prop.elec_low, prop.elec_high, prop.term_low, prop.term_high );
     printf( "Cluster=%u/%u\n", prop.count, RAFT_MAX_CLUSTER );
     printf( "Snapshot threshold: %u%%\n", prop.snapth );
}

