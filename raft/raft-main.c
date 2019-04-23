
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <mmf.h>
#include <sec.h>
#include "raft.h"

static void usage( char *fmt, ... ) {
    printf( "Usage:    prop\n"
	    "          reset\n" 
            "          add cluster [clid=ID] [currentterm=CURRENTTERM] [votedfor=VOTEDFOR]\n"
            "          set cluster ID [currentterm=CURRENTTERM] [votedfor=VOTEDFOR]\n"
            "          rem cluster ID\n"
            "          add member [clid=CLID] [hostid=HOSTID] [lastseen=LASTSEEN] [nextping=NEXTPING] [nextidx=NEXTIDX] [matchidx=MATCHIDX] [flags=FLAGS]\n"
            "          set member ID [clid=CLID] [hostid=HOSTID] [lastseen=LASTSEEN] [nextping=NEXTPING] [nextidx=NEXTIDX] [matchidx=MATCHIDX] [flags=FLAGS]\n"
            "          rem member ID\n"
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
		      if( argval ) entry.clid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "currentterm" ) == 0 ) {
                      if( argval ) entry.currentterm = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "votedfor" ) == 0 ) {
    		      if( argval ) entry.votedfor = strtoull( argval, NULL, 16 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
	    if( !entry.clid ) sec_rand( &entry.clid, sizeof(entry.clid) );
            sts = raft_cluster_add( &entry );
            if( sts ) usage( "Failed to add cluster" );
            printf( "Added cluster ID=%"PRIx64"\n", entry.clid );

	    /* add local host as member */	    
	    raft_member_add_local( entry.clid );
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
                } else if( strcmp( argname, "lastseen" ) == 0 ) {
                      if( argval ) entry.lastseen = strtoull( argval, NULL, 16 );
                } else if( strcmp( argname, "nextping" ) == 0 ) {
                      if( argval ) entry.nextping = strtoull( argval, NULL, 16 );
                } else if( strcmp( argname, "nextidx" ) == 0 ) {
                      if( argval ) entry.nextidx = strtoull( argval, NULL, 16 );
                } else if( strcmp( argname, "matchidx" ) == 0 ) {
                      if( argval ) entry.matchidx = strtoull( argval, NULL, 16 );
                } else if( strcmp( argname, "flags" ) == 0 ) {
                      if( argval ) entry.flags = strtoul( argval, NULL, 10 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
            sts = raft_member_add( &entry );
            if( sts ) usage( "Failed to add member" );
            printf( "Added member ID=%"PRIx64"\n", entry.memberid );
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
            uint64_t tag;
            i++;
            if( i >= argc ) usage( NULL );
            tag = strtoull( argv[i], NULL, 16 );
            sts = raft_member_rem( tag );
            if( sts ) usage( "Failed to rem member" );
        } else usage( NULL );
    } else if( strcmp( argv[i], "set" ) == 0 ) {
        uint64_t tag;
        i++;
        if( i >= argc ) usage( NULL );
        if( strcmp( argv[i], "cluster" ) == 0 ) {
            struct raft_cluster entry;
            char argname[64], *argval;
            memset( &entry, 0, sizeof(entry) );
            i++;
            if( i >= argc ) usage( NULL );
            tag = strtoull( argv[i], NULL, 16 );
            sts = raft_cluster_by_id( tag, &entry );
            if( sts ) usage( "Failed to lookup" );
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "currentterm" ) == 0 ) {
                      if( argval ) entry.currentterm = strtoull( argval, NULL, 16 );
                } else if( strcmp( argname, "votedfor" ) == 0 ) {
                      if( argval ) entry.votedfor = strtoull( argval, NULL, 16 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
            sts = raft_cluster_set( &entry );
            if( sts ) usage( "Failed to set cluster" );
        } else if( strcmp( argv[i], "member" ) == 0 ) {
            struct raft_member entry;
            char argname[64], *argval;
            memset( &entry, 0, sizeof(entry) );
            i++;
            if( i >= argc ) usage( NULL );
            tag = strtoull( argv[i], NULL, 16 );
            sts = raft_member_by_id( tag, &entry );
            if( sts ) usage( "Failed to lookup" );
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "clid" ) == 0 ) {
                      if( argval ) entry.clid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "hostid" ) == 0 ) {
		   if( argval ) entry.hostid = strtoull( argval, NULL, 16 );		      
                } else if( strcmp( argname, "lastseen" ) == 0 ) {
                      if( argval ) entry.lastseen = strtoull( argval, NULL, 16 );
                } else if( strcmp( argname, "nextping" ) == 0 ) {
                      if( argval ) entry.nextping = strtoull( argval, NULL, 16 );
                } else if( strcmp( argname, "nextidx" ) == 0 ) {
                      if( argval ) entry.nextidx = strtoull( argval, NULL, 16 );
                } else if( strcmp( argname, "matchidx" ) == 0 ) {
                      if( argval ) entry.matchidx = strtoull( argval, NULL, 16 );
                } else if( strcmp( argname, "flags" ) == 0 ) {
                      if( argval ) entry.flags = strtoul( argval, NULL, 10 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
            sts = raft_member_set( &entry );
            if( sts ) usage( "Failed to set member" );
	} else if( strcmp( argv[i], "port") == 0 ) {
     	    int port;
	    i++;
	    if( i >= argc ) usage( NULL );
	    port = strtoul( argv[i], NULL, 10 );
	    raft_set_port( port );
        } else usage( NULL );
    } else usage( NULL );

    raft_close();
    return 0;
}

static void cmd_list( void ) {
  int sts, i, n, m, j;
  struct raft_cluster *cluster;
  struct raft_member member[RAFT_MAX_CLUSTER_MEMBER];
  
  n = raft_cluster_list( NULL, 0 );
  cluster = (struct raft_cluster *)malloc( sizeof(*cluster) * n );
  m = raft_cluster_list( cluster, n );
  if( m < n ) n = m;
  for( i = 0; i < n; i++ ) {
    printf( "cluster clid=%"PRIx64" currentterm=%"PRIx64" votedfor=%"PRIx64" \n", cluster[i].clid, cluster[i].currentterm, cluster[i].votedfor );
    
    /* print all members of this cluster */
    m = raft_member_list_by_clid( cluster[i].clid, member, RAFT_MAX_CLUSTER_MEMBER );
    for( j = 0; j < m; j++ ) {
      printf( "    member memberid=%"PRIx64" hostid=%"PRIx64" lastseen=%"PRIx64" nextping=%"PRIx64" nextidx=%"PRIx64" matchidx=%"PRIx64" flags=%d \n", 
	      member[j].memberid, member[j].hostid, member[j].lastseen, member[j].nextping, member[j].nextidx, member[j].matchidx, member[j].flags );
    }
  }
  free( cluster );
  printf( "\n" );
}

static void cmd_prop( void ) {
     struct raft_prop prop;
     raft_prop( &prop );
     printf( "seq=%"PRIu64"\n", prop.seq );
     printf( "cluster=%d/%d\n", prop.cluster_count, prop.cluster_max );
     printf( "member=%d/%d\n", prop.member_count, prop.member_max );
     printf( "port=%d\n", prop.port );
}

