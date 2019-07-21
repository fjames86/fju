
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
		      if( argval ) entry.id = strtoull( argval, NULL, 16 );
                 } else {
		     printf( "Unknown field name %s\n", argname ); usage( NULL );
		 }
                 i++;
            }
	    if( !entry.id ) sec_rand( &entry.id, sizeof(entry.id) );
            sts = raft_cluster_add( &entry );
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
            sts = raft_member_add( &entry );
            if( sts ) usage( "Failed to add member" );
            printf( "Added member ID=%"PRIx64"\n", entry.id );
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
                 } else {
		     printf( "Unknown field name %s\n", argname ); usage( NULL );
		 }
                 i++;
            }
            sts = raft_member_set( &entry );
            if( sts ) usage( "Failed to set member" );
        } else usage( NULL );
    } else usage( NULL );

    raft_close();
    return 0;
}

static void cmd_list( void ) {
  int sts, i, n, m, j;
  struct raft_cluster *cluster;
  struct raft_member *member;
  char timestr[64];
  struct tm *tm;
  time_t now;
  char strflags[128];
  
  n = raft_cluster_list( NULL, 0 );
  cluster = (struct raft_cluster *)malloc( sizeof(*cluster) * n );

  member = (struct raft_member *)malloc( sizeof(*member) * 256 );
  
  m = raft_cluster_list( cluster, n );
  if( m < n ) n = m;
  for( i = 0; i < n; i++ ) {
      printf( "cluster id=%"PRIx64"\n", cluster[i].id );
    
      /* print all members of this cluster */
      m = raft_member_list( cluster[i].id, member, 256 );
      for( j = 0; j < m; j++ ) {
	  printf( "    member id=%"PRIx64" hostid=%"PRIx64"\n", 
		  member[j].id, member[j].hostid );
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
}

