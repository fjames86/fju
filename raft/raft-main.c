
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
            "          add cluster [currentterm=CURRENTTERM] [votedfor=VOTEDFOR] ]\n"
            "          set cluster ID [currentterm=CURRENTTERM] [votedfor=VOTEDFOR] ]\n"
            "          rem cluster ID\n"
            "          add member [clid=CLID] [hostid=HOSTID] [lastseen=LASTSEEN] [nextping=NEXTPING] [nextidx=NEXTIDX] [matchidx=MATCHIDX] [flags=FLAGS] ]\n"
            "          set member ID [clid=CLID] [hostid=HOSTID] [lastseen=LASTSEEN] [nextping=NEXTPING] [nextidx=NEXTIDX] [matchidx=MATCHIDX] [flags=FLAGS] ]\n"
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
                 if( strcmp( argname, "currentterm" ) == 0 ) {
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
        } else usage( NULL );
    } else usage( NULL );

    raft_close();
    return 0;
}

static void cmd_list( void ) {
    int sts, i, n, m;
    {
        struct raft_cluster *lst;
        n = raft_cluster_list( NULL, 0 );
        lst = (struct raft_cluster *)malloc( sizeof(*lst) * n );
        m = raft_cluster_list( lst, n );
        if( m < n ) n = m;
        for( i = 0; i < n; i++ ) {
            printf( "%-16s %-16"PRIx64" currentterm=%"PRIx64" votedfor=%"PRIx64" \n", "cluster", lst[i].clid, lst[i].currentterm, lst[i].votedfor );
        }
        free( lst );
        printf( "\n" );
    }
    {
        struct raft_member *lst;
        n = raft_member_list( NULL, 0 );
        lst = (struct raft_member *)malloc( sizeof(*lst) * n );
        m = raft_member_list( lst, n );
        if( m < n ) n = m;
        for( i = 0; i < n; i++ ) {
	  printf( "%-16s %-16"PRIx64" clid=%"PRIx64" hostid=%"PRIx64" lastseen=%"PRIx64" nextping=%"PRIx64" nextidx=%"PRIx64" matchidx=%"PRIx64" flags=%d \n", "member", lst[i].memberid, lst[i].clid, lst[i].hostid, lst[i].lastseen, lst[i].nextping, lst[i].nextidx, lst[i].matchidx, lst[i].flags );
        }
        free( lst );
        printf( "\n" );
    }
}

static void cmd_prop( void ) {
     struct raft_prop prop;
     raft_prop( &prop );
     printf( "seq=%"PRIu64"\n", prop.seq );
     printf( "cluster=%d/%d\n", prop.cluster_count, prop.cluster_max );
     printf( "member=%d/%d\n", prop.member_count, prop.member_max );
}

