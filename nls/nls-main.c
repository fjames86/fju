
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <mmf.h>
#include "nls.h"

static void usage( char *fmt, ... ) {
    printf( "Usage:    prop\n" 
            "          add share [name=NAME] [hshare=HSHARE] \n"
            "          set share HSHARE [name=NAME]\n"
            "          rem share HSHARE\n"
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

    sts = nls_open();
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
        if( strcmp( argv[i], "share" ) == 0 ) {
            struct nls_share entry;
            char argname[64], *argval;
            memset( &entry, 0, sizeof(entry) );
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "name" ) == 0 ) {
                      if( argval ) strncpy( entry.name, argval, sizeof(entry.name) );
                } else if( strcmp( argname, "hshare" ) == 0 ) {
                      if( argval ) entry.hshare = strtoull( argval, NULL, 16 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
            sts = nls_share_add( &entry );
            if( sts ) usage( "Failed to add share" );
            printf( "Added share HSHARE=%"PRIx64"\n", entry.hshare );
	} else if( strcmp( argv[i], "remote" ) == 0 ) {
            struct nls_remote entry;
            char argname[64], *argval;
            memset( &entry, 0, sizeof(entry) );
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "name" ) == 0 ) {
		     if( argval ) strncpy( entry.share.name, argval, sizeof(entry.share.name) );
		 } else if( strcmp( argname, "hshare" ) == 0 ) {
  		     if( argval ) entry.share.hshare = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "hostid" ) == 0 ) {
		     if( argval ) entry.hostid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "lastid" ) == 0 ) {
		     if( argval ) entry.lastid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "seq" ) == 0 ) {
		     if( argval ) entry.seq = strtoull( argval, NULL, 10 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
            sts = nls_remote_add( &entry );
            if( sts ) usage( "Failed to add remote" );
            printf( "Added remote HSHARE=%"PRIx64"\n", entry.share.hshare );
        } else usage( NULL );
    } else if( strcmp( argv[i], "rem" ) == 0 ) {
        i++;
        if( i >= argc ) usage( NULL );
        if( strcmp( argv[i], "share" ) == 0 ) {
            uint64_t tag;
            i++;
            if( i >= argc ) usage( NULL );
            tag = strtoull( argv[i], NULL, 16 );
            sts = nls_share_rem( tag );
            if( sts ) usage( "Failed to rem share" );
	} else if( strcmp( argv[i], "remote" ) == 0 ) {
            uint64_t tag;
            i++;
            if( i >= argc ) usage( NULL );
            tag = strtoull( argv[i], NULL, 16 );
            sts = nls_remote_rem( tag );
            if( sts ) usage( "Failed to rem remote" );
        } else usage( NULL );
    } else if( strcmp( argv[i], "set" ) == 0 ) {
        uint64_t tag;
        i++;
        if( i >= argc ) usage( NULL );
        if( strcmp( argv[i], "share" ) == 0 ) {
            struct nls_share entry;
            char argname[64], *argval;
            memset( &entry, 0, sizeof(entry) );
            i++;
            if( i >= argc ) usage( NULL );
            tag = strtoull( argv[i], NULL, 16 );
            sts = nls_share_by_hshare( tag, &entry );
            if( sts ) usage( "Failed to lookup" );
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "name" ) == 0 ) {
                      if( argval ) strncpy( entry.name, argval, sizeof(entry.name) );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
            sts = nls_share_set( &entry );
            if( sts ) usage( "Failed to set share" );
	} else if( strcmp( argv[i], "remote" ) == 0 ) {
            struct nls_remote entry;
            char argname[64], *argval;
            memset( &entry, 0, sizeof(entry) );
            i++;
            if( i >= argc ) usage( NULL );
            tag = strtoull( argv[i], NULL, 16 );
            sts = nls_remote_by_hshare( tag, &entry );
            if( sts ) usage( "Failed to lookup" );
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "name" ) == 0 ) {
                      if( argval ) strncpy( entry.share.name, argval, sizeof(entry.share.name) );
		 } else if( strcmp( argname, "hostid" ) == 0 ) {
                      if( argval ) entry.hostid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "lastid" ) == 0 ) {
                      if( argval ) entry.lastid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "seq" ) == 0 ) {
                      if( argval ) entry.seq = strtoull( argval, NULL, 10 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
            sts = nls_remote_set( &entry );
            if( sts ) usage( "Failed to set remote" );
        } else usage( NULL );
    } else if( strcmp( argv[i], "reset" ) == 0 ) {
      nls_reset();
    } else usage( NULL );

    nls_close();
    return 0;
}

static void cmd_list( void ) {
    int sts, i, n, m;
    {
        struct nls_share *lst;
        n = nls_share_list( NULL, 0 );
        lst = (struct nls_share *)malloc( sizeof(*lst) * n );
        m = nls_share_list( lst, n );
        if( m < n ) n = m;
        for( i = 0; i < n; i++ ) {
            printf( "%-16s %-8"PRIx64" name=%s\n", "share", lst[i].hshare, lst[i].name );
        }
        free( lst );
        printf( "\n" );
    }
    {
        struct nls_remote *lst;
        n = nls_remote_list( NULL, 0 );
        lst = (struct nls_remote *)malloc( sizeof(*lst) * n );
        m = nls_remote_list( lst, n );
        if( m < n ) n = m;
        for( i = 0; i < n; i++ ) {
	  printf( "%-16s %-8"PRIx64" name=%s hostid=%"PRIx64" seq=%"PRIu64" lastid=%"PRIx64"\n", "remote", lst[i].share.hshare, lst[i].share.name, lst[i].hostid, lst[i].seq, lst[i].lastid );
        }
        free( lst );
        printf( "\n" );
    }

}

static void cmd_prop( void ) {
     struct nls_prop prop;
     nls_prop( &prop );
     printf( "seq=%"PRIu64"\n", prop.seq );
     printf( "share=%d/%d\n", prop.share_count, prop.share_max );
     printf( "remote=%d/%d\n", prop.remote_count, prop.remote_max );
}
