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
#include <mmf.h>
#include <time.h>
#include "nls.h"
#include <hostreg.h>

static void usage( char *fmt, ... ) {
    printf( "Usage:    prop\n"
	    "          set prop [rpc=RPC] [notreg=NOTREG] [notify=NOTIFY]\n"
	    "\n" 
            "          add share path=PATH [hshare=HSHARE]\n"
            "          set share HSHARE [path=PATH]\n"
            "          rem share HSHARE\n"
	    "\n" 
            "          add remote [hshare=HSHARE] [hostid=HOSTID] [remote_period=PERIOD]\n"
            "          set remote HSHARE [hostid=HOSTID] [remote_period=PERIOD]\n"
            "          rem remote HSHARE\n"
	    "\n"
            "          rem notify TAG\n"
	    
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

    sts = hostreg_open();
    if( sts ) usage( "Failed to open hostreg" );
    
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
                 if( strcmp( argname, "path" ) == 0 ) {
                      if( argval ) strncpy( entry.path, argval, sizeof(entry.path) );
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
		 if( strcmp( argname, "hshare" ) == 0 ) {
  		     if( argval ) entry.hshare = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "hostid" ) == 0 ) {
		     if( argval ) entry.hostid = hostreg_hostid_by_name( argval );
		 } else if( strcmp( argname, "lastid" ) == 0 ) {
		     if( argval ) entry.lastid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "seq" ) == 0 ) {
		     if( argval ) entry.seq = strtoull( argval, NULL, 10 );
		 } else if( strcmp( argname, "notify_period" ) == 0 ) {
		     if( argval ) entry.notify_period = strtoul( argval, NULL, 10 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
	    }
            sts = nls_remote_add( &entry );
            if( sts ) usage( "Failed to add remote" );
            printf( "Added remote HSHARE=%"PRIx64"\n", entry.hshare );
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
        } else if( strcmp( argv[i], "notify" ) == 0 ) {
            uint64_t tag;
            i++;
            if( i >= argc ) usage( NULL );
            tag = strtoull( argv[i], NULL, 16 );
            sts = nls_notify_rem( tag );
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
                 if( strcmp( argname, "path" ) == 0 ) {
		     if( argval ) strncpy( entry.path, argval, sizeof(entry.path) );
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
		 if( strcmp( argname, "hostid" ) == 0 ) {
		     if( argval ) entry.hostid = hostreg_hostid_by_name( argval );
		 } else if( strcmp( argname, "lastid" ) == 0 ) {
                      if( argval ) entry.lastid = strtoull( argval, NULL, 16 );
		 } else if( strcmp( argname, "seq" ) == 0 ) {
                      if( argval ) entry.seq = strtoull( argval, NULL, 10 );
		 } else if( strcmp( argname, "notify_period" ) == 0 ) {
                      if( argval ) entry.notify_period = strtoul( argval, NULL, 10 );
                 } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
                 i++;
            }
            sts = nls_remote_set( &entry );
            if( sts ) usage( "Failed to set remote" );
        } else if( strcmp( argv[i], "prop" ) == 0 ) {
            char argname[64], *argval;
            i++;
            while( i < argc ) {
                 argval_split( argv[i], argname, &argval );
                 if( strcmp( argname, "rpc" ) == 0 ) {
		     if( argval ) nls_set_rpc_timeout( strtoull( argval, NULL, 10 ) );
		 } else if( strcmp( argname, "notreg" ) == 0 ) {
		     if( argval ) nls_set_notreg_period( strtoul( argval, NULL, 10 ) );
		 } else if( strcmp( argname, "notify" ) == 0 ) {
		     if( argval ) nls_set_notify_period( strtoul( argval, NULL, 10 ) );		     
                 } else { printf( "Unknown property name %s\n", argname ); usage( NULL ); }
                 i++;
            }
	} else usage( NULL );
    } else if( strcmp( argv[i], "reset" ) == 0 ) {
      nls_reset();
    } else usage( NULL );

    nls_close();
    return 0;
}

static void cmd_list( void ) {
    int i, n, m;
    {
        struct nls_share *lst;
        n = nls_share_list( NULL, 0 );
        lst = (struct nls_share *)malloc( sizeof(*lst) * n );
        m = nls_share_list( lst, n );
        if( m < n ) n = m;
        for( i = 0; i < n; i++ ) {
	    printf( "%-16s %-8"PRIx64" path=%s\n", "share", lst[i].hshare, lst[i].path );
        }
        free( lst );
        if( n > 0 ) printf( "\n" );
    }
    {
        struct nls_remote *lst;
	char timestr[64], lastcstr[64], namestr[HOSTREG_MAX_NAME];
	struct tm *tm;
  	time_t now;
	
        n = nls_remote_list( NULL, 0 );
        lst = (struct nls_remote *)malloc( sizeof(*lst) * n );
        m = nls_remote_list( lst, n );
        if( m < n ) n = m;
        for( i = 0; i < n; i++ ) {
	  if( lst[i].timestamp == 0 ) strcpy( timestr, "Never" );
	  else {
	    now = (time_t)lst[i].timestamp;
	    tm = localtime( &now );
	    strftime( timestr, sizeof(timestr), "%Y-%m-%d %H:%M:%S", tm );
	  }
	  if( lst[i].last_contact == 0 ) strcpy( lastcstr, "Never" );
	  else {
	    now = (time_t)lst[i].last_contact;
	    tm = localtime( &now );
	    strftime( lastcstr, sizeof(lastcstr), "%Y-%m-%d %H:%M:%S", tm );
	  }
	    
	  printf( "%-16s %-8"PRIx64" hostid=%"PRIx64" (%s) seq=%"PRIu64" lastid=%"PRIx64" timestamp=%s path=/opt/fju/etc/nls/%"PRIx64"/%"PRIx64".log notify_period=%us last_contact=%s\n",
		  "remote",
		  lst[i].hshare, lst[i].hostid, hostreg_name_by_hostid( lst[i].hostid, namestr ), lst[i].seq, lst[i].lastid, timestr, lst[i].hostid, lst[i].hshare, lst[i].notify_period, lastcstr );
        }
        free( lst );
        if( n > 0 ) printf( "\n" );
    }
    {
        struct nls_notify *lst;
	char timestr[64], namestr[HOSTREG_MAX_NAME];
	struct tm *tm;
  	time_t now;

        n = nls_notify_list( NULL, 0 );
        lst = (struct nls_notify *)malloc( sizeof(*lst) * n );
        m = nls_notify_list( lst, n );
        if( m < n ) n = m;
        for( i = 0; i < n; i++ ) {
	  if( lst[i].timestamp == 0 ) strcpy( timestr, "Never" );
	  else {
	    now = (time_t)lst[i].timestamp;
	    tm = localtime( &now );
	    strftime( timestr, sizeof(timestr), "%Y-%m-%d %H:%M:%S", tm );
	  }

	  printf( "%-16s %-8"PRIx64" hostid=%"PRIx64" (%s) hshare=%"PRIx64" seq=%"PRIu64" lastid=%"PRIx64" timestamp=%s period=%u\n",
		  "notify",
		  lst[i].tag, lst[i].hostid, hostreg_name_by_hostid( lst[i].hostid, namestr ), lst[i].hshare, lst[i].seq, lst[i].lastid, timestr, lst[i].period );
        }
        free( lst );
        if( n > 0 ) printf( "\n" );
    }

}

static void cmd_prop( void ) {
     struct nls_prop prop;
     nls_prop( &prop );
     printf( "seq=%"PRIu64" rpc=%ums notreg_period=%us notify_period=%us\n", prop.seq, prop.rpc_timeout, prop.notreg_period, prop.notify_period );
     printf( "share=%d/%d\n", prop.share_count, prop.share_max );
     printf( "remote=%d/%d\n", prop.remote_count, prop.remote_max );
     printf( "notify=%d/%d\n", prop.notify_count, prop.notify_max );
}

