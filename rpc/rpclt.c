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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

#include <rpc.h>
#include <hrauth.h>
#include <hostreg.h>
#include <raft.h>

struct clt_info {
    uint32_t prog;
    uint32_t vers;
    uint32_t proc;
    char *procname;
    void (*getargs)( int argc, char **argv, int idx, struct xdr_s *xdr );
    void (*results)( struct xdr_s *xdr );	
};

static void rpcbind_results( struct xdr_s *xdr );
static void raft_results( struct xdr_s *xdr );

static struct clt_info clt_procs[] = {
    { 10000, 2, 4, "rpcbind.list", NULL, rpcbind_results },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 3, "raft.list", NULL, raft_results },    
    { 0, 0, 0, NULL, NULL, NULL }
};

static void usage( char *fmt, ... ) {
  va_list args;
  struct clt_info *info;
  
  printf( "Usage: rpclt [OPTIONS] HOSTID program.vers.proc [args...]\n"
	  "\n"
	  "Where OPTIONS:\n"
	  "      -p port             Port\n"
	  "      -L none|integ|priv  Service level\n"
	  "      -t timeout          Timeout (ms)\n" 
	  "\n" );
  info = clt_procs;
  printf( "Programs:\n" );
  while( info->prog ) {
      printf( "    %-8s %u:%u:%u\n",
	      info->procname, info->prog, info->vers, info->proc );
      info++;
  }
  
  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
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

static struct {
    int timeout;
    int port;
    uint64_t hostid;
    int service;
} glob;

int main( int argc, char **argv ) {
    int i, sts;
    struct clt_info *info;
    
    glob.port = 8000;
    glob.timeout = 1000;

    i = 1;
    while( i < argc ) {
	if( strcmp( argv[i], "-p" ) == 0 ) {
	    i++;
	    if( i >= argc ) usage( NULL );
	    glob.port = strtoul( argv[i], NULL, 10 );
	} else if( strcmp( argv[i], "-L" ) == 0 ) {
	    i++;
	    if( i >= argc ) usage( NULL );
	    if( strcmp( argv[i], "none" ) == 0 ) {
		glob.service = HRAUTH_SERVICE_NONE;
	    } else if( strcmp( argv[i], "integ" ) == 0 ) {
		glob.service = HRAUTH_SERVICE_INTEG;
	    } else if( strcmp( argv[i], "priv" ) == 0 ) {
		glob.service = HRAUTH_SERVICE_PRIV;
	    } else usage( NULL );
	} else if( strcmp( argv[i], "-t" ) == 0 ) {
	    i++;
	    if( i >= argc ) usage( NULL );
	    glob.timeout = strtoul( argv[i], NULL, 10 );
	} else break;
	i++;
    }

    glob.hostid = strtoull( argv[i], NULL, 16 );
    i++;
    
    info = clt_procs;
    while( info->prog ) {
	if( strcmp( argv[i], info->procname ) == 0 ) {
	    struct hrauth_call_udp_args args;
	    char argbuf[1024];
	    
	    memset( &args, 0, sizeof(args) );
	    args.prog = info->prog;
	    args.vers = info->vers;
	    args.proc = info->proc;
	    args.port = glob.port;
	    args.timeout = glob.timeout;
	    args.service = glob.service;
	    xdr_init( &args.args, (uint8_t *)argbuf, sizeof(argbuf) );
	    
	    i++;
	    if( info->getargs ) info->getargs( argc, argv, i, &args.args );
	    sts = hrauth_call_udp_sync( &args );
	    if( sts ) usage( "RPC call failed" );
	    if( info->results ) info->results( &args.res );
	    exit( 0 );
	}
	info++;
    }
    usage( "Unknown proc \"%s\"", argv[i] );
    
    return 0;
}

static void rpcbind_results( struct xdr_s *xdr ) {
  int sts, b;
  int i;
  uint32_t prog, vers, prot, port;
  
  i = 0;
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) goto bad;
  while( b ) {
      sts = xdr_decode_uint32( xdr, &prog );
      if( sts ) goto bad;
      sts = xdr_decode_uint32( xdr, &vers );
      if( sts ) goto bad;
      sts = xdr_decode_uint32( xdr, &prot );
      if( sts ) goto bad;
      sts = xdr_decode_uint32( xdr, &port );
      if( sts ) goto bad;

      printf( "%-8u %-8u %-8u %-8u\n", prog, vers, prot, port );
      
      sts = xdr_decode_boolean( xdr, &b );
      if( sts ) goto bad;
  }

 bad:
  usage( "XDR error" );
}

static void raft_results( struct xdr_s *xdr ) {
    int i, n, j, m;
    struct raft_cluster cl;
    struct raft_member member;
    
    xdr_decode_uint32( xdr, (uint32_t *)&n );
    for( i = 0; i < n; i++ ) {
	xdr_decode_uint64( xdr, &cl.id );
	xdr_decode_uint64( xdr, &cl.leaderid );
	xdr_decode_uint64( xdr, &cl.termseq );
	xdr_decode_uint64( xdr, &cl.voteid );
	xdr_decode_uint32( xdr, &cl.state );
	xdr_decode_uint32( xdr, &cl.typeid );
	xdr_decode_uint64( xdr, &cl.commitseq );
	xdr_decode_uint64( xdr, &cl.stateseq );
	xdr_decode_uint64( xdr, &cl.stateterm );
	// TODO: print
	
	xdr_decode_uint32( xdr, (uint32_t *)&m );
	for( j = 0; j < m; j++ ) {
	    xdr_decode_uint64( xdr, &member.hostid );
	    xdr_decode_uint64( xdr, &member.lastseen );
	    xdr_decode_uint32( xdr, &member.flags );
	    xdr_decode_uint64( xdr, &member.nextseq );	    
	    xdr_decode_uint64( xdr, &member.stateseq );
	    // TODO: print 
	}
    }
    
}
