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
#include <time.h>

#include <rpc.h>
#include <hrauth.h>
#include <hostreg.h>
#include <raft.h>
#include <rex.h>

struct clt_info {
    uint32_t prog;
    uint32_t vers;
    uint32_t proc;
    void (*getargs)( int argc, char **argv, int idx, struct xdr_s *xdr );
    void (*results)( struct xdr_s *xdr );
    char *procname;
    char *procargs;
};

static void rpcbind_results( struct xdr_s *xdr );
static void raft_list_results( struct xdr_s *xdr );
static void raft_add_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void raft_rem_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void rex_read_results( struct xdr_s *xdr );
static void rex_write_results( struct xdr_s *xdr );
static void rex_read_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void rex_write_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void hrauth_local_results( struct xdr_s *xdr );

static struct clt_info clt_procs[] = {
    { 100000, 2, 4, NULL, rpcbind_results, "rpcbind.list", NULL },
    { HRAUTH_PROGRAM, HRAUTH_VERSION, 1, NULL, hrauth_local_results, "hrauth.local", NULL },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 3, NULL, raft_list_results, "raft.list", NULL },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 4, raft_add_args, NULL, "raft.add", "clid=CLID" },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 5, raft_rem_args, NULL, "raft.rem", "clid=CLID" },
    { REX_RPC_PROG, REX_RPC_VERS, 1, rex_read_args, rex_read_results, "rex.read", "clid=CLID" },
    { REX_RPC_PROG, REX_RPC_VERS, 2, rex_write_args, rex_write_results, "rex.write", "clid=CLID data=DATA" },
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
      printf( "    %u:%u:%u %s %s\n",
	      info->prog, info->vers, info->proc, info->procname, info->procargs ? info->procargs : "" );
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
    else *argval = "";

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
  int i, sts, idx;
    struct clt_info *info;
    char *term;

#ifdef WIN32
    {
	WSADATA wsadata;
	WSAStartup( MAKEWORD(2,2), &wsadata );
    }
#endif

    hostreg_open();
    
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
	    } else if( strcmp( argv[i], "noauth" ) == 0 ) {
		glob.service = -1;		
	    } else usage( NULL );
	} else if( strcmp( argv[i], "-t" ) == 0 ) {
	    i++;
	    if( i >= argc ) usage( NULL );
	    glob.timeout = strtoul( argv[i], NULL, 10 );
	} else break;
	i++;
    }

    if( i >= argc ) usage( NULL );
    glob.hostid = strtoull( argv[i], &term, 16 );
    if( *term ) {
      struct hostreg_host host;
      if( strcmp( argv[i], "local" ) == 0 ) {
	glob.hostid = 0;
	i++;
      } else {
	sts = hostreg_host_by_name( argv[i], &host );
	if( !sts ) {
	  glob.hostid = host.id;
	  i++;
	}
      }
    }
    if( i >= argc ) usage( NULL );
    
    idx = 0;
    while( clt_procs[idx].prog ) {
      info = &clt_procs[idx];
	if( strcmp( argv[i], info->procname ) == 0 ) {
	    struct hrauth_call hcall;
	    struct xdr_s args, res;
	    struct hrauth_call_opts opts;
	    char argbuf[1024];
	    
	    memset( &hcall, 0, sizeof(hcall) );
	    hcall.hostid = glob.hostid;
	    hcall.prog = info->prog;
	    hcall.vers = info->vers;
	    hcall.proc = info->proc;
	    hcall.timeout = glob.timeout;
	    hcall.service = glob.service;
	    xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
	    xdr_init( &res, NULL, 0 );

	    opts.mask = HRAUTH_CALL_OPT_PORT;
	    opts.port = glob.port;
	    
	    i++;
	    if( info->getargs ) info->getargs( argc, argv, i, &args );
	    sts = hrauth_call_udp( &hcall, &args, &res, &opts );
	    if( sts ) usage( "RPC call failed" );
	    if( info->results ) info->results( &res );
	    exit( 0 );
	}
	idx++;
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

      printf( "%-12u %-8u %-8u %-8u\n", prog, vers, prot, port );
      
      sts = xdr_decode_boolean( xdr, &b );
      if( sts ) goto bad;
  }
  return;
  
 bad:
  usage( "XDR error" );
}

static void raft_list_results( struct xdr_s *xdr ) {
    int i, n, j, m;
    struct raft_cluster cl;
    struct raft_member member;
    char timestr[64];
    struct tm *tm;
    time_t now;
    
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
	printf( "Cluster ID=%"PRIx64" leader=%"PRIx64" termseq=%"PRIu64"\n"
		"        state=%s typeid=%u commitseq=%"PRIu64" stateseq=%"PRIu64" stateterm=%"PRIu64"\n",
		cl.id, cl.leaderid, cl.termseq,
		cl.state == RAFT_STATE_FOLLOWER ? "FOLLOWER" :
		cl.state == RAFT_STATE_CANDIDATE ? "CANDIDATE" :
		cl.state == RAFT_STATE_LEADER ? "LEADER" :
		"Unknown",
		cl.typeid, cl.commitseq, cl.stateseq, cl.stateterm );
	
	xdr_decode_uint32( xdr, (uint32_t *)&m );
	for( j = 0; j < m; j++ ) {
	    xdr_decode_uint64( xdr, &member.hostid );
	    xdr_decode_uint64( xdr, &member.lastseen );
	    xdr_decode_uint32( xdr, &member.flags );
	    xdr_decode_uint64( xdr, &member.nextseq );	    
	    xdr_decode_uint64( xdr, &member.stateseq );

	    if( member.lastseen ) {
	      now = (time_t)member.lastseen;
	      tm = localtime( &now );
	      strftime( timestr, sizeof(timestr), "%Y-%m-%d %H:%M:%S", tm );
	    } else {
	      strcpy( timestr, "Never" );
	    }
	  	    
	    printf( "    Member ID=%"PRIx64" lastseen=%s flags=%x nextseq=%"PRIu64" stateseq=%"PRIu64"\n",
		    member.hostid, timestr, member.flags, member.nextseq, member.stateseq );
	}
    }
    
}

static void rex_read_results( struct xdr_s *xdr ) {
  int sts, len, i;
  uint8_t *buf;

  sts = xdr_decode_opaque_ref( xdr, &buf, &len );
  if( sts ) usage( "XDR error" );

  for( i = 0; i < len; i++ ) {
    if( i > 0 && (i % 16) == 0 ) printf( "\n" );
    printf( "%02x ", (uint32_t)buf[i] );
  }
  printf( "\n" );
}

static void rex_write_results( struct xdr_s *xdr ) {
  int sts, b;
  uint64_t leaderid;

  sts = xdr_decode_boolean( xdr, &b );
  sts = xdr_decode_uint64( xdr, &leaderid );

  printf( "Success=%s Leader=%"PRIx64"\n", b ? "True" : "False", leaderid );
}

static void rex_read_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  uint64_t clid = 0;
  char argname[64], *argval;
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "clid" ) == 0 ) {
      clid = strtoul( argval, NULL, 16 );
    } else usage( NULL );
    i++;
  }
  if( !clid ) usage( NULL );
  
  xdr_encode_uint64( xdr, clid );
}

static void rex_write_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  uint64_t clid = 0;
  char argname[64], *argval;
  char data[REX_MAX_BUF];
  char tmp[4];
  int len;
  int j;
  
  memset( data, 0, sizeof(data) );
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "clid" ) == 0 ) {
      clid = strtoul( argval, NULL, 16 );
    } else if( strcmp( argname, "data" ) == 0 ) {
      len = strlen( argval ) / 2;
      for( j = 0; j < (len > REX_MAX_BUF ? REX_MAX_BUF : len); j++ ) {
	memset( tmp, 0, sizeof(tmp) );
	tmp[0] = argval[2*j];
	tmp[1] = argval[2*j + 1];
	data[j] = strtoul( tmp, NULL, 16 );
      }
    } else usage( NULL );
    i++;
  }
  if( !clid ) usage( NULL );
  
  xdr_encode_uint64( xdr, clid );
  xdr_encode_opaque( xdr, (uint8_t *)data, REX_MAX_BUF );
}

static void raft_add_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  int sts;
  struct raft_cluster cl;
  struct raft_member member[32];
  int nmember, j;
  uint64_t clid = 0;
  char argname[64], *argval;
  
  while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "clid" ) == 0 ) {
	  clid = strtoull( argval, NULL, 16 );
      } else usage( NULL );
      i++;
  }
  if( !clid ) usage( NULL );
  
  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) usage( "Unknown cluster" );
  nmember = raft_member_list( clid, member, 32 );
  
  xdr_encode_uint64( xdr, clid );
  xdr_encode_uint32( xdr, cl.typeid ); 
  xdr_encode_uint32( xdr, 0 ); // flags 
  
  xdr_encode_uint32( xdr, nmember );
  for( j = 0; j < nmember; j++ ) {
    xdr_encode_uint64( xdr, member[j].hostid );
  }
    
}
static void raft_rem_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
    uint64_t clid;
    char argname[64], *argval;
    while( i < argc ) {
	argval_split( argv[i], argname, &argval );
	if( strcmp( argname, "clid" ) == 0 ) {
	    clid = strtoull( argval, NULL, 16 );
	} else usage( NULL );
	i++;
    }
    if( !clid ) usage( NULL );
    xdr_encode_uint64( xdr, clid );
}

static void bn2hex( char *bn, char *hex, int len ) {
  int i;
  unsigned int x;
  memset( hex, 0, 2*len );
  for( i = 0; i < len; i++ ) {
    x = (unsigned int)((unsigned char)bn[i]);
    sprintf( hex + 2*i, "%02x", x );
  }
  hex[len*2] = '\0';
}
static char *mynet_ntop( uint32_t inaddr, char *str ) {
  uint8_t *inp = (uint8_t *)&inaddr;
    sprintf( str, "%d.%d.%d.%d", inp[0], inp[1], inp[2], inp[3] );
    return str;	     
}
static void print_host( struct hostreg_host *host ) {
    char hex[256];
    int j;
    
    memset( hex, 0, sizeof(hex) );
    bn2hex( (char *)host->pubkey, hex, host->publen );
    printf( "ID=%"PRIx64" name=%s pubkey=%s ",
	    host->id, host->name, hex );

    for( j = 0; j < host->naddr; j++ ) {
	mynet_ntop( host->addr[j], hex );
	printf( "addr=%s ", hex );
    }
    printf( "\n" );    
}

static void hrauth_local_results( struct xdr_s *xdr ) {
    struct hostreg_host x;
    int sts, i;
    
    sts = xdr_decode_uint64( xdr, &x.id );
    if( sts ) goto bad;
    sts = xdr_decode_string( xdr, x.name, sizeof(x.name) );
    if( sts ) goto bad;
    x.publen = sizeof(x.pubkey);
    sts = xdr_decode_opaque( xdr, x.pubkey, (int *)&x.publen );
    if( sts ) goto bad;
    sts = xdr_decode_uint32( xdr, &x.naddr );
    if( sts ) goto bad;
    if( x.naddr > HOSTREG_MAX_ADDR ) goto bad;
    for( i = 0; i < x.naddr; i++ ) {
	sts = xdr_decode_uint32( xdr, &x.addr[i] );
	if( sts ) goto bad;
    }
    print_host( &x );
    
    return;
bad:
    usage( "XDR error" );
}
