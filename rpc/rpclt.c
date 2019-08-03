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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>
#include <time.h>
#ifndef WIN32
#include <arpa/inet.h>
#endif

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
static void clt_call( struct clt_info *info, int argc, char **argv, int i );
static void clt_broadcast( struct clt_info *info, int argc, char **argv, int i );
static void raft_add_results( struct xdr_s *xdr );

static struct clt_info clt_procs[] = {
    { 100000, 2, 0, NULL, NULL, "rpcbind.null", NULL },
    { 100000, 2, 4, NULL, rpcbind_results, "rpcbind.list", NULL },
    { HRAUTH_RPC_PROG, HRAUTH_RPC_VERS, 0, NULL, NULL, "hrauth.null", NULL },    
    { HRAUTH_RPC_PROG, HRAUTH_RPC_VERS, 1, NULL, hrauth_local_results, "hrauth.local", NULL },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 0, NULL, NULL, "raft.null", NULL },    
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 3, NULL, raft_list_results, "raft.list", NULL },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 4, raft_add_args, raft_add_results, "raft.add", "clid=CLID" },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 5, raft_rem_args, NULL, "raft.rem", "clid=CLID" },
    { REX_RPC_PROG, REX_RPC_VERS, 0, NULL, NULL, "rex.null", NULL },    
    { REX_RPC_PROG, REX_RPC_VERS, 1, rex_read_args, rex_read_results, "rex.read", "clid=CLID" },
    { REX_RPC_PROG, REX_RPC_VERS, 2, rex_write_args, rex_write_results, "rex.write", "clid=CLID data=DATA" },
    { 0, 0, 0, NULL, NULL, NULL }
};

static void usage( char *fmt, ... ) {
  va_list args;
  struct clt_info *info;
  char procstr[256];
  
  printf( "Usage: rpclt [OPTIONS] <HOSTID|addr|local|broadcast BCADDR> program.vers.proc [args...]\n"
	  "\n"
	  "Where OPTIONS:\n"
	  "      -p port             Port\n"
	  "      -L none|integ|priv  Service level\n"
	  "      -t timeout          Timeout (ms)\n"
	  "      -P udp|tcp          Call using TCP or UDP protocol (addr only)\n"
	  "      -T                  Report round trip time\n" 
	  "\n" );
  info = clt_procs;
  printf( "Procedures:\n" );
  while( info->prog ) {
      sprintf( procstr, "%u:%u:%u", info->prog, info->vers, info->proc );
      printf( "    %-16s %-16s %s\n",
	      procstr, info->procname, info->procargs ? info->procargs : "" );
      info++;
  }
  
  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }

  exit( 1 );
}

#if 0
static int mynet_pton( char *str, uint8_t *inaddr ) {
    char *p;
    int i, j;
    uint32_t u;
    
    p = str;
    memset( inaddr, 0, 4 );
    
    for( j = 0; j < 4; j++ ) {
	memset( tmp, 0, sizeof(tmp) );
	i = 0;
	while( 1 ) {
	    if( *p == '\0' ) break;
	    if( *p == '.' ) {
		p++;
		break;
	    }

	    if( i <= 3 ) {
	      tmp[i] = *p;
	    }
	    i++;
	    p++;
	}
	u = strtoul( tmp, NULL, 10 );
	if( u > 255 ) return -1;
	inaddr[j] = (uint8_t)u;
	if( *p == '\0' ) break;
    }
    if( j != 4 ) return -1;
    
    return 0;
}
#endif

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
    int broadcast;
    uint32_t addr;
    int tcp;
    int reporttime;
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
    raft_open();
    
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
	} else if( strcmp( argv[i], "-P" ) == 0 ) {
	    i++;
	    if( i >= argc ) usage( NULL );
	    if( strcmp( argv[i], "tcp" ) == 0 ) {
		glob.tcp = 1;
	    } else if( strcmp( argv[i], "udp" ) == 0 ) {
	    } else usage( NULL );
	} else if( strcmp( argv[i], "-T" ) == 0 ) {
	    glob.reporttime = 1;
	} else break;
	i++;
    }

    if( i >= argc ) usage( NULL );
    glob.hostid = strtoull( argv[i], &term, 16 );
    if( *term ) {
      struct hostreg_host host;
      glob.hostid = 0;
      if( strcmp( argv[i], "local" ) == 0 ) {
	glob.hostid = 0;
	glob.addr = htonl( INADDR_LOOPBACK );
	i++;
      } else if( strcmp( argv[i], "broadcast" ) == 0 ) {
	glob.hostid = 0;
	glob.broadcast = 1;
	i++;
	if( i >= argc ) usage( NULL );
	sts = inet_pton( AF_INET, argv[i], &glob.addr );
	//sts = mynet_pton( argv[i], (uint8_t *)&glob.addr );
	if( sts != 1 ) usage( "Invalid IP address" );
	i++;
      } else {
	sts = hostreg_host_by_name( argv[i], &host );
	if( !sts ) {
	  glob.hostid = host.id;
	  i++;
	} else {
	  sts = inet_pton( AF_INET, argv[i], &glob.addr );
	  //sts = mynet_pton( argv[i], (uint8_t *)&glob.addr );
	  if( sts == 1 ) {
	    i++;
	  } else {
	    glob.addr = htonl( INADDR_LOOPBACK );	    
	  }
	}
      }
    }
    if( i >= argc ) usage( NULL );
    
    idx = 0;
    while( clt_procs[idx].prog ) {
      info = &clt_procs[idx];
      if( strcmp( argv[i], info->procname ) == 0 ) {
	if( glob.broadcast ) {
	  clt_broadcast( info, argc, argv, i );
	} else {
	  clt_call( info, argc, argv, i );
	}
	exit( 0 );
      }
      idx++;
    }
    usage( "Unknown host or proc \"%s\"", argv[i] );
    
    return 0;
}

static void clt_call( struct clt_info *info, int argc, char **argv, int i ) {
  struct hrauth_call hcall;
  struct xdr_s args, res;
  struct hrauth_call_opts opts;
  char argbuf[1024];
  int sts;
  struct rpc_call_pars pars;
  struct sockaddr_in *sinp;
  uint64_t tstart, tend;
      
  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  xdr_init( &res, NULL, 0 );
  
  if( glob.hostid ) {
    memset( &hcall, 0, sizeof(hcall) );
    hcall.hostid = glob.hostid;
    hcall.prog = info->prog;
    hcall.vers = info->vers;
    hcall.proc = info->proc;
    hcall.timeout = glob.timeout;
    hcall.service = glob.service;
    
	opts.mask = HRAUTH_CALL_OPT_PORT|HRAUTH_CALL_OPT_TMPBUF;
    opts.port = glob.port;
	xdr_init( &opts.tmpbuf, malloc( 32*1024 ), 32*1024 );
  } else {
    memset( &pars, 0, sizeof(pars) );
    pars.prog = info->prog;
    pars.vers = info->vers;
    pars.proc = info->proc;
    sinp = (struct sockaddr_in *)&pars.raddr;
    sinp->sin_family = AF_INET;
    sinp->sin_port = htons( glob.port );
    sinp->sin_addr.s_addr = glob.addr;
    pars.raddr_len = sizeof(*sinp);
    pars.timeout = glob.timeout;
    xdr_init( &pars.buf, malloc( 32*1024 ), 32*1024 );
  }
   
  i++;
  if( info->getargs ) info->getargs( argc, argv, i, &args );

  tstart = rpc_now();
  if( glob.hostid ) {
    sts = hrauth_call_udp( &hcall, &args, &res, &opts );
  } else if( glob.tcp ) {
    sts = rpc_call_tcp( &pars, &args, &res );
  } else {
    sts = rpc_call_udp( &pars, &args, &res );
  }
  tend = rpc_now();
  if( sts ) usage( "RPC call failed" );
  if( glob.reporttime ) printf( ";; Time: %dms\n", (int)(tend - tstart) );
  if( info->results ) info->results( &res );
}

static void clt_broadcast_cb( struct rpc_inc *inc, void *cxt ) {
  char ipstr[64];
  struct clt_info *info = (struct clt_info *)cxt;
  struct sockaddr_in *sinp = (struct sockaddr_in *)&inc->raddr;
  
  sprintf( ipstr,
	   "%d.%d.%d.%d:%d",
	   (sinp->sin_addr.s_addr >> 24) & 0xff,
	   (sinp->sin_addr.s_addr >> 16) & 0xff,
	   (sinp->sin_addr.s_addr >> 8) & 0xff,
	   (sinp->sin_addr.s_addr) & 0xff,
	   ntohs( sinp->sin_port ) );
  printf( "RECV %s (%d)\n", ipstr, (int)(inc->xdr.count - inc->xdr.offset) );
  if( info->results ) {
    info->results( &inc->xdr );
    printf( "\n" );
  }
}

static void clt_broadcast( struct clt_info *info, int argc, char **argv, int i ) {
  struct rpc_call_pars pars;
  struct xdr_s args;
  char argbuf[1024];
  int sts;
  char *tmpbuf = malloc( 32*1024 );
  struct sockaddr_in sin;
  
  memset( &pars, 0, sizeof(pars) );
  pars.prog = info->prog;
  pars.vers = info->vers;
  pars.proc = info->proc;

  memset( &sin, 0, sizeof(sin) );
  sin.sin_family = AF_INET;
  sin.sin_port = htons( glob.port );

  sin.sin_addr.s_addr = glob.addr;
  memcpy( &pars.raddr, &sin, sizeof(sin) );
  pars.raddr_len = sizeof(sin);
  pars.timeout = 1000;
  xdr_init( &pars.buf, (uint8_t *)tmpbuf, 32*1024 );
  
  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  
  i++;
  if( info->getargs ) info->getargs( argc, argv, i, &args );
  sts = rpc_call_broadcast( &pars, &args, clt_broadcast_cb, info );
  if( sts ) usage( "RPC broadcast failed" );
  
  free( tmpbuf );
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
  int sts, len;
  uint8_t *buf;

  sts = xdr_decode_opaque_ref( xdr, &buf, &len );
  if( sts ) usage( "XDR error" );
  if( len > 0 ) {
    if( buf[len - 1] && len < xdr->count ) buf[len] = '\0';
    printf( "%s\n", buf );
  }
  
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
    } else usage( "Unknown arg \"%s\"", argname );
    i++;
  }
  if( !clid ) usage( "Need CLID" );
  
  xdr_encode_uint64( xdr, clid );
}

static void rex_write_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  uint64_t clid = 0;
  char argname[64], *argval;
  char data[REX_MAX_BUF];
  int len;
  
  memset( data, 0, sizeof(data) );
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "clid" ) == 0 ) {
      clid = strtoull( argval, NULL, 16 );
    } else if( strcmp( argname, "data" ) == 0 ) {
      len = strlen( argval );
      memcpy( data, argval, len );
    } else usage( "Unknown arg \"%s\"", argname );
    i++;
  }
  if( !clid ) usage( "Need CLID" );
  
  xdr_encode_uint64( xdr, clid );
  xdr_encode_opaque( xdr, (uint8_t *)data, len );
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
      } else usage( "Unknown arg \"%s\"", argname );
      i++;
  }
  if( !clid ) usage( "Need CLID" );
  
  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) usage( "Unknown cluster %"PRIx64"", clid );
  nmember = raft_member_list( clid, member, 32 );
  
  xdr_encode_uint64( xdr, clid );
  xdr_encode_uint32( xdr, cl.typeid ); 
  xdr_encode_uint32( xdr, 0 ); // flags 
  
  xdr_encode_uint32( xdr, nmember );
  for( j = 0; j < nmember; j++ ) {
    xdr_encode_uint64( xdr, member[j].hostid == glob.hostid ? hostreg_localid() : member[j].hostid );
  }
    
}
static void raft_rem_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
    uint64_t clid;
    char argname[64], *argval;
    while( i < argc ) {
	argval_split( argv[i], argname, &argval );
	if( strcmp( argname, "clid" ) == 0 ) {
	    clid = strtoull( argval, NULL, 16 );
	} else usage( "Unknown arg \"%s\"", argname );
	i++;
    }
    if( !clid ) usage( "Need CLID" );
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

static void raft_add_results( struct xdr_s *xdr ) {
  int sts, b;
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) goto bad;
  printf( "Success=%s\n", b ? "True" : "False" );
  return;
  
 bad:
  usage( "XDR error" );
}

