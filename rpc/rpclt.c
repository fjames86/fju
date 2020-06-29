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
#define strcasecmp _stricmp
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
#include <ctype.h>

#include <fju/rpc.h>
#include <fju/hrauth.h>
#include <fju/hostreg.h>
#include <fju/raft.h>
#include <fju/rex.h>
#include <fju/nls.h>
#include <fju/freg.h>
#include <fju/sec.h>
#include <fju/programs.h>

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
static void nls_list_results( struct xdr_s *xdr );
static void nls_read_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void nls_read_results( struct xdr_s *xdr );
static void nls_write_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void nls_write_results( struct xdr_s *xdr );
static void freg_list_results( struct xdr_s *xdr );
static void freg_list_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void freg_get_results( struct xdr_s *xdr );
static void freg_get_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void freg_put_results( struct xdr_s *xdr );
static void freg_put_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void freg_rem_results( struct xdr_s *xdr );
static void freg_rem_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void cmdprog_event_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void fvm_list_results( struct xdr_s *xdr );
static void fvm_load_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void fvm_load_results( struct xdr_s *xdr );
static void fvm_register_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void fvm_register_results( struct xdr_s *xdr );
static void fvm_cluster_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void fvm_cluster_results( struct xdr_s *xdr );
static void rawmode_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void rawmode_results( struct xdr_s *xdr );


static struct clt_info clt_procs[] = {
    { 100000, 2, 0, NULL, NULL, "rpcbind.null", NULL },
    { 100000, 2, 4, NULL, rpcbind_results, "rpcbind.list", NULL },
    { HRAUTH_RPC_PROG, HRAUTH_RPC_VERS, 1, NULL, hrauth_local_results, "hrauth.local", NULL },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 3, NULL, raft_list_results, "raft.list", NULL },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 4, raft_add_args, raft_add_results, "raft.add", "clid=CLID" },
    { RAFT_RPC_PROG, RAFT_RPC_VERS, 5, raft_rem_args, NULL, "raft.rem", "clid=CLID" },
    { REX_RPC_PROG, REX_RPC_VERS, 1, rex_read_args, rex_read_results, "rex.read", "clid=CLID" },
    { REX_RPC_PROG, REX_RPC_VERS, 2, rex_write_args, rex_write_results, "rex.write", "clid=CLID data=DATA" },
    { NLS_RPC_PROG, NLS_RPC_VERS, 1, NULL, nls_list_results, "nls.list", NULL },
    { NLS_RPC_PROG, NLS_RPC_VERS, 3, nls_read_args, nls_read_results, "nls.read", "hshare=HSHARE [id=ID]" },
    { NLS_RPC_PROG, NLS_RPC_VERS, 4, nls_write_args, nls_write_results, "nls.write", "hshare=HSHARE [str=string]" },
    { FREG_RPC_PROG, FREG_RPC_VERS, 1, freg_list_args, freg_list_results, "freg.list", "parentid=PARENTID" },
    { FREG_RPC_PROG, FREG_RPC_VERS, 2, freg_get_args, freg_get_results, "freg.get", "id=PARENTID" },
    { FREG_RPC_PROG, FREG_RPC_VERS, 3, freg_put_args, freg_put_results, "freg.put", "parentid=PARENTID name=NAME flags=FLAGS [u32=*] [u64=*] [str=*] [opaque-file=*]" },
    { FREG_RPC_PROG, FREG_RPC_VERS, 4, freg_rem_args, freg_rem_results, "freg.rem", "parentid=PARENTID id=ID" },
    { FJUD_RPC_PROG, 1, 1, NULL, NULL, "fjud.stop", NULL },
    { FJUD_RPC_PROG, 1, 2, cmdprog_event_args, NULL, "fjud.event", "category=* eventid=* parm=*" },
    { FVM_RPC_PROG, 1, 1, NULL, fvm_list_results, "fvm.list", NULL },
    { FVM_RPC_PROG, 1, 2, fvm_load_args, fvm_load_results, "fvm.load", "filename=* register=true|false" },
    { FVM_RPC_PROG, 1, 3, fvm_register_args, fvm_register_results, "fvm.unload", "name=*" },
    { FVM_RPC_PROG, 1, 4, fvm_register_args, fvm_register_results, "fvm.register", "name=*" },
    { FVM_RPC_PROG, 1, 5, fvm_register_args, fvm_register_results, "fvm.unregister", "name=*" },
    { FVM_RPC_PROG, 1, 8, fvm_cluster_args, fvm_cluster_results, "fvm.cluster", "name=* clid=*" },
    { 0, 0, 0, rawmode_args, rawmode_results, "rawmode", "prog vers proc [u32=*] [u64=*] [str=*] [bool=*]" },
    
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
  while( info->procname ) {
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
    char *procname;
    
#ifdef WIN32
    {
	WSADATA wsadata;
	WSAStartup( MAKEWORD(2,2), &wsadata );
    }
#endif

    hostreg_open();
    raft_open();
    freg_open( NULL, NULL );

    glob.hostid = hostreg_localid();
    glob.port = 8000;
    sts = freg_ensure( NULL, 0, "/fju/rpc/port", FREG_TYPE_UINT32, (char *)&glob.port, sizeof(glob.port), NULL );
    glob.timeout = 1000;
    sts = freg_ensure( NULL, 0, "/fju/rpc/timeout", FREG_TYPE_UINT32, (char *)&glob.timeout, sizeof(glob.timeout), NULL );

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

    if( i >= argc ) glob.addr = htonl( INADDR_LOOPBACK );
    else {
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
    }
    
    if( i >= argc ) procname = "rpcbind.list";
    else procname = argv[i]; 
    
    idx = 0;
    while( clt_procs[idx].procname ) {
      info = &clt_procs[idx];
      if( strcmp( procname, info->procname ) == 0 ) {

	if( info->prog == 0 ) {
	  /* rawmode expect args: prog vers proc */
	  i++;
	  if( i >= argc ) usage( "Need prog" );
	  info->prog = strtoul( argv[i], NULL, 0 );
	  i++;
	  if( i >= argc ) usage( "Need vers" );
	  info->vers = strtoul( argv[i], NULL, 0 );
	  i++;
	  if( i >= argc ) usage( "Need proc" );
	  info->proc = strtoul( argv[i], NULL, 0 );
	}
	
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
  char *argbuf;
  int sts;
  struct rpc_call_pars pars;
  struct sockaddr_in *sinp;
  uint64_t tstart, tend;

  argbuf = malloc( 32 * 1024 );
  xdr_init( &args, (uint8_t *)argbuf, 32*1024 );
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
  free( argbuf );
}

static void clt_broadcast_cb( struct rpc_inc *inc, void *cxt ) {
  char ipstr[64];
  struct clt_info *info = (struct clt_info *)cxt;
  struct sockaddr_in *sinp = (struct sockaddr_in *)&inc->raddr;
  
  sprintf( ipstr,
	   "%d.%d.%d.%d:%d",
	   (sinp->sin_addr.s_addr) & 0xff,
	   (sinp->sin_addr.s_addr >> 8) & 0xff,
	   (sinp->sin_addr.s_addr >> 16) & 0xff,
	   (sinp->sin_addr.s_addr >> 24) & 0xff,
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

static struct {
  uint32_t prog;
  char *name;
} rpcbind_name_list[] = {
  { HRAUTH_RPC_PROG, "hrauth" },
  { RAFT_RPC_PROG, "raft" },
  { REX_RPC_PROG, "rex" },
  { NLS_RPC_PROG, "nls" },
  { FREG_RPC_PROG, "freg" },
  { FVM_RPC_PROG, "fvm" },
  { RPCBIND_RPC_PROG, "rpcbind" },
  { FJUD_RPC_PROG, "fjud" },
  { SVCTEST_RPC_PROG, "svctest" },
  { 0, NULL }
};

static char *rpcbind_name_by_prog( uint32_t prog ) {
  int i;
  i = 0;
  while( 1 ) {
    if( !rpcbind_name_list[i].name ) break;
    if( rpcbind_name_list[i].prog == prog ) return rpcbind_name_list[i].name;
    i++;
  }
  return "";
}

static void rpcbind_results( struct xdr_s *xdr ) {
  int sts, b;
  uint32_t prog, vers, prot, port;
  
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

      printf( "%-12u %-8u %-8u %-8u %s\n", prog, vers, prot, port, rpcbind_name_by_prog( prog ) );
      
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
	      sec_timestr( member.lastseen, timestr );
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
  if( sts ) usage( "xdr error" );
  sts = xdr_decode_uint64( xdr, &leaderid );
  if( sts ) usage( "xdr error" );
  
  printf( "Success=%s Leader=%"PRIx64"\n", b ? "True" : "False", leaderid );
}

static void rex_read_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
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

static int nls_decode_prop( struct xdr_s *xdr, uint64_t *hshare, struct log_prop *prop ) {
  int sts;
  sts = xdr_decode_uint64( xdr, hshare );
  sts = xdr_decode_uint32( xdr, &prop->version );
  sts = xdr_decode_uint64( xdr, &prop->seq );
  sts = xdr_decode_uint32( xdr, &prop->lbacount );
  sts = xdr_decode_uint32( xdr, &prop->start );
  sts = xdr_decode_uint32( xdr, &prop->count );
  sts = xdr_decode_uint64( xdr, &prop->last_id );
  sts = xdr_decode_uint32( xdr, &prop->flags );
  return sts;
}
  
static void nls_list_results( struct xdr_s *xdr ) {
  uint64_t hshare;
  struct log_prop prop;
  int sts, b;

  sts = xdr_decode_boolean( xdr, &b );
  printf( "%-16s %-8s %-6s %-6s %-6s %-16s %-4s\n", "hshare", "seq", "lbac", "start", "count", "lastid", "flags" );
  
  while( !sts && b ) {
    nls_decode_prop( xdr, &hshare, &prop );
    printf( "%-16"PRIx64" %-8"PRIu64" %-6u %-6u %-6u %-16"PRIx64" %-4x\n",
	    hshare, prop.seq, prop.lbacount, prop.start, prop.count, prop.last_id, prop.flags );

    sts = xdr_decode_boolean( xdr, &b );
  }
}

static void nls_read_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  uint64_t hshare, id;

  hshare = 0;
  id = 0;
  while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "hshare" ) == 0 ) {
	  hshare = strtoull( argval, NULL, 16 );
      } else if( strcmp( argname, "id" ) == 0 ) {
	id = strtoull( argval, NULL, 16 );
      } else usage( "Unknown arg \"%s\"", argname );
      i++;
  }
  if( !hshare ) usage( "Need hshare" );
  
  xdr_encode_uint64( xdr, hshare );
  xdr_encode_uint64( xdr, id ); 
  xdr_encode_uint32( xdr, 8*1024 ); // xdr count    
}

static void nls_read_results( struct xdr_s *xdr ) {
  int sts, b;
  uint64_t hshare;
  struct log_prop prop;
  uint64_t id, prev_id, seq;
  uint32_t flags;
  char *bufp;
  int lenp;
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  if( !b ) usage( "Unknown share" );
  sts = nls_decode_prop( xdr, &hshare, &prop );
  if( sts ) usage( "XDR error" );
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  printf( "%-16s %-8s %-8s\n", "ID", "Seq", "Flags" );
  while( !sts && b ) {
    sts = xdr_decode_uint64( xdr, &id );
    sts = xdr_decode_uint64( xdr, &prev_id );
    sts = xdr_decode_uint64( xdr, &seq );
    sts = xdr_decode_uint32( xdr, &flags );
    sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&bufp, &lenp );
    if( sts ) usage( "XDR error" );
    printf( "%-16"PRIx64" %-8"PRIu64" %-8x %s\n",
	    id, seq, flags, flags & LOG_BINARY ? "Binary" : bufp );
    
    sts = xdr_decode_boolean( xdr, &b );
  }
  
}

static void nls_write_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  uint64_t hshare;
  char *str;
  uint32_t flags;
  
  hshare = 0;
  str = "";
  flags = 0;
  while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "hshare" ) == 0 ) {
	  hshare = strtoull( argval, NULL, 16 );
      } else if( strcmp( argname, "str" ) == 0 ) {
	str = argval;
      } else if( strcmp( argname, "flags" ) == 0 ) {
	flags = strtoul( argval, NULL, 16 );
      } else usage( "Unknown arg \"%s\"", argname );
      i++;
  }
  if( !hshare ) usage( "Need hshare" );
  
  xdr_encode_uint64( xdr, hshare );
  xdr_encode_uint32( xdr, flags & ~LOG_BINARY ); 
  xdr_encode_opaque( xdr, (uint8_t *)str, strlen( str ) );
}

static void nls_write_results( struct xdr_s *xdr ) {
  int sts, b;
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  printf( "%s\n", b ? "Success" : "Failure" );  
}

static void freg_list_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
    char argname[64], *argval;
    uint64_t parentid = 0;
    
    while( i < argc ) {
	argval_split( argv[i], argname, &argval );
	if( strcmp( argname, "parentid" ) == 0 ) {
	    parentid = strtoull( argval, NULL, 16 );
	} else usage( "Unknown arg \"%s\"", argname );
	i++;
    }
    xdr_encode_uint64( xdr, parentid );
}

static void freg_list_results( struct xdr_s *xdr ) {
    int sts, b;
    uint32_t u32;
    uint64_t u64;
    char *bufp;
    int lenp, i;
    char name[FREG_MAX_NAME];
    char str[512];
    uint32_t flags;
    uint64_t id;
    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) usage( "xdr error" );
    while( b ) {
	xdr_decode_uint64( xdr, &id );
	xdr_decode_string( xdr, name, sizeof(name) );
	xdr_decode_uint32( xdr, &flags );
	printf( "%016"PRIx64" %-32s ", id, name );
	switch( flags & FREG_TYPE_MASK ) {
	case FREG_TYPE_UINT32:
	    xdr_decode_uint32( xdr, &u32 );
	    printf( "u32 %u\n", u32 );
	    break;
	case FREG_TYPE_UINT64:
	    xdr_decode_uint64( xdr, &u64 );
	    printf( "u64 %"PRIu64"\n", u64 );
	    break;
	case FREG_TYPE_KEY:
	  //	    xdr_decode_uint64( xdr, &u64 );
	  printf( "key\n" ); // %"PRIx64"\n", u64 );
	    break;
	case FREG_TYPE_STRING:
	    xdr_decode_string( xdr, str, sizeof(str) );
	    printf( "str %s\n", str );
	    break;
	case FREG_TYPE_OPAQUE:
	  xdr_decode_opaque_ref( xdr, (uint8_t **)&bufp, &lenp );
	    printf( "opaque " );
	    for( i = 0; i < lenp; i++ ) {
		printf( "%02x", (uint32_t)(uint8_t)bufp[i] );
	    }
	    printf( "\n" );
	    break;
	}
	
	xdr_decode_boolean( xdr, &b );
    }
    
   
}

static void freg_get_results( struct xdr_s *xdr ) {
  int sts, b, i;
  uint32_t flags;
  uint64_t id;
  char *bufp;
  int lenp;
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  if( b ) usage( "Failed to get" );

  sts = xdr_decode_uint64( xdr, &id );
  sts = xdr_decode_uint32( xdr, &flags );
  printf( "%"PRIx64" flags=%x", id, flags );
  xdr_decode_opaque_ref( xdr, (uint8_t **)&bufp, &lenp );
  for( i = 0; i < lenp; i++ ) {
    printf( "%02x", (uint32_t)((uint8_t *)bufp)[i] );
  }
  printf( "\n" );
  
}

static void freg_get_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
    char argname[64], *argval;
    uint64_t id = 0;
    
    while( i < argc ) {
	argval_split( argv[i], argname, &argval );
	if( strcmp( argname, "id" ) == 0 ) {
	    id = strtoull( argval, NULL, 16 );
	} else usage( "Unknown arg \"%s\"", argname );
	i++;
    }
    xdr_encode_uint64( xdr, id );  
}

static void freg_put_results( struct xdr_s *xdr ) {
  int sts, b;
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "xdr error" );
  printf( "%s\n", b ? "Success" : "Failure" );
}

static void freg_put_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  uint64_t parentid = 0;
  char *name = NULL;
  uint32_t flags, u32;
  uint64_t u64;
  char *bufp = NULL;
  int len = 0;
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "parentid" ) == 0 ) {
      parentid = strtoull( argval, NULL, 16 );
    } else if( strcmp( argname, "name" ) == 0 ) {
      name = argval;
    } else if( strcmp( argname, "flags" ) == 0 ) {
      flags = strtoul( argval, NULL, 16 );
    } else if( strcmp( argname, "u32" ) == 0 ) {
      u32 = strtoul( argval, NULL, 10 );
      bufp = (char *)&u32;
      len = 4;
    } else if( strcmp( argname, "u64" ) == 0 ) {
      u64 = strtoull( argval, NULL, 16 );
      bufp = (char *)&u64;
      len = 8;
    } else if( strcmp( argname, "str" ) == 0 ) {
      bufp = argval;
      len = strlen( argval );
    } else if( strcmp( argname, "opaque-file" ) == 0 ) {
      struct mmf_s mmf;
      int sts;
      
      sts = mmf_open2( argval, &mmf, MMF_OPEN_EXISTING );
      if( sts ) usage( "Failed to open file \"%s\"", argval );
      sts = mmf_remap( &mmf, mmf.fsize );
      if( sts ) usage( "Failed to map file" );
      bufp = malloc( mmf.fsize );
      len = mmf.fsize;
      memcpy( bufp, mmf.file, mmf.fsize );
      mmf_close( &mmf );
    } else usage( "Unknown arg \"%s\"", argname );
    i++;
  }
  if( !name ) usage( "Need name" );
  
  xdr_encode_uint64( xdr, parentid );
  xdr_encode_string( xdr, name );
  xdr_encode_uint32( xdr, flags );
  xdr_encode_opaque( xdr, (uint8_t *)bufp, len );
}

static void freg_rem_results( struct xdr_s *xdr ) {
  int sts, b;
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "xdr error" );
  printf( "%s\n", b ? "Success" : "Failure" );  
}


static void freg_rem_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  uint64_t id = 0, parentid = 0;
    
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "id" ) == 0 ) {
      id = strtoull( argval, NULL, 16 );
    } else if( strcmp( argname, "parentid" ) == 0 ) {
      parentid = strtoull( argval, NULL, 16 );
    } else usage( "Unknown arg \"%s\"", argname );
    i++;
  }
  xdr_encode_uint64( xdr, parentid );  
  xdr_encode_uint64( xdr, id );  
}

static void cmdprog_event_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  uint32_t category, eventid;
  uint8_t parm[1024];
  int parmlen, j;
  char tmp[4];
  
  category = 0;
  eventid = 0;
  parmlen = 0;
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "category" ) == 0 ) {
      if( !argval ) usage( "Need category" );
      category = strtoul( argval, NULL, 10 );
    } else if( strcmp( argname, "eventid" ) == 0 ) {
      if( !argval ) usage( "Need eventid" );
      eventid = strtoul( argval, NULL, 10 );
    } else if( strcmp( argname, "parm" ) == 0 ) {
	parmlen = strlen( argval ) / 2;
	memset( tmp, 0, 4 );
	for( j = 0; j < parmlen; j++ ) {
	    tmp[0] = argval[2*j];
	    tmp[1] = argval[2*j + 1];
	    parm[j] = (uint8_t)strtoul( tmp, NULL, 16 );
	}
    } else usage( "Unknown arg \"%s\"", argname );
    i++;
  }

  xdr_encode_uint32( xdr, category );
  xdr_encode_uint32( xdr, eventid );
  xdr_encode_opaque( xdr, parm, parmlen );
}

static void fvm_list_results( struct xdr_s *xdr ) {
  int sts, b;
  char name[64];
  uint32_t progid, versid, datasize, textsize;
  int c;
  uint64_t clid;
  uint32_t flags;
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "xdr error" );
  while( b ) {
    sts = xdr_decode_string( xdr, name, sizeof(name) );
    sts = xdr_decode_uint32( xdr, &progid );
    sts = xdr_decode_uint32( xdr, &versid );
    sts = xdr_decode_uint32( xdr, &datasize );
    sts = xdr_decode_uint32( xdr, &textsize );
    sts = xdr_decode_uint64( xdr, &clid );
    printf( "%-32s Program %u:%u Data %u Text %u CLID %"PRIx64"\n",
	    name, progid, versid, datasize, textsize, clid );
    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) usage( "xdr error" );
    c = 0;
    while( b ) {
      sts = xdr_decode_string( xdr, name, sizeof(name) );
      sts = xdr_decode_uint32( xdr, &flags );
      printf( "  ID %-4u Name %-16s Flags 0x%04x\n", c, name, flags );
      c++;
      
      sts = xdr_decode_boolean( xdr, &b );
      if( sts ) usage( "xdr error" );
    }
    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) usage( "xdr error" );
  }
  
}

static void fvm_load_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char *filename = NULL;
  struct mmf_s mmf;
  char *buf;
  int sts, registerp;
  char argname[64], *argval;

  registerp = 0;
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "filename" ) == 0 ) {
      filename = argval;
    } else if( strcmp( argname, "register" ) == 0 ) {
      registerp = (strcasecmp( argval, "true" ) == 0);     
    } else usage( NULL );
    i++;
  }

  if( !filename ) usage( NULL );

  sts = mmf_open2( filename, &mmf, MMF_OPEN_EXISTING );
  if( sts ) usage( "Failed to open file" );

  buf = malloc( mmf.fsize );
  mmf_read( &mmf, buf, mmf.fsize, 0 );
  xdr_encode_opaque( xdr, (uint8_t *)buf, mmf.fsize );
  free( buf );
  xdr_encode_boolean( xdr, registerp );
  
  mmf_close( &mmf );
}

static void fvm_load_results( struct xdr_s *xdr ) {
  int b;
  
  xdr_decode_boolean( xdr, &b );
  printf( "%s\n", b ? "failure" : "success" );
}


static void fvm_register_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  char name[64];

  memset( name, 0, sizeof(name) );
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "name" ) == 0 ) {
      strncpy( name, argval, 63 );
    } else usage( NULL );
    i++;
  }

  if( !name[0] ) usage( "Need name" );
  
  xdr_encode_string( xdr, name );
}

static void fvm_register_results( struct xdr_s *xdr ) {
  int b;
  
  xdr_decode_boolean( xdr, &b );
  printf( "%s\n", b ? "failure" : "success" );
}

static void rawmode_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "u32" ) == 0 ) {
      xdr_encode_uint32( xdr, strtoul( argval, NULL, 0 ) );
    } else if( strcmp( argname, "u64" ) == 0 ) {
      xdr_encode_uint64( xdr, strtoull( argval, NULL, 0 ) );
    } else if( strcmp( argname, "str" ) == 0 ) {
      xdr_encode_string( xdr, argval );
    } else if( strcmp( argname, "bool" ) == 0 ) {
      xdr_encode_boolean( xdr, strcmp( argval, "true" ) == 0 );
    } else usage( NULL );
    i++;
  }

}

static void rawmode_results( struct xdr_s *xdr ) {
  int i, count, j, c;
  count = xdr->count - xdr->offset;
  
  for( i = 0; i < count; i += 16 ) {
    for( j = 0; j < 16; j++ ) {
      if( (i + j) < count ) {
	printf( "%02x ", (uint32_t)xdr->buf[xdr->offset + i + j] );
      }
    }
    for( j = 0; j < 16; j++ ) {
      if( (i + j) < count ) {
	c = xdr->buf[xdr->offset + i + j];
	if( !isalnum( c ) ) c = '.';
	printf( "%c", c );
      }
    }
    
    printf( "\n" );
  }
}

static void fvm_cluster_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  char name[64];
  uint64_t clid = 0;
  
  memset( name, 0, sizeof(name) );
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "name" ) == 0 ) {
      strncpy( name, argval, 63 );
    } else if( strcmp( argname, "clid" ) == 0 ) {
      clid = strtoull( argval, NULL, 16 );
    } else usage( NULL );
    i++;
  }

  if( !name[0] ) usage( "Need name" );
  
  xdr_encode_string( xdr, name );
  xdr_encode_uint64( xdr, clid );
}

static void fvm_cluster_results( struct xdr_s *xdr ) {
  int b;
  
  xdr_decode_boolean( xdr, &b );
  printf( "%s\n", b ? "failure" : "success" );
}
