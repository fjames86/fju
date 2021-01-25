
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
#include <fju/freg.h>
#include <fju/sec.h>
#include <fju/programs.h>
#include <fju/raft.h>
#include <fju/fvm.h>
#include <fju/dmb.h>
#include <fju/dlm.h>

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
static void hrauth_local_results( struct xdr_s *xdr );
static void hrauth_list_results( struct xdr_s *xdr );
static void clt_call( struct clt_info *info, int argc, char **argv, int i );
static void clt_broadcast( struct clt_info *info, int argc, char **argv, int i );
static void freg_list_results( struct xdr_s *xdr );
static void freg_list_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void freg_get_results( struct xdr_s *xdr );
static void freg_get_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void freg_put_results( struct xdr_s *xdr );
static void freg_put_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void freg_rem_results( struct xdr_s *xdr );
static void freg_rem_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void cmdprog_licinfo_results( struct xdr_s *xdr );
static void cmdprog_connlist_results( struct xdr_s *xdr );
static void rawmode_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void rawmode_results( struct xdr_s *xdr );
static void raft_command_results( struct xdr_s *xdr );
static void raft_command_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void raft_snapshot_results( struct xdr_s *xdr );
static void raft_snapshot_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void raft_change_results( struct xdr_s *xdr );
static void raft_change_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void fvm_list_results( struct xdr_s *xdr );
static void fvm_load_results( struct xdr_s *xdr );
static void fvm_load_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void fvm_unload_results( struct xdr_s *xdr );
static void fvm_unload_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void fvm_run_results( struct xdr_s *xdr );
static void fvm_run_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void fvm_clrun_results( struct xdr_s *xdr );
static void fvm_clrun_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void fvm_reload_results( struct xdr_s *xdr );
static void fvm_reload_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void dmb_invoke_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void dmb_publish_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void dmb_publish_results( struct xdr_s *xdr );
static void dlm_list_results( struct xdr_s *xdr );
static void dlm_acquire_args( int argc, char **argv, int i, struct xdr_s *xdr );
static void dlm_acquire_results( struct xdr_s *xdr );
static void dlm_release_args( int argc, char **argv, int i, struct xdr_s *xdr );

static struct clt_info clt_procs[] = {
    { 0, 0, 0, rawmode_args, rawmode_results, "raw", "prog vers proc [u32=*] [u64=*] [str=*] [bool=*] [fixed=*]" },
    { 100000, 2, 0, NULL, NULL, "rpcbind.null", NULL },
    { 100000, 2, 4, NULL, rpcbind_results, "rpcbind.list", NULL },
    { HRAUTH_RPC_PROG, HRAUTH_RPC_VERS, 1, NULL, hrauth_local_results, "hrauth.local", NULL },
    { HRAUTH_RPC_PROG, HRAUTH_RPC_VERS, 2, NULL, hrauth_list_results, "hrauth.list", NULL },
    { FREG_RPC_PROG, FREG_RPC_VERS, 1, freg_list_args, freg_list_results, "freg.list", "parentid=PARENTID" },
    { FREG_RPC_PROG, FREG_RPC_VERS, 2, freg_get_args, freg_get_results, "freg.get", "id=PARENTID" },
    { FREG_RPC_PROG, FREG_RPC_VERS, 3, freg_put_args, freg_put_results, "freg.put", "parentid=PARENTID name=NAME flags=FLAGS [u32=*] [u64=*] [str=*] [opaque-file=*]" },
    { FREG_RPC_PROG, FREG_RPC_VERS, 4, freg_rem_args, freg_rem_results, "freg.rem", "parentid=PARENTID id=ID" },
    { FJUD_RPC_PROG, 1, 1, NULL, NULL, "fjud.stop", NULL },
    { FJUD_RPC_PROG, 1, 3, NULL, cmdprog_licinfo_results, "fjud.licinfo", "" },
    { FJUD_RPC_PROG, 1, 4, NULL, cmdprog_connlist_results, "fjud.connlist", "" },        
    { RAFT_RPC_PROG, 1, 3, raft_command_args, raft_command_results, "raft.command", "clid=* [command=base64]" },
    { RAFT_RPC_PROG, 1, 5, raft_snapshot_args, raft_snapshot_results, "raft.snapshot", "clid=*" },
    { RAFT_RPC_PROG, 1, 6, raft_change_args, raft_change_results, "raft.change", "clid=* [cookie=*] [member=*]* [appid=*] [reset]" },
    { FVM_RPC_PROG, 1, 1, NULL, fvm_list_results, "fvm.list", NULL },
    { FVM_RPC_PROG, 1, 2, fvm_load_args, fvm_load_results, "fvm.load", "filename=* [register] [reload]" },
    { FVM_RPC_PROG, 1, 3, fvm_unload_args, fvm_unload_results, "fvm.unload", "name=*" },
    { FVM_RPC_PROG, 1, 4, fvm_run_args, fvm_run_results, "fvm.run", "modname=* procname=* args=*" },
    { FVM_RPC_PROG, 1, 5, fvm_clrun_args, fvm_clrun_results, "fvm.clrun", "modname=* procname=* args=*" },
    { FVM_RPC_PROG, 1, 6, fvm_reload_args, fvm_reload_results, "fvm.reload", "modname" },
    { DMB_RPC_PROG, 1, 1, dmb_invoke_args, NULL, "dmb.invoke", "msgid=* [seq=*] [flags=*] [buf=base64]" },
    { DMB_RPC_PROG, 1, 3, dmb_publish_args, dmb_publish_results, "dmb.publish", "msgid=* [flags=*] [buf=base64]" },
    { DLM_RPC_PROG, 1, 1, NULL, dlm_list_results, "dlm.list", NULL },
    { DLM_RPC_PROG, 1, 2, dlm_acquire_args, dlm_acquire_results, "dlm.acquire", "resid=* [shared] [cookie=*]" },
    { DLM_RPC_PROG, 1, 3, dlm_release_args, NULL, "dlm.release", "lockid=*" },                
    
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
    int rawmodeb64;
} glob;

int rpc_main( int argc, char **argv ) {
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
    freg_open( NULL, NULL );

    glob.hostid = hostreg_localid();
    glob.port = 8000;
    sts = freg_ensure( NULL, 0, "/fju/rpc/port", FREG_TYPE_UINT32, (char *)&glob.port, sizeof(glob.port), NULL );
    glob.timeout = 1000;
    sts = freg_ensure( NULL, 0, "/fju/rpc/timeout", FREG_TYPE_UINT32, (char *)&glob.timeout, sizeof(glob.timeout), NULL );
    glob.service = HRAUTH_SERVICE_PRIV;
    
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
      uint64_t hid = strtoull( argv[i], &term, 16 );
      if( !*term ) glob.hostid = hid;
      
      if( *term ) {
	struct hostreg_host host;
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
	      glob.hostid = 0;	      
	      i++;
	    } else {
	      //glob.addr = htonl( INADDR_LOOPBACK );	    
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
    if( glob.tcp ) {
      sts = hrauth_call_tcp( &hcall, &args, &res, &opts );
    } else {
      sts = hrauth_call_udp( &hcall, &args, &res, &opts );
    }
  } else if( glob.tcp ) {
    sts = rpc_call_tcp( &pars, &args, &res );
  } else {
    sts = rpc_call_udp( &pars, &args, &res );
  }
  tend = rpc_now();
  if( sts ) usage( "RPC call failed: %s", rpc_errmsg( NULL ) );
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

static char *rpcbind_name_by_prog( uint32_t prog ) {
  static struct freg_entry entry;
  
  int sts;
  uint32_t progid;
  uint64_t id, hkey;

  sts = freg_subkey( NULL, 0, "/fju/rpc/progreg", FREG_CREATE, &hkey );
  if( !sts ) {
    id = 0;
    sts = freg_next( NULL, hkey, id, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_UINT32 ) {
	sts = freg_get( NULL, entry.id, NULL, (char *)&progid, sizeof(progid), NULL );
	if( !sts && progid == prog ) {
	  return entry.name;
	}
      }
      
      id = entry.id;
      sts = freg_next( NULL, hkey, id, &entry );
    }
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

static void hrauth_list_results( struct xdr_s *xdr ) {
    struct hostreg_host x;
    int sts, i, b;

    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) goto bad;
    while( b ) {      
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

      sts = xdr_decode_boolean( xdr, &b );
      if( sts ) goto bad;
    }
    
    return;
bad:
    usage( "XDR error" );
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
    } else if( strcmp( argname, "fixed" ) == 0 ) {
      int sts = base64_decode( (char *)(xdr->buf + xdr->offset), xdr->count - xdr->offset, argval );
      if( sts < 0 ) usage( "Failed to decode fixed base64" );
      xdr->offset += sts;
    } else if( strcmp( argname, "resb64" ) == 0 ) {
      glob.rawmodeb64 = 1;
    } else usage( NULL );
    i++;
  }

}

static void rawmode_results( struct xdr_s *xdr ) {
  int i, count, j, c;

  if( glob.rawmodeb64 ) {
    char *str;
    c = xdr->count - xdr->offset;
    str = malloc( (4*(c / 3)) + 5 );
    base64_encode( (char *)(xdr->buf + xdr->offset), xdr->count - xdr->offset, str );
    printf( "%s\n", str );
    free( str );
  } else {    
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
  
}

static void cmdprog_licinfo_results( struct xdr_s *xdr ) {
  int sts, b;
  uint64_t u64;
  uint32_t u32;
  char str[256];
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "xdr error" );
  if( b ) {
    sts = xdr_decode_uint64( xdr, &u64 );
    if( sts ) usage( "xdr error" );
    printf( "Hostid     %" PRIx64 "\n", u64 );
    sts = xdr_decode_uint64( xdr, &u64 );
    if( sts ) usage( "xdr error" );
    sec_timestr( u64, str );
    printf( "Expiry     %s\n", str );
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) usage( "xdr error" );
    printf( "Version    %u\n", u32 );
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) usage( "xdr error" );
    printf( "Flags      %u\n", u32 );    
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) usage( "xdr error" );    
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) usage( "xdr error" );    
  } else {
    printf( "License check failed" );
  }
}

static void cmdprog_connlist_results( struct xdr_s *xdr ) {
  int sts, b;
  uint64_t connid, rx, tx;
  uint32_t dirtype, type, addr, port, cstate, coffset, ccount;
  char ip[256];

  printf( "%-8s %-4s %-8s %-8s %-8s %-12s %-4s\n", "ConnID", "Dir", "State", "RX", "TX", "Offset/Count", "Type" );
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "xdr error" );
  while( b ) {
    sts = xdr_decode_uint64( xdr, &connid );
    if( !sts ) sts = xdr_decode_uint32( xdr, &dirtype );
    if( !sts ) sts = xdr_decode_uint32( xdr, &cstate );
    if( !sts ) sts = xdr_decode_uint64( xdr, &rx );
    if( !sts ) sts = xdr_decode_uint64( xdr, &tx );
    if( !sts ) sts = xdr_decode_uint32( xdr, &coffset );
    if( !sts ) sts = xdr_decode_uint32( xdr, &ccount );    
    if( !sts ) sts = xdr_decode_uint32( xdr, &type );
    if( sts ) usage( "xdr error" );
    if( type == RPC_LISTEN_TCP ) {
      sts = xdr_decode_uint32( xdr, &addr );
      if( !sts ) sts = xdr_decode_uint32( xdr, &port );
    }
    if( sts ) usage( "xdr error" );
    printf( "%-8"PRIu64" %-4s %-8s %-8"PRIu64" %-8"PRIu64" %4u/%4u %-4s",
	    connid,
	    dirtype == RPC_CONN_DIR_INCOMING ? "IN" : "OUT",
	    cstate == RPC_CSTATE_RECVLEN ? "RecvLen" :
	    cstate == RPC_CSTATE_RECV ? "Recv" :
	    cstate == RPC_CSTATE_SENDLEN ? "SendLen" :
	    cstate == RPC_CSTATE_SEND ? "Send" :
	    cstate == RPC_CSTATE_CONNECT ? "Connect" :
	    cstate == RPC_CSTATE_CLOSE ? "Close" :
	    "Other",
	    rx, tx, coffset, ccount,
	    type == RPC_LISTEN_TCP ? "TCP" :
	    type == RPC_LISTEN_UNIX ? "UNIX" : 
	    "Other" );
    if( type == RPC_LISTEN_TCP ) {
      strcpy( ip, "" );
      mynet_ntop( addr, ip );
      sprintf( ip + strlen( ip ), ":%u", port );
      printf( "%s", ip );
    }
    printf( "\n" );
	    

    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) usage( "xdr error" );    
  }

}

static void raft_command_results( struct xdr_s *xdr ) {
  int sts, b;
  uint64_t cseq;
  sts = xdr_decode_boolean( xdr, &b );
  if( !sts ) sts = xdr_decode_uint64( xdr, &cseq );
  if( sts ) usage( "XDR error" );
  if( !b ) printf( "Failure\n" );
  else printf( "Success cseq=%"PRIu64"\n", cseq );
}

static void raft_command_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  char *term;
  uint64_t clid = 0;
  char *cmdstr = NULL;
  int sts;
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "clid" ) == 0 ) {
      clid = strtoull( argval, &term, 16 );
      if( *term ) usage( "Failed to parse CLID" );
    } else if( strcmp( argname, "command" ) == 0 ) {
      cmdstr = argval;
    } else usage( NULL );
    i++;
  }

  if( !clid ) usage( "Need CLID" );
  xdr_encode_uint64( xdr, clid );
  if( cmdstr ) {
    sts = base64_decode( (char *)(xdr->buf + xdr->offset + 4), (xdr->count - xdr->offset) - 4, cmdstr );
    if( sts < 0 ) usage( "Failed to decode base64 command buffer" );
    xdr_encode_uint32( xdr, sts );
    xdr->offset += sts;
    if( sts % 4 ) xdr->offset += 4 - (sts % 4);
  } else {
    xdr_encode_opaque( xdr, NULL, 0 );
  }
}

static void raft_snapshot_results( struct xdr_s *xdr ) {
  int sts, b;
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  
  if( !b ) printf( "Failure\n" );
  else printf( "Success\n" );
}

static void raft_snapshot_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  char *term;
  uint64_t clid = 0;
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "clid" ) == 0 ) {
      clid = strtoull( argval, &term, 16 );
      if( *term ) usage( "Failed to parse CLID" );
    } else usage( NULL );
    i++;
  }

  if( !clid ) usage( "Need CLID" );
  xdr_encode_uint64( xdr, clid );
}

static void raft_change_results( struct xdr_s *xdr ) {
}

static void raft_change_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;
  char *term;  
  uint64_t clid;
  int bcookie, bmembers, bappid, breset;
  char cookie[RAFT_MAX_COOKIE];
  uint32_t nmember, appid;
  uint64_t members[RAFT_MAX_MEMBER];
  
  bcookie = 0;
  bmembers = 0;
  clid = 0;
  memset( cookie, 0, sizeof(cookie) );
  nmember = 0;
  bappid = 0;
  appid = 0;
  breset = 0;
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "clid" ) == 0 ) {
      clid = strtoull( argval, &term, 16 );
      if( *term ) usage( "Failed to parse CLID" );
    } else if( strcmp( argname, "cookie" ) == 0 ) {
      strncpy( cookie, argval, RAFT_MAX_COOKIE - 1 );
      bcookie = 1;
    } else if( strcmp( argname, "member" ) == 0 ) {
      if( nmember >= RAFT_MAX_MEMBER ) usage( "Max members" );
      members[nmember] = strtoull( argval, &term, 16 );
      if( *term ) usage( "Failed to parse hostid" );
      nmember++;
      bmembers = 1;
    } else if( strcmp( argname, "appid" ) == 0 ) {
      appid = strtol( argval, NULL, 10 );
      bappid = 1;
    } else if( strcmp( argname, "reset" ) == 0 ) {
      breset = 1;
    } else usage( NULL );
    
    i++;
  }

  if( !clid ) usage( "Need CLID" );

  xdr_encode_uint64( xdr, clid );
  xdr_encode_boolean( xdr, bcookie );
  if( bcookie ) xdr_encode_fixed( xdr, (uint8_t *)cookie, RAFT_MAX_COOKIE );
  xdr_encode_boolean( xdr, bmembers );
  if( bmembers ) {
    xdr_encode_uint32( xdr, nmember );
    for( i = 0; i < nmember; i++ ) xdr_encode_uint64( xdr, members[i] );
  }
  xdr_encode_boolean( xdr, bappid );
  if( bappid ) xdr_encode_uint32( xdr, appid );
  xdr_encode_boolean( xdr, breset );
}


static void fvm_list_results( struct xdr_s *xdr ) {
  int sts, b, i;
  char name[64];
  uint32_t progid, versid, datasize, textsize, nprocs, address;
  uint64_t siginfo, timestamp, nsteps, rcount;
  int vartype, isvar, nargs, j;
  char timestr[64];
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  while( b ) {
    xdr_decode_string( xdr, name, sizeof(name) );
    xdr_decode_uint32( xdr, &progid );
    xdr_decode_uint32( xdr, &versid );
    xdr_decode_uint32( xdr, &datasize );
    xdr_decode_uint32( xdr, &textsize );
    xdr_decode_uint64( xdr, &timestamp );
    xdr_decode_uint32( xdr, &nprocs );
    printf( "%s %u:%u Data=%u Text=%u Timestamp=%s\n", name, progid, versid, datasize, textsize, sec_timestr( timestamp, timestr ) );    
    for( i = 0; i < nprocs; i++ ) {
      xdr_decode_string( xdr, name, sizeof(name) ); 
      xdr_decode_uint32( xdr, &address );     
      xdr_decode_uint64( xdr, &siginfo );
      xdr_decode_uint64( xdr, &nsteps );
      xdr_decode_uint64( xdr, &rcount );      
      
      printf( "    [%d] %s(", i, name );
      nargs = FVM_SIGINFO_NARGS(siginfo);
      for( j = 0; j < nargs; j++ ) {
	isvar = FVM_SIGINFO_ISVAR(siginfo,j);
	vartype = FVM_SIGINFO_VARTYPE(siginfo,j);
	printf( "%s%s%s", j ? ", " : "", isvar ? "var " : "",
		vartype == 0 ? "Int" :
		vartype == 1 ? "String" :
		vartype == 2 ? "Opaque" : 
		"Other" );
      }
      printf( ") RCount=%"PRIu64" NSteps=%"PRIu64"\n", rcount, nsteps );
    }
    printf( "\n" );
    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) usage( "XDR error" );
  }
}

static void fvm_load_results( struct xdr_s *xdr ) {
  int sts, b;
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  printf( "%s\n", b ? "Success" : "Failure" );
}

static void fvm_load_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;  
  char *filename;
  int sts;
  int registerp;
  struct mmf_s mmf;
  uint32_t flags;
  
  filename = NULL;
  registerp = 0;
  flags = 0;
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "filename" ) == 0 ) {
      filename = argval;
    } else if( strcmp( argname, "register" ) == 0 ) {
      registerp = 1;
    } else if( strcmp( argname, "reload" ) == 0 ) {
      flags |= FVM_RELOAD;
    } else usage( NULL );
    
    i++;
  }

  if( !filename ) usage( "Need filename" );

  sts = mmf_open2( filename, &mmf, MMF_OPEN_EXISTING );
  if( sts ) usage( "Failed to open file" );

  mmf_remap( &mmf, mmf.fsize );
  xdr_encode_opaque( xdr, mmf.file, mmf.fsize );
  xdr_encode_uint32( xdr, flags );
  xdr_encode_boolean( xdr, registerp );
  mmf_close( &mmf );
}

static void fvm_unload_results( struct xdr_s *xdr ) {
  int sts, b;
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  printf( "%s\n", b ? "Success" : "Failure" );
}

static void fvm_unload_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;  
  char *modname;

  modname = NULL;
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "name" ) == 0 ) {
      modname = argval;
    } else usage( NULL );
    
    i++;
  }

  if( !modname ) usage( "Need module name" );

  xdr_encode_string( xdr, modname );
}

static void fvm_run_results( struct xdr_s *xdr ) {
  int sts, b;
  uint8_t *bufp;
  int lenp;
  char *str;
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  printf( "%s\n", b ? "Success" : "Failure" );
  sts = xdr_decode_opaque_ref( xdr, &bufp, &lenp );
  if( sts ) usage( "XDR error" );

  if( b ) {
    str = malloc( lenp * 2 );
    base64_encode( (char *)bufp, lenp, str );
    printf( "%s\n", str );
  }
}

static void fvm_run_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;  
  char *modname, *procname;
  char *buf;
  int len, sts;
  
  modname = NULL;
  buf = NULL;
  len = 0;
  procname = NULL;

  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "modname" ) == 0 ) {
      modname = argval;
    } else if( strcmp( argname, "procname" ) == 0 ) {
      procname = argval;
    } else if( strcmp( argname, "args" ) == 0 ) {
      buf = malloc( 32*1024 );
      len = 32*1024;
      sts = base64_decode( buf, len, argval );
      if( sts < 0 ) usage( "Base64 decode error" );
      len = sts;
    } else usage( NULL );
    
    i++;
  }

  if( !modname ) usage( "Need module name" );
  if( !procname ) usage( "Need proc name" );
  
  xdr_encode_string( xdr, modname );
  xdr_encode_string( xdr, procname );
  xdr_encode_opaque( xdr, (uint8_t *)buf, len );
}


static void fvm_clrun_results( struct xdr_s *xdr ) {
  int sts, b;
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  printf( "%s\n", b ? "Success" : "Failure" );
}

static void fvm_clrun_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;  
  char *modname, *procname;
  char *buf;
  int len, sts;
  uint64_t clid;
  
  modname = NULL;
  buf = NULL;
  len = 0;
  clid = 0;
  procname = NULL;

  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "modname" ) == 0 ) {
      modname = argval;
    } else if( strcmp( argname, "procname" ) == 0 ) {
      procname = argval;
    } else if( strcmp( argname, "clid" ) == 0 ) {
      clid = strtoull( argval, NULL, 16 );
    } else if( strcmp( argname, "args" ) == 0 ) {
      buf = malloc( 32*1024 );
      len = 32*1024;
      sts = base64_decode( buf, len, argval );
      if( sts < 0 ) usage( "Base64 decode error" );
      len = sts;
    } else usage( NULL );
    
    i++;
  }

  if( !modname ) usage( "Need module name" );
  if( !procname ) usage( "Need proc name" );

  xdr_encode_uint64( xdr, clid );
  xdr_encode_string( xdr, modname );
  xdr_encode_string( xdr, procname );
  xdr_encode_opaque( xdr, (uint8_t *)buf, len );
}


static void fvm_reload_results( struct xdr_s *xdr ) {
  int sts, b;
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  printf( "%s\n", b ? "Success" : "Failure" );
}

static void fvm_reload_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char *modname;

  if( i >= argc ) usage( "Need modname" );
  modname = argv[i];
  xdr_encode_string( xdr, modname );
}

static void dmb_invoke_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;  
  uint32_t msgid, flags;
  char *buf;
  int len, sts;
  uint64_t seq;
  
  msgid = 0;
  flags = 0;
  buf = NULL;
  len = 0;
  seq = 0;
  
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "msgid" ) == 0 ) {
      msgid = strtoul( argval, NULL, 0 );
    } else if( strcmp( argname, "flags" ) == 0 ) {
      flags = strtoul( argval, NULL, 0 );
    } else if( strcmp( argname, "seq" ) == 0 ) {
      seq = strtoull( argval, NULL, 0 );      
    } else if( strcmp( argname, "buf" ) == 0 ) {
      buf = malloc( DMB_MAX_MSG );
      len = DMB_MAX_MSG;
      sts = base64_decode( buf, len, argval );
      if( sts < 0 ) usage( "Base64 decode error" );
      len = sts;
    } else usage( NULL );
    
    i++;
  }

  if( !msgid ) usage( "Need msgid" );

  xdr_encode_uint64( xdr, hostreg_localid() );
  xdr_encode_uint64( xdr, seq );  
  xdr_encode_uint32( xdr, msgid );
  xdr_encode_uint32( xdr, flags );
  xdr_encode_opaque( xdr, (uint8_t *)buf, len );
}

static void dmb_publish_results( struct xdr_s *xdr ) {
  int sts;
  uint64_t seq;
  
  sts = xdr_decode_uint64( xdr, &seq );
  if( sts ) usage( "XDR error" );
  printf( "%"PRIu64"\n", seq );
}

static void dmb_publish_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;  
  uint32_t msgid, flags;
  char *buf;
  int len, sts;

  msgid = 0;
  flags = 0;
  buf = NULL;
  len = 0;

  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "msgid" ) == 0 ) {
      msgid = strtoul( argval, NULL, 0 );
    } else if( strcmp( argname, "flags" ) == 0 ) {
      flags = strtoul( argval, NULL, 0 );
    } else if( strcmp( argname, "buf" ) == 0 ) {
      buf = malloc( DMB_MAX_MSG );
      len = DMB_MAX_MSG;
      sts = base64_decode( buf, len, argval );
      if( sts < 0 ) usage( "Base64 decode error" );
      len = sts;
    } else usage( NULL );
    
    i++;
  }

  if( !msgid ) usage( "Need msgid" );

  xdr_encode_uint32( xdr, msgid );
  xdr_encode_uint32( xdr, flags );
  xdr_encode_opaque( xdr, (uint8_t *)buf, len );
}

static void dlm_list_results( struct xdr_s *xdr ) {
  int sts, b, i;
  uint64_t lockid, resid, hostid;
  uint32_t state;
  char cookie[DLM_MAX_COOKIE];

  printf( "%-16s %-16s %-16s %-10s %s\n", "LockID", "HostID", "ResID", "State", "Cookie" );
  
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) usage( "XDR error" );
  while( b ) {
    sts = xdr_decode_uint64( xdr, &lockid );
    if( !sts ) sts = xdr_decode_uint64( xdr, &hostid );
    if( !sts ) sts = xdr_decode_uint64( xdr, &resid );    
    if( !sts ) sts = xdr_decode_uint32( xdr, &state );
    if( !sts ) sts = xdr_decode_fixed( xdr, (uint8_t *)cookie, DLM_MAX_COOKIE );
    if( sts ) usage( "XDR error" );

    printf( "%-16"PRIx64" %-16"PRIx64" %-16"PRIx64" %-10s ", lockid, hostid, resid,
	    state == DLM_EX ? "Exclusive" :
	    state == DLM_SH ? "Shared" :
	    state == DLM_BLOCKEX ? "BlockedEX" :
	    state == DLM_BLOCKSH ? "BlockedSH" :	    
	    "Other" );
    for( i = 0; i < DLM_MAX_COOKIE; i++ ) {
      printf( "%02x", (uint32_t)(uint8_t)cookie[i] );
    }
    printf( "\n" );
    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) usage( "XDR error" );
  }
}

static void dlm_acquire_results( struct xdr_s *xdr ) {
  int sts;
  uint64_t lockid;
  sts = xdr_decode_uint64( xdr, &lockid );
  if( sts ) usage( "XDR error" );
  printf( "%"PRIx64"\n", lockid );
}

static void dlm_acquire_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;  
  uint64_t resid;
  int shared;
  char cookie[DLM_MAX_COOKIE];

  resid = 0;
  shared = 0;
  memset( cookie, 0, sizeof(cookie) );
    
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "resid" ) == 0 ) {
      resid = strtoull( argval, NULL, 16 );
    } else if( strcmp( argname, "shared" ) == 0 ) {
      shared = 1;
    } else if( strcmp( argname, "exclusive" ) == 0 ) {
      shared = 0;
    } else if( strcmp( argname, "cookie" ) == 0 ) {
      strncpy( cookie, argval, DLM_MAX_COOKIE );
    } else usage( NULL );
    
    i++;
  }

  xdr_encode_uint64( xdr, resid );
  xdr_encode_boolean( xdr, shared );
  xdr_encode_fixed( xdr, (uint8_t *)cookie, DLM_MAX_COOKIE );
}

static void dlm_release_args( int argc, char **argv, int i, struct xdr_s *xdr ) {
  char argname[64], *argval;  
  uint64_t lockid;

  lockid = 0;
    
  while( i < argc ) {
    argval_split( argv[i], argname, &argval );
    if( strcmp( argname, "lockid" ) == 0 ) {
      lockid = strtoull( argval, NULL, 16 );
    } else usage( NULL );
    
    i++;
  }

  xdr_encode_uint64( xdr, lockid );
}
