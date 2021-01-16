
#include "fjui.h"

void fjui_net_service( void ) {
	/* poll networking */
	rpc_service( 0 );
}

static struct fjui_hostinfo *hostinfo;

struct fjui_hostinfo *fjui_hostinfo_by_name( char *name ) {
	struct fjui_hostinfo *h;
	h = hostinfo;
	while( h ) {
		if( strcasecmp( h->name, name ) == 0 ) return h;
		h = h->next;
	}
	return NULL;
}

struct fjui_hostinfo *fjui_hostinfo_by_id( uint64_t hostid ) {
	struct fjui_hostinfo *h;
	h = hostinfo;
	while( h ) {
		if( h->hostid == hostid ) return h;
		h = h->next;
	}
	return NULL;
}

struct fjui_hostinfo *fjui_hostinfo_add( uint64_t hostid ) {
	struct fjui_hostinfo *h;

	h = fjui_hostinfo_by_id( hostid );
	if( h ) return h;

	h = malloc( sizeof(*h) );
	memset( h, 0, sizeof(*h) );
	h->hostid = hostid;
	hostreg_name_by_hostid( hostid, h->name );
	h->next = hostinfo;
	hostinfo = h;
	return h;
}

static void getlicinfo_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	int sts;
	char timestr[64];
	uint64_t hostid;
	struct fjui_hostinfo *info;
	int b;

	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "%s Timeout", info->name );
		return;
	}
	fjui_set_statusbar( 1, "%s GetLicInfo Success ", info->name, sec_timestr( time( NULL ), timestr ) );

	xdr_decode_boolean( xdr, &b );
	if( b ) {
		xdr_decode_uint64( xdr, &info->lic.hostid );
		xdr_decode_uint64( xdr, &info->lic.expire );
		xdr_decode_uint32( xdr, &info->lic.version );
		xdr_decode_uint32( xdr, &info->lic.flags );
		xdr_decode_uint32( xdr, &info->lic.spare[0] );
		xdr_decode_uint32( xdr, &info->lic.spare[1] );        
    } else {
		memset( &info->lic, 0, sizeof(info->lic) );
	}

	/* notify summary page that the info has been retreived */
	fjui_summary_setinfo( info );
}

/* rpc calls for information */
void fjui_call_getlicinfo( uint64_t hostid ) {
	struct hrauth_call hcall;
	int sts;

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FJUD_RPC_PROG;
  hcall.vers = FJUD_RPC_VERS;
  hcall.proc = 3; /* nullping proc */
  hcall.donecb = getlicinfo_cb;
  hcall.cxt = NULL;
  hcall.timeout = 200;
  hcall.service = HRAUTH_SERVICE_PRIV;

  sts = hrauth_call_tcp_async( &hcall, NULL, 0 );
  if( sts ) {
    //hrauth_log( LOG_LVL_ERROR, "fjui_call_getlicinfo failed hostid=%"PRIx64"", hostid );
    fjui_set_statusbar( 1, "GetLicInfo failed" );
  }
}


static void connlist_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	int sts;
	char timestr[64];
	uint64_t hostid;
	struct fjui_hostinfo *info;
	int b;
	uint32_t addr, port;

	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "%s Timeout", info->name );
		return;
	}
	fjui_set_statusbar( 1, "%s ConnList Success ", info->name, sec_timestr( time( NULL ), timestr ) );


	info->nconn = 0;
	sts = xdr_decode_boolean( xdr, &b );
	if( sts ) return;
	while( b ) {
		sts = xdr_decode_uint64( xdr, &info->conn[info->nconn].connid );
		if( !sts ) sts = xdr_decode_uint32( xdr, &info->conn[info->nconn].dirtype );
		if( !sts ) sts = xdr_decode_uint32( xdr, &info->conn[info->nconn].cstate );
		if( !sts ) sts = xdr_decode_uint64( xdr, &info->conn[info->nconn].rx );
		if( !sts ) sts = xdr_decode_uint64( xdr, &info->conn[info->nconn].tx );
		if( !sts ) sts = xdr_decode_uint32( xdr, &info->conn[info->nconn].coffset );
		if( !sts ) sts = xdr_decode_uint32( xdr, &info->conn[info->nconn].ccount );    
		if( !sts ) sts = xdr_decode_uint32( xdr, &info->conn[info->nconn].type );
		if( sts ) return;
		if( info->conn[info->nconn].type == RPC_LISTEN_TCP ) {
			sts = xdr_decode_uint32( xdr, &addr );
			if( !sts ) sts = xdr_decode_uint32( xdr, &port );
			info->conn[info->nconn].sin.sin_addr.s_addr = htonl( addr );
			info->conn[info->nconn].sin.sin_port = htons( port );
		}
		if( sts ) return;
		
		info->nconn++;
		if( info->nconn >= 16 ) break;

		sts = xdr_decode_boolean( xdr, &b );
		if( sts ) return;
	}

	/* notify summary page that the info has been retreived */
	fjui_summary_setinfo( info );
}


void fjui_call_connlist( uint64_t hostid ) {
	struct hrauth_call hcall;
	int sts;

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FJUD_RPC_PROG;
  hcall.vers = FJUD_RPC_VERS;
  hcall.proc = 4; 
  hcall.donecb = connlist_cb;
  hcall.cxt = NULL;
  hcall.timeout = 200;
  hcall.service = HRAUTH_SERVICE_PRIV;

  sts = hrauth_call_tcp_async( &hcall, NULL, 0 );
  if( sts ) {
    fjui_set_statusbar( 1, "ConnList failed" );    
    //hrauth_log( LOG_LVL_ERROR, "fjui_call_getlicinfo failed hostid=%"PRIx64"", hostid );
  }
}



static void fvmlist_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	int sts;
	char timestr[64];
	uint64_t hostid;
	struct fjui_hostinfo *info;
	int i, b;
	uint32_t addr, port;

	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "%s Timeout", info->name );
		return;
	}
	fjui_set_statusbar( 1, "%s FvmList Success ", info->name, sec_timestr( time( NULL ), timestr ) );


	info->nmodule = 0;
	sts = xdr_decode_boolean( xdr, &b );
	if( sts ) return;
	while( b ) {
		xdr_decode_string( xdr, info->modules[info->nmodule].name, sizeof(info->modules[info->nmodule].name) );
		xdr_decode_uint32( xdr, &info->modules[info->nmodule].progid );
		xdr_decode_uint32( xdr, &info->modules[info->nmodule].versid );
		xdr_decode_uint32( xdr, &info->modules[info->nmodule].datasize );
		xdr_decode_uint32( xdr, &info->modules[info->nmodule].textsize );
		xdr_decode_uint64( xdr, &info->modules[info->nmodule].timestamp );
		xdr_decode_uint32( xdr, &info->modules[info->nmodule].nprocs );
    
	    for( i = 0; i < info->modules[info->nmodule].nprocs; i++ ) {
			xdr_decode_string( xdr, info->modules[info->nmodule].procs[i].name, sizeof(info->modules[info->nmodule].procs[i].name) ); 
			xdr_decode_uint32( xdr, &info->modules[info->nmodule].procs[i].address );     
			xdr_decode_uint64( xdr, &info->modules[info->nmodule].procs[i].siginfo );
		}

		info->nmodule++;
		if( info->nmodule >= 32 ) break;

		sts = xdr_decode_boolean( xdr, &b );
		if( sts ) return;
	}

	fjui_fvm_setinfo( info );
}

void fjui_call_fvmlist( uint64_t hostid ) {
	struct hrauth_call hcall;
	int sts;

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FVM_RPC_PROG;
  hcall.vers = FVM_RPC_VERS;
  hcall.proc = 1; 
  hcall.donecb = fvmlist_cb;
  hcall.cxt = NULL;
  hcall.timeout = 200;
  hcall.service = HRAUTH_SERVICE_PRIV;

  sts = hrauth_call_tcp_async( &hcall, NULL, 0 );
  if( sts ) {
    fjui_set_statusbar( 1, "FvmList failed" );    
    //hrauth_log( LOG_LVL_ERROR, "fjui_call_getlicinfo failed hostid=%"PRIx64"", hostid );
  }
}
 



static void rpcbindlist_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	int sts;
	char timestr[64];
	uint64_t hostid;
	struct fjui_hostinfo *info;
	int i, b;
	uint32_t addr, port;

	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "%s Timeout", info->name );
		return;
	}
	fjui_set_statusbar( 1, "%s RpcBindList Success ", info->name, sec_timestr( time( NULL ), timestr ) );


	info->nrpcbind = 0;
	sts = xdr_decode_boolean( xdr, &b );
	if( sts ) return;
	while( b ) {
		xdr_decode_uint32( xdr, &info->rpcbind[info->nrpcbind].prog );
		xdr_decode_uint32( xdr, &info->rpcbind[info->nrpcbind].vers );
		xdr_decode_uint32( xdr, &info->rpcbind[info->nrpcbind].prot );
		xdr_decode_uint32( xdr, &info->rpcbind[info->nrpcbind].port );

		info->nrpcbind++;
		if( info->nrpcbind >= 64 ) break;

		sts = xdr_decode_boolean( xdr, &b );
		if( sts ) return;
	}

	fjui_summary_setinfo( info );
}

void fjui_call_rpcbindlist( uint64_t hostid ) {
	struct hrauth_call hcall;
	int sts;

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = 100000;
  hcall.vers = 2;
  hcall.proc = 4; 
  hcall.donecb = rpcbindlist_cb;
  hcall.cxt = NULL;
  hcall.timeout = 200;
  hcall.service = HRAUTH_SERVICE_PRIV;

  sts = hrauth_call_tcp_async( &hcall, NULL, 0 );
  if( sts ) {
    fjui_set_statusbar( 1, "RpcBindList failed" );    
    //hrauth_log( LOG_LVL_ERROR, "fjui_call_getlicinfo failed hostid=%"PRIx64"", hostid );
  }
}



static void raftlist_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	int sts;
	char timestr[64];
	uint64_t hostid;
	struct fjui_hostinfo *info;
	int i, b, j;
	uint32_t addr, port;


	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "%s Timeout", info->name );
		return;
	}
	fjui_set_statusbar( 1, "%s RaftList Success ", info->name, sec_timestr( time( NULL ), timestr ) );


	info->nraft = 0;
	sts = xdr_decode_boolean( xdr, &b );
	if( sts ) return;
	while( b ) {
		
		xdr_decode_uint64( xdr, &info->raft[info->nraft].clid );
		xdr_decode_uint64( xdr, &info->raft[info->nraft].leaderid );
		xdr_decode_uint64( xdr, &info->raft[info->nraft].voteid );
		xdr_decode_uint64( xdr, &info->raft[info->nraft].term );
		xdr_decode_uint64( xdr, &info->raft[info->nraft].appliedseq );
		xdr_decode_uint64( xdr, &info->raft[info->nraft].commitseq );
		xdr_decode_uint32( xdr, &info->raft[info->nraft].state );
		xdr_decode_uint32( xdr, &info->raft[info->nraft].appid );
		xdr_decode_uint32( xdr, &info->raft[info->nraft].flags );
		xdr_decode_fixed( xdr, info->raft[info->nraft].cookie, RAFT_MAX_COOKIE );
		xdr_decode_uint32( xdr, &info->raft[info->nraft].nmember );
		for( j = 0; j < info->raft[info->nraft].nmember; j++ ) {
			xdr_decode_uint64( xdr, &info->raft[info->nraft].member[j].hostid );
			xdr_decode_uint64( xdr, &info->raft[info->nraft].member[j].lastseen );
			xdr_decode_uint64( xdr, &info->raft[info->nraft].member[j].storedseq );
			xdr_decode_uint32( xdr, &info->raft[info->nraft].member[j].flags );
		}

		info->nraft++;
		if( info->nraft >= 32 ) break;

		sts = xdr_decode_boolean( xdr, &b );
		if( sts ) return;
	}

	fjui_raft_setinfo( info );
}

void fjui_call_raftlist( uint64_t hostid ) {
	struct hrauth_call hcall;
	int sts;

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT_RPC_PROG;
  hcall.vers = RAFT_RPC_VERS;
  hcall.proc = 7; 
  hcall.donecb = raftlist_cb;
  hcall.cxt = NULL;
  hcall.timeout = 200;
  hcall.service = HRAUTH_SERVICE_PRIV;

  sts = hrauth_call_tcp_async( &hcall, NULL, 0 );
  if( sts ) {
    fjui_set_statusbar( 1, "RaftList failed" );    
    //hrauth_log( LOG_LVL_ERROR, "fjui_call_getlicinfo failed hostid=%"PRIx64"", hostid );
  }
}

static void fvmrun_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	int sts;
	int b;

	if( !xdr ) return;
	fjui_set_statusbar( 1, "FvmRun Success " );

	sts = xdr_decode_boolean( xdr, &b );

	fjui_fvm_setcallres( sts || !b ? NULL : xdr );
}

void fjui_call_fvmrun( uint64_t hostid, char *modname, char *procname, struct xdr_s *args ) {
	struct hrauth_call hcall;
	int sts;
	struct xdr_s args2[2];
	char bb[256];

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FVM_RPC_PROG;
  hcall.vers = FVM_RPC_VERS;
  hcall.proc = 4; 
  hcall.donecb = fvmrun_cb;
  hcall.cxt = NULL;
  hcall.timeout = 1000;
  hcall.service = HRAUTH_SERVICE_PRIV;

  xdr_init( &args2[0], bb, sizeof(bb) );
  xdr_encode_string( &args2[0], modname );
  xdr_encode_string( &args2[0], procname );
  xdr_encode_uint32( &args2[0], args->offset );
  xdr_init( &args2[1], args->buf, args->offset );
  args2[1].offset = args->offset;
  sts = hrauth_call_tcp_async( &hcall, args2, 2 );
  if( sts ) {
    fjui_set_statusbar( 1, "FvmRun failed" );
  }
}

static void logread_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	/* results: nentries,opaque array of entries */
	/* each entry is msgid(uint64), flags (uint32) timestamp(u64) data (opaque) */
	int i, len, sts;
	char *bufp;
	uint32_t nentry, flags;
	uint64_t msgid, timestamp;
	struct xdr_s xx;

	

	if( !xdr ) {
		fjui_set_statusbar( 1, "LogRead Timeout" );
		return;
	}
	fjui_set_statusbar( 1, "LogRead Success " );

	sts = xdr_decode_uint32( xdr, &nentry );
	sts = xdr_decode_opaque_ref( xdr, &bufp, &len );
	xdr_init( &xx, bufp, len );
	for( i = 0; i < nentry; i++ ) {
		sts = xdr_decode_uint64( &xx, &msgid );
		if( !sts ) sts = xdr_decode_uint32( &xx, &flags );
		if( !sts ) sts = xdr_decode_uint64( &xx, &timestamp );
		if( !sts ) sts = xdr_decode_opaque_ref( &xx, &bufp, &len );
		if( sts ) return;

		if(fjui_log_addentry( hcallp->hostid, msgid, flags, timestamp, bufp, len, i )) return;
	}

	if( nentry > 0 ) {
		//fjui_call_logread( hcallp->hostid, msgid );
	}
}

void fjui_call_logread( uint64_t hostid, char *name, uint64_t lastid ) {
	struct hrauth_call hcall;
	int sts;
	struct xdr_s args[1];
	char bb[256];

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = LOG_RPC_PROG;
  hcall.vers = LOG_RPC_VERS;
  hcall.proc = 4; 
  hcall.donecb = logread_cb;
  hcall.cxt = NULL;
  hcall.cxt2 = lastid;
  hcall.timeout = 1000;
  hcall.service = HRAUTH_SERVICE_PRIV;

  xdr_init( &args[0], bb, sizeof(bb) );
  xdr_encode_string( &args[0], name ? name : "fju" );
  xdr_encode_uint64( &args[0], lastid );
  xdr_encode_uint32( &args[0], 256 );
  sts = hrauth_call_tcp_async( &hcall, args, 1 );
  if( sts ) {
    fjui_set_statusbar( 1, "LogRead failed" );
  }
}


static void reglist_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	int sts, b;
	uint64_t id, u64;
	char name[256];
	char str[1024];
	uint32_t flags, u32;
	char *bufp;
	int len;
	HTREEITEM hparent;

	hparent = (HTREEITEM)hcallp->cxt2;

	if( !xdr ) {
		fjui_set_statusbar( 1, "RegList Timeout " );
		return;
	}
	fjui_set_statusbar( 1, "RegList Success " );

	//reg_deletechildren( hparent );

	sts = xdr_decode_boolean( xdr, &b );
	if( sts ) return;
	while( b ) {

		sts = xdr_decode_uint64( xdr, &id );
		if( !sts ) xdr_decode_string( xdr, name, sizeof(name) );
		if( !sts ) xdr_decode_uint32( xdr, &flags );
		if( sts ) return;

		bufp = NULL;
		len = 0;

		switch( flags & FREG_TYPE_MASK ) {
		case FREG_TYPE_UINT32:
			sts = xdr_decode_uint32( xdr, &u32 );
			if( sts ) return;
			bufp = (char *)&u32;
			len = sizeof(u32);
			break;
		case FREG_TYPE_UINT64:
			sts = xdr_decode_uint64( xdr, &u64 );	
			if( sts ) return;
			bufp = (char *)&u64;
			len = sizeof(u64);
			break;
		case FREG_TYPE_KEY:
			bufp = NULL;
			len = 0;
			break;
		case FREG_TYPE_STRING:
			sts = xdr_decode_string( xdr, str, sizeof(str) );
			if( sts ) return;
			bufp = str;
			len = strlen( str ) + 1;
			break;
		case FREG_TYPE_OPAQUE:
			sts = xdr_decode_opaque_ref( xdr, (uint8_t *)&bufp, &len );
			if( sts ) return;
			break;
		}

		reg_additem( name, id, flags, bufp, len, hparent );

		sts = xdr_decode_boolean( xdr, &b );
		if( sts ) return;
	}


	TreeView_Expand( fjui_get_hwnd( "reg_tv" ), hparent, TVE_EXPAND|TVE_EXPANDPARTIAL );
}

void fjui_call_reglist( uint64_t hostid, uint64_t hitem, HTREEITEM hparent ) {
	struct hrauth_call hcall;
	int sts;
	struct xdr_s args[1];
	char bb[256];

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FREG_RPC_PROG;
  hcall.vers = FREG_RPC_VERS;
  hcall.proc = 1; 
  hcall.donecb = reglist_cb;
  hcall.cxt = NULL;
  hcall.cxt2 = hparent;
  hcall.timeout = 1000;
  hcall.service = HRAUTH_SERVICE_PRIV;

  xdr_init( &args[0], bb, sizeof(bb) );
  xdr_encode_uint64( &args[0], hitem );
  sts = hrauth_call_tcp_async( &hcall, args, 1 );
  if( sts ) {
    fjui_set_statusbar( 1, "RegList failed" );
  }
}


static void regrem_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	fjui_set_statusbar( 1, "RegRem %s", xdr ? "Success" : "Timeout" );
	fjui_reg_refresh_selected( hcallp->hostid );
}

void fjui_call_regrem( uint64_t hostid, uint64_t parentid, uint64_t itemid ) {
	struct hrauth_call hcall;
	int sts;
	struct xdr_s args[1];
	char bb[256];

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FREG_RPC_PROG;
  hcall.vers = FREG_RPC_VERS;
  hcall.proc = 4; 
  hcall.donecb = regrem_cb;
  hcall.cxt = NULL;
  hcall.cxt2 = parentid;
  hcall.timeout = 1000;
  hcall.service = HRAUTH_SERVICE_PRIV;

  xdr_init( &args[0], bb, sizeof(bb) );
  xdr_encode_uint64( &args[0], parentid );
  xdr_encode_uint64( &args[0], itemid );
  sts = hrauth_call_tcp_async( &hcall, args, 1 );
  if( sts ) {
    fjui_set_statusbar( 1, "RegRem failed" );
  }
}

static void regput_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {
	fjui_set_statusbar( 1, "RegPut %s", xdr ? "Succes" : "Timeout" );
	if( xdr ) {
	  fjui_reg_refresh_selected( hcallp->hostid );
	}
}

void fjui_call_regput( uint64_t hostid, uint64_t parentid, char *name, uint32_t flags, char *buf, int len ) {
	struct hrauth_call hcall;
	int sts;
	struct xdr_s args[2];
	char bb[256];

  //hrauth_log( LOG_LVL_TRACE, "fjui_call_getlicinfo %"PRIx64"", hostid );
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FREG_RPC_PROG;
  hcall.vers = FREG_RPC_VERS;
  hcall.proc = 3; 
  hcall.donecb = regput_cb;
  hcall.cxt = NULL;
  hcall.cxt2 = parentid;
  hcall.timeout = 1000;
  hcall.service = HRAUTH_SERVICE_PRIV;

  xdr_init( &args[0], bb, sizeof(bb) );
  xdr_encode_uint64( &args[0], parentid );
  xdr_encode_string( &args[0], name );
  xdr_encode_uint32( &args[0], flags );
  xdr_encode_uint32( &args[0], len );
  xdr_init( &args[1], buf, len );
  args[1].offset = len;
  sts = hrauth_call_tcp_async( &hcall, args, 2 );
  if( sts ) {
    fjui_set_statusbar( 1, "RegPut failed" );
  }
}

static void fvmload_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {
  int sts, b;
  fjui_set_statusbar( 1, "FvmLoad %s", xdr ? "Succes" : "Timeout" );
  sts = xdr_decode_boolean( xdr, &b );
  if( !sts && b ) {
    fjui_fvm_refresh( hcallp->hostid );
  }
}

void fjui_call_fvmload( uint64_t hostid, char *buf, int len, uint32_t flags, int registerp ) {
	struct hrauth_call hcall;
	int sts;
	struct xdr_s args[3];
	char argbuf[16], argbuf2[16];
	
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FVM_RPC_PROG;
  hcall.vers = FVM_RPC_VERS;
  hcall.proc = 2; 
  hcall.donecb = fvmload_cb;
  hcall.cxt = NULL;
  hcall.timeout = 200;
  hcall.service = HRAUTH_SERVICE_PRIV;

  xdr_init( &args[0], argbuf, sizeof(argbuf) );
  xdr_encode_uint32( &args[0], len );
  xdr_init( &args[1], buf, len );
  args[1].offset = len;
  xdr_init( &args[2], argbuf2, sizeof(argbuf2) );
  xdr_encode_uint32( &args[2], flags );
  xdr_encode_boolean( &args[2], registerp );
  
  sts = hrauth_call_tcp_async( &hcall, args, 3 );
  if( sts ) {
    fjui_set_statusbar( 1, "FvmLoad failed" );    
  }
}

static void fvmunload_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {
  int sts, b;
  
  fjui_set_statusbar( 1, "FvUnload %s", xdr ? "Succes" : "Timeout" );
  sts = xdr_decode_boolean( xdr, &b );
  if( !sts && b ) {
    fjui_fvm_refresh( hcallp->hostid );
  }
}

void fjui_call_fvmunload( uint64_t hostid, char *modname ) {
	struct hrauth_call hcall;
	int sts;
	struct xdr_s args;
	char argbuf[64];
	
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = FVM_RPC_PROG;
  hcall.vers = FVM_RPC_VERS;
  hcall.proc = 3; 
  hcall.donecb = fvmload_cb;
  hcall.cxt = NULL;
  hcall.timeout = 200;
  hcall.service = HRAUTH_SERVICE_PRIV;

  xdr_init( &args, argbuf, sizeof(argbuf) );
  xdr_encode_string( &args, modname );
  sts = hrauth_call_tcp_async( &hcall, &args, 1 );
  if( sts ) {
    fjui_set_statusbar( 1, "FvmUnload failed" );    
  }
}
