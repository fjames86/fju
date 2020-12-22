
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

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "Timeout" );
		return;
	}

	fjui_set_statusbar( 1, "Success ", sec_timestr( time( NULL ), timestr ) );

	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );

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
  }
}


static void connlist_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {	
	int sts;
	char timestr[64];
	uint64_t hostid;
	struct fjui_hostinfo *info;
	int b;
	uint32_t addr, port;

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "Timeout" );
		return;
	}

	fjui_set_statusbar( 1, "Success ", sec_timestr( time( NULL ), timestr ) );

	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );

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

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "Timeout" );
		return;
	}

	fjui_set_statusbar( 1, "Success ", sec_timestr( time( NULL ), timestr ) );

	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );


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
		if( info->modules >= 32 ) break;

		sts = xdr_decode_boolean( xdr, &b );
		if( sts ) return;
	}


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

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "Timeout" );
		return;
	}

	fjui_set_statusbar( 1, "Success ", sec_timestr( time( NULL ), timestr ) );

	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );


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

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "Timeout" );
		return;
	}

	fjui_set_statusbar( 1, "Success ", sec_timestr( time( NULL ), timestr ) );

	hostid = hcallp->hostid;
	info = fjui_hostinfo_by_id( hostid );
	if( !info ) info = fjui_hostinfo_add( hostid );


	info->nraft = 0;
	sts = xdr_decode_boolean( xdr, &b );
	if( sts ) return;
	while( b ) {
		
		info->nraft++;
		if( info->nraft >= 32 ) break;

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
    //hrauth_log( LOG_LVL_ERROR, "fjui_call_getlicinfo failed hostid=%"PRIx64"", hostid );
  }
}