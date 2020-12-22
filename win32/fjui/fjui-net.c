
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

	if( !xdr ) {
		/* timeout */
		fjui_set_statusbar( 1, "Timeout" );
		return;
	}

	fjui_set_statusbar( 1, "Success ", sec_timestr( time( NULL ), timestr ) );
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