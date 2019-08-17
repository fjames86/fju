
#include <fju/freg.h>
#include <fju/rpc.h>

#include <stdlib.h>
#include <string.h>

static int freg_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int freg_proc_list( struct rpc_inc *inc ) {
    int sts, handle;
    uint64_t parentid;
    int n, m, i;
    char *str;
    uint32_t u32;
    uint64_t u64;
    struct freg_entry *elist;
    
    sts = xdr_decode_uint64( &inc->xdr, &parentid );
    if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
    
    rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
    n = freg_list( parentid, NULL, 0 );
    elist = malloc( sizeof(*elist) * n );
    m = freg_list( parentid, elist, n );
    if( m < n ) n = m;

    for( i = 0; i < n; i++ ) {
	xdr_encode_boolean( &inc->xdr, 1 );
	xdr_encode_string( &inc->xdr, elist[i].name );
	xdr_encode_uint32( &inc->xdr, elist[i].flags );
	switch( elist[i].flags & FREG_TYPE_MASK ) {
	case FREG_TYPE_UINT32:
	    sts = freg_get( parentid, elist[i].name, NULL, (char *)&u32, sizeof(u32), NULL );
	    xdr_encode_uint32( &inc->xdr, u32 );
	    break;
	case FREG_TYPE_UINT64:
	case FREG_TYPE_KEY:
	    sts = freg_get( parentid, elist[i].name, NULL, (char *)&u64, sizeof(u64), NULL );
	    xdr_encode_uint64( &inc->xdr, u64 );	    
	    break;
	case FREG_TYPE_STRING:
	    str = malloc( elist[i].len + 1 );
	    sts = freg_get( parentid, elist[i].name, NULL, (char *)str, elist[i].len + 1, NULL );
	    xdr_encode_string( &inc->xdr, str );
	    free( str );
	    break;
	case FREG_TYPE_OPAQUE:
	    str = malloc( elist[i].len );
	    sts = freg_get( parentid, elist[i].name, NULL, (char *)str, elist[i].len, NULL );
	    xdr_encode_opaque( &inc->xdr, str, elist[i].len );
	    free( str );
	    break;
	}
    }
    xdr_encode_boolean( &inc->xdr, 0 );

    free( elist );
    rpc_complete_accept_reply( inc, handle );
    
    return 0;
}


static struct rpc_proc freg_procs[] = {
  { 0, freg_proc_null },
  { 1, freg_proc_list },
  { 0, NULL }
};

static struct rpc_version freg_vers = {
  NULL, FREG_RPC_VERS, freg_procs
};

static struct rpc_program freg_prog = {
  NULL, FREG_RPC_PROG, &freg_vers
};

void freg_register( void ) {
    freg_open();
    rpc_program_register( &freg_prog );
}
