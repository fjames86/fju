
#include <fju/freg.h>
#include <fju/rpc.h>
#include <fju/hrauth.h>

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

    if( inc->msg.u.call.auth.flavour != RPC_AUTH_HRAUTH ) {
      return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
    }

    sts = xdr_decode_uint64( &inc->xdr, &parentid );
    if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
    
    rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
    n = freg_list( NULL, parentid, NULL, 0 );
    elist = malloc( sizeof(*elist) * n );
    m = freg_list( NULL, parentid, elist, n );
    if( m < n ) n = m;

    for( i = 0; i < n; i++ ) {
	xdr_encode_boolean( &inc->xdr, 1 );
	xdr_encode_uint64( &inc->xdr, elist[i].id );
	xdr_encode_string( &inc->xdr, elist[i].name );
	xdr_encode_uint32( &inc->xdr, elist[i].flags );
	switch( elist[i].flags & FREG_TYPE_MASK ) {
	case FREG_TYPE_UINT32:
	  sts = freg_get( NULL, elist[i].id, NULL, (char *)&u32, sizeof(u32), NULL );
	  xdr_encode_uint32( &inc->xdr, u32 );
	  break;
	case FREG_TYPE_UINT64:
	  sts = freg_get( NULL, elist[i].id, NULL, (char *)&u64, sizeof(u64), NULL );
	  xdr_encode_uint64( &inc->xdr, u64 );	    
	  break;
	case FREG_TYPE_KEY:
	  break;
	case FREG_TYPE_STRING:
	    str = malloc( elist[i].len + 1 );
	    sts = freg_get( NULL, elist[i].id, NULL, (char *)str, elist[i].len + 1, NULL );
	    xdr_encode_string( &inc->xdr, str );
	    free( str );
	    break;
	case FREG_TYPE_OPAQUE:
	    str = malloc( elist[i].len );
	    sts = freg_get( NULL, elist[i].id, NULL, (char *)str, elist[i].len, NULL );
	    xdr_encode_opaque( &inc->xdr, (uint8_t *)str, elist[i].len );
	    free( str );
	    break;
	}
    }
    xdr_encode_boolean( &inc->xdr, 0 );

    free( elist );
    rpc_complete_accept_reply( inc, handle );
    
    return 0;
}

static int freg_proc_get( struct rpc_inc *inc ) {
  int sts, handle, len, i;
  uint32_t flags;
  uint64_t id;
  char *buf = NULL;

  if( inc->msg.u.call.auth.flavour != RPC_AUTH_HRAUTH ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }

  sts = xdr_decode_uint64( &inc->xdr, &id );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = freg_get( NULL, id, &flags, NULL, 0, &len );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  buf = malloc( len );
  sts = freg_get( NULL, id, &flags, buf, len, NULL );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    goto done;
  }

  xdr_encode_boolean( &inc->xdr, 1 );
  xdr_encode_uint64( &inc->xdr, id );
  xdr_encode_uint32( &inc->xdr, flags );
  switch( flags & FREG_TYPE_MASK ) {
  case FREG_TYPE_UINT32:
    xdr_encode_uint32( &inc->xdr, *(uint32_t *)buf );
    break;
  case FREG_TYPE_UINT64:
    xdr_encode_uint64( &inc->xdr, *(uint64_t *)buf ); 
    break;
  case FREG_TYPE_KEY:
    xdr_encode_uint32( &inc->xdr, len / sizeof(uint64_t) );
    for( i = 0; i < (len / sizeof(uint64_t)); i++ ) {
      xdr_encode_uint64( &inc->xdr, ((uint64_t *)buf)[i] );
    }
    break;
  case FREG_TYPE_STRING:
    xdr_encode_string( &inc->xdr, buf );
    break;
  case FREG_TYPE_OPAQUE:
    xdr_encode_opaque( &inc->xdr, (uint8_t *)buf, len );
    break;
  }

 done:
  if( buf ) free( buf );
  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int freg_proc_put( struct rpc_inc *inc ) {
  int sts, handle, len;
  uint32_t flags;
  uint64_t parentid, id;
  char *buf = NULL;
  char name[FREG_MAX_NAME];

  if( inc->msg.u.call.auth.flavour != RPC_AUTH_HRAUTH ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }

  sts = xdr_decode_uint64( &inc->xdr, &parentid );
  if( !sts ) sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&buf, &len );  
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  sts = freg_put( NULL, parentid, name, flags, buf, len, &id );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
  } else {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_uint64( &inc->xdr, id );
  }  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int freg_proc_rem( struct rpc_inc *inc ) {
  int sts, handle;
  uint64_t parentid, id;

  if( inc->msg.u.call.auth.flavour != RPC_AUTH_HRAUTH ) {
    return rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  }
  
  sts = xdr_decode_uint64( &inc->xdr, &parentid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &id );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  sts = freg_rem( NULL, parentid, id );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static struct rpc_proc freg_procs[] = {
  { 0, freg_proc_null },
  { 1, freg_proc_list },
  { 2, freg_proc_get },
  { 3, freg_proc_put },
  { 4, freg_proc_rem },
  { 0, NULL }
};

static struct rpc_version freg_vers = {
  NULL, FREG_RPC_VERS, freg_procs
};

static struct rpc_program freg_prog = {
  NULL, FREG_RPC_PROG, &freg_vers
};

void freg_register( void ) {
  freg_open( NULL, NULL );
  rpc_program_register( &freg_prog );
}
