
/*
 * This module provides a mechanism for stopping the daemon via RPC.
 * Can be extended in future to provide other rpcd service commands.
 */

#include <stdlib.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/programs.h>
#include <fju/lic.h>
#include <fju/hrauth.h>

#include "rpc-private.h"

static int cmdprog_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int cmdprog_proc_stop( struct rpc_inc *inc ) {
  int handle;
  
  rpcd_stop();
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int cmdprog_proc_licinfo( struct rpc_inc *inc ) {
  int handle, sts;
  struct lic_s lic;
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = fju_check_license( NULL, 0, &lic );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
  if( !sts ) {
    xdr_encode_uint64( &inc->xdr, lic.hostid );
    xdr_encode_uint64( &inc->xdr, lic.expire );
    xdr_encode_uint32( &inc->xdr, lic.version );
    xdr_encode_uint32( &inc->xdr, lic.flags );
    xdr_encode_uint32( &inc->xdr, lic.spare[0] );
    xdr_encode_uint32( &inc->xdr, lic.spare[1] );        
  }
    
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int cmdprog_proc_connlist( struct rpc_inc *inc ) {
  int handle;
  struct rpc_conn *c;

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  c = rpc_conn_list();
  while( c ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_uint64( &inc->xdr, c->connid );
    xdr_encode_uint32( &inc->xdr, c->dirtype );
    xdr_encode_uint32( &inc->xdr, c->cstate );
    xdr_encode_uint64( &inc->xdr, c->cdata.rx );
    xdr_encode_uint64( &inc->xdr, c->cdata.tx );
    xdr_encode_uint32( &inc->xdr, c->cdata.offset );
    xdr_encode_uint32( &inc->xdr, c->cdata.count );
    xdr_encode_uint32( &inc->xdr, c->listype );
    switch( c->listype ) {
    case RPC_LISTEN_TCP:
      xdr_encode_uint32( &inc->xdr, ((struct sockaddr_in *)&c->addr)->sin_addr.s_addr );
      xdr_encode_uint32( &inc->xdr, ntohs( ((struct sockaddr_in *)&c->addr)->sin_port ) );
      break;
    default:
      break;
    }
    
    /* TODO encode other info */
    
    c = c->next;
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}
  
static struct rpc_proc cmdprog_procs[] = {
  { 0, cmdprog_proc_null },
  { 1, cmdprog_proc_stop },
  { 3, cmdprog_proc_licinfo },
  { 4, cmdprog_proc_connlist },
  { 0, NULL }
};

static struct rpc_version cmdprog_vers = {
  NULL, 1, cmdprog_procs
};

static uint32_t cmdprog_auths[] = { RPC_AUTH_HRAUTH, 0 };
static struct rpc_program cmdprog_prog = {
    NULL, FJUD_RPC_PROG, &cmdprog_vers, cmdprog_auths
};

int cmdprog_register( void ) {
  rpc_program_register( &cmdprog_prog );
  return 0;
}

