
/*
 * This module provides a mechanism for stopping the daemon via RPC.
 * Can be extended in future to provide other rpcd service commands.
 */

#include <stdlib.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/programs.h>

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

static int cmdprog_proc_event( struct rpc_inc *inc ) {
  int handle, sts, lenp;
  uint32_t eventid;
  char *bufp;
  struct xdr_s args;
  
  sts = xdr_decode_uint32( &inc->xdr, &eventid );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  xdr_init( &args, (uint8_t *)bufp, lenp );
  rpcd_event_publish( eventid, &args );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static struct rpc_proc cmdprog_procs[] = {
  { 0, cmdprog_proc_null },
  { 1, cmdprog_proc_stop },
  { 2, cmdprog_proc_event },
  { 0, NULL }
};

static struct rpc_version cmdprog_vers = {
  NULL, 1, cmdprog_procs
};

static struct rpc_program cmdprog_prog = {
  NULL, FJUD_RPC_PROG, &cmdprog_vers
};

int cmdprog_register( void ) {
  rpc_program_register( &cmdprog_prog );
  return 0;
}

