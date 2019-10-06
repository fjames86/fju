
/*
 * This module provides a mechanism for stopping the daemon via RPC.
 * Can be extended in future to provide other rpcd service commands.
 */

#include <stdlib.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>

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

static struct rpc_proc cmdprog_procs[] = {
  { 0, cmdprog_proc_null },
  { 1, cmdprog_proc_stop },
  { 0, NULL }
};

static struct rpc_version cmdprog_vers = {
  NULL, 1, cmdprog_procs
};

static struct rpc_program cmdprog_prog = {
  NULL, 999999, &cmdprog_vers
};

int cmdprog_register( void ) {
  rpc_program_register( &cmdprog_prog );
  return 0;
}

