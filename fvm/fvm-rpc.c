

#include <stdlib.h>
#include <string.h>

#include <fju/rpc.h>
#include <fju/fvm.h>

static struct {
  struct fvm_state fvm;
} glob;

static int fvm_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm_proc_run( struct rpc_inc *inc ) {
  int handle, sts;
  char *bufp;
  int buflen, timeout;
  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &buflen );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, (uint32_t *)&timeout );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  fvm_load( &glob.fvm, (uint16_t *)bufp, buflen / 2 );
  if( timeout ) fvm_run_timeout( &glob.fvm, timeout );
  else fvm_run( &glob.fvm );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static struct rpc_proc fvm_procs[] = {
  { 0, fvm_proc_null },
  { 1, fvm_proc_run },
  { 0, NULL }
};

static struct rpc_version fvm_vers = {
  NULL, FVM_RPC_VERS, fvm_procs
};

static struct rpc_program fvm_prog = {
  NULL, FVM_RPC_PROG, &fvm_vers
};


static void fvm_iter_cb( struct rpc_iterator *iter );

static struct rpc_iterator fvm_iter = {
    NULL,
    0,
    1000,
    fvm_iter_cb,
    NULL
};

static void fvm_iter_cb( struct rpc_iterator *iter ) {
}


void fvm_register( void ) {
  rpc_program_register( &fvm_prog );
  rpc_iterator_register( &fvm_iter );
}
