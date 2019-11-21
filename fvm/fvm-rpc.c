

#include <stdlib.h>
#include <string.h>

#include <fju/rpc.h>
#include <fju/fvm.h>
#include <fju/sec.h>

struct loaded_fvm {
  struct loaded_fvm *next;
  uint32_t id;
  struct fvm_state fvm;
};
  
static struct {
  struct loaded_fvm *progs;
} glob;

static void fvm_iter_cb( struct rpc_iterator *iter );

static struct rpc_iterator fvm_iter = {
    NULL,
    0,
    1000,
    fvm_iter_cb,
    NULL
};


static int fvm_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm_proc_start( struct rpc_inc *inc ) {
  int handle, sts, buflen;
  char *bufp;
  struct loaded_fvm *lf;
  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &buflen );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  lf = malloc( sizeof(*lf) );
  memset( lf, 0, sizeof(*lf) );
  lf->id = sec_rand_uint32();
  fvm_load( &lf->fvm, (uint16_t *)bufp, buflen / 2 );
  lf->fvm.inlog_id = 0;
  /* push onto list */
  lf->next = glob.progs;
  glob.progs = lf;
  /* ensure the iterator runs immediately */
  fvm_iter.timeout = 0;
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_uint32( &inc->xdr, lf->id );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_stop( struct rpc_inc *inc ) {
  int handle, sts;
  uint32_t id;
  struct loaded_fvm *lf, *prev;
  
  sts = xdr_decode_uint32( &inc->xdr, &id );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  lf = glob.progs;
  prev = NULL;
  while( lf ) {
    if( lf->id == id ) {
      if( prev ) prev = lf->next;
      else glob.progs = lf->next;
      free( lf );
      break;
    }
    prev = lf;
    lf = lf->next;    
  }
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}


static int fvm_proc_list( struct rpc_inc *inc ) {
  int handle;
  struct loaded_fvm *lf;

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  
  lf = glob.progs;
  while( lf ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_uint32( &inc->xdr, lf->id );
    lf = lf->next;    
  }
  xdr_encode_boolean( &inc->xdr, 0 );

  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static struct rpc_proc fvm_procs[] = {
  { 0, fvm_proc_null },
  { 1, fvm_proc_start },
  { 2, fvm_proc_stop },
  { 3, fvm_proc_list },
  { 0, NULL }
};

static struct rpc_version fvm_vers = {
  NULL, FVM_RPC_VERS, fvm_procs
};

static struct rpc_program fvm_prog = {
  NULL, FVM_RPC_PROG, &fvm_vers
};


static void fvm_iter_cb( struct rpc_iterator *iter ) {
  struct loaded_fvm *lf, *prev, *next;
  uint64_t timeout;

  timeout = iter->timeout;
  lf = glob.progs;
  prev = NULL;
  while( lf ) {
    next = lf->next;
    fvm_run_nsteps( &lf->fvm, 1000 );
    if( lf->fvm.flags & FVM_FLAG_RUNNING ) {
      /* program still needs to run, but we yield here and schedule ourselves to be run again ASAP */
      timeout = 0;
      prev = lf;
    } else {
      /* program run completed - free */
      if( prev ) prev->next = next;
      else glob.progs = next;
      free( lf );
    }
    lf = next;
  }

  iter->timeout = timeout;

}


void fvm_register( void ) {
  rpc_program_register( &fvm_prog );
  rpc_iterator_register( &fvm_iter );
}
