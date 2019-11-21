

#include <stdlib.h>
#include <string.h>

#include <fju/rpc.h>
#include <fju/fvm.h>
#include <fju/sec.h>
#include <fju/log.h>

struct loaded_fvm {
  struct loaded_fvm *next;
  uint32_t id;
    uint32_t flags;
#define FVM_RPC_AUTOUNLOAD 0x0001 
  struct fvm_state fvm;
};
  
static struct {
  struct loaded_fvm *progs;
  struct log_s outlog;
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

static int fvm_proc_load( struct rpc_inc *inc ) {
  int handle, buflen, sts, start;
  char *bufp;
  struct loaded_fvm *lf;
  uint32_t flags;
  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &buflen );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &start );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  lf = malloc( sizeof(*lf) );
  memset( lf, 0, sizeof(*lf) );
  lf->id = sec_rand_uint32();
  fvm_load( &lf->fvm, (uint16_t *)bufp, buflen / 2 );
  lf->fvm.outlog = &glob.outlog;
  lf->fvm.inlog_id = 0;
  lf->flags = flags;
  
  /* push onto list */
  lf->next = glob.progs;
  glob.progs = lf;
  
  /* ensure the iterator runs immediately */
  fvm_iter.timeout = 0;
  if( start ) lf->fvm.flags |= FVM_FLAG_RUNNING;
  else lf->fvm.flags &= ~FVM_FLAG_RUNNING;
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_uint32( &inc->xdr, lf->id );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_unload( struct rpc_inc *inc ) {
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
    xdr_encode_uint32( &inc->xdr, lf->fvm.flags );
    lf = lf->next;    
  }
  xdr_encode_boolean( &inc->xdr, 0 );

  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}


static int fvm_proc_pause( struct rpc_inc *inc ) {
  int handle, sts, stop;
  struct loaded_fvm *lf;
  uint32_t id;

  sts = xdr_decode_uint32( &inc->xdr, &id );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &stop );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );  
  lf = glob.progs;
  while( lf ) {
      if( lf->id == id ) {
	  if( stop ) lf->fvm.flags &= ~FVM_FLAG_RUNNING;
	  else lf->fvm.flags |= FVM_FLAG_RUNNING;
	  break;
      }
      lf = lf->next;    
  }

  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static struct rpc_proc fvm_procs[] = {
  { 0, fvm_proc_null },
  { 1, fvm_proc_load },
  { 2, fvm_proc_unload },
  { 3, fvm_proc_list },
  { 4, fvm_proc_pause },
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
    } else if( (lf->fvm.flags & FVM_FLAG_DONE) && (lf->flags & FVM_RPC_AUTOUNLOAD) ) {
	if( prev ) prev->next = next;
	else glob.progs = next;
	free( lf );
    } else if( lf->fvm.sleep_timeout && (rpc_now() >= lf->fvm.sleep_timeout) ) {
	lf->fvm.flags |= FVM_FLAG_RUNNING;
	lf->fvm.sleep_timeout = 0;
	timeout = 0;
    } else {
	/* stopped running but set to auto unload when stopped */
	prev = lf;
    }
    lf = next;
  }

  iter->timeout = timeout;
}


void fvm_register( void ) {
  log_open( NULL, NULL, &glob.outlog );
  rpc_program_register( &fvm_prog );
  rpc_iterator_register( &fvm_iter );
}
