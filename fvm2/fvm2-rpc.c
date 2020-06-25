
#include "fvm2-private.h"
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <arpa/inet.h>

/* rpc interface */

/*
 *Load module
 * Unload module
 * List modules 
 * 
 */

/* want to be able to register an rpc program which is dynamically generated (normally it is always a static sturct) */

static struct rpc_program *alloc_program( uint32_t prog, uint32_t vers, int nprocs, rpc_proc_t proccb ) {
  struct rpc_program *pg;
  struct rpc_version *vs;
  struct rpc_proc *pc;
  int i;
  
  pg = malloc( sizeof(*pg) );
  memset( pg, 0, sizeof(*pg) );
  pg->prog = prog;
  vs = malloc( sizeof(*vs) );
  memset( vs, 0, sizeof(*vs) );
  pg->vers = vs;

  pc = malloc( sizeof(*pc) * (nprocs + 1) );
  memset( pc, 0, sizeof(*pc) * (nprocs + 1) );
  for( i = 0; i < nprocs; i++ ) {
    pc[i].proc = i;
    pc[i].fn = proccb;
  }
  vs->procs = pc;
  
  return pg;    
}

static int fvm2_rpc_proc( struct rpc_inc *inc ) {
  /* get prog, vers, proc */
  uint32_t prog, proc;
  struct fvm2_s state;
  int sts, handle, arglength;
  
  prog = inc->msg.u.call.prog;
  proc = inc->msg.u.call.proc;

  /* initialize state */
  sts = fvm2_state_init( &state, prog, proc );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  /* copy args onto fvm stack and set r0 to length */
  arglength = inc->xdr.count - inc->xdr.offset;
  memcpy( &state.stack, inc->xdr.buf + inc->xdr.offset, arglength );
  state.reg[FVM2_REG_R0] = htonl( arglength );
  sts = fvm2_run( &state, fvm2_max_steps( 0 ) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  sts = xdr_encode_fixed( &inc->xdr, state.stack, state.reg[FVM2_REG_R0] );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

int fvm2_register_program( char *mname ) {
  struct fvm2_module *m;
  struct rpc_program *pg;
  int i, nprocs;

  m = fvm2_module_by_name( mname );
  if( !m ) return -1;

  /* collect symbols pointing to text segment, probably functions? */
  nprocs = 0;
  for( i = 0; i < m->header.symcount; i++ ) {
    if( m->symbols[i].addr >= FVM2_ADDR_TEXT && m->symbols[i].addr <= (FVM2_ADDR_TEXT + FVM2_MAX_TEXT) ) {
      nprocs++;
    }
  }
  
  pg = alloc_program( m->header.progid, m->header.versid, nprocs, fvm2_rpc_proc );
  rpc_program_register( pg );
  return 0;
}

