
#include "fvm2-private.h"
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <arpa/inet.h>
#include <fju/programs.h>

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

int fvm2_unregister_program( char *mname ) {
  struct fvm2_module *m;
  struct rpc_program *p;
  struct rpc_version *vs;
  struct rpc_proc *pc;
  
  m = fvm2_module_by_name( mname );
  if( !m ) return -1;
  
  rpc_program_find( m->header.progid, 0, 0, &p, &vs, &pc );
  if( !p ) return -1;

  rpc_program_unregister( p );
  free( p->vers->procs );
  free( p->vers );
  free( p );
  return 0;  
}

/* ------------------- */

static int fvm2_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm2_proc_list( struct rpc_inc *inc ) {
  int handle;
  int i, j, n, m;
  struct fvm2_module_info *minfo = NULL;
  struct fvm2_symbol *sym = NULL;
  int nsym = 0;
  
  /* no args */
  n = fvm2_module_list( NULL, 0 );
  if( n > 0 ) {
    minfo = malloc( sizeof(*minfo) * n );
    m = fvm2_module_list( minfo, n );
    if( m < n ) n = m;
  }

  nsym = 32;
  sym = malloc( sizeof(*sym) * nsym );
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  for( i = 0; i < n; i++ ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_string( &inc->xdr, minfo[i].name );
    xdr_encode_uint32( &inc->xdr, minfo[i].progid );
    xdr_encode_uint32( &inc->xdr, minfo[i].versid );
    xdr_encode_uint32( &inc->xdr, minfo[i].datasize );
    xdr_encode_uint32( &inc->xdr, minfo[i].textsize );
    m = fvm2_module_symbols( minfo[i].name, sym, nsym );
    if( m > nsym ) {
      nsym = m;
      sym = realloc( sym, sizeof(*sym) * nsym );
    }
    m = fvm2_module_symbols( minfo[i].name, sym, nsym );
    for( j = 0; j < m; j++ ) {
      xdr_encode_boolean( &inc->xdr, 1 );
      xdr_encode_string( &inc->xdr, sym[j].name );
    }
    xdr_encode_boolean( &inc->xdr, 0 );
  }
  xdr_encode_boolean( &inc->xdr, 0 );
  
  rpc_complete_accept_reply( inc, handle );

  if( minfo ) free( minfo );
  if( sym ) free( sym );
  
  return 0;
}

static int fvm2_proc_load( struct rpc_inc *inc ) {
  int handle;
  char *bufp;
  int lenp, sts;
  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm2_module_register( bufp, lenp );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm2_proc_unload( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM2_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm2_module_unload( name );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm2_proc_register( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM2_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm2_register_program( name );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm2_proc_unregister( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM2_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm2_unregister_program( name );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct rpc_proc fvm2_procs[] = {
  { 0, fvm2_proc_null },
  { 1, fvm2_proc_list },
  { 2, fvm2_proc_load },
  { 3, fvm2_proc_unload },
  { 4, fvm2_proc_register },
  { 5, fvm2_proc_unregister },
  { 0, NULL }
};

static struct rpc_version fvm2_vers = {
  NULL, FVM2_RPC_VERS, fvm2_procs
};

static struct rpc_program fvm2_prog = {
  NULL, FVM2_RPC_PROG, &fvm2_vers
};

void fvm2_rpc_register( void ) {
  rpc_program_register( &fvm2_prog );
}
