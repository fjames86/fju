
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <WinSock2.h>
#include <Windows.h>
#else
#include <arpa/inet.h>
#endif

#include "fvm-private.h"
#include <fju/rpc.h>
#include <fju/rpcd.h>

#include <fju/programs.h>
#include <fju/log.h>
#include <fju/freg.h>


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
  vs->vers = vers;
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

static int fvm_rpc_proc( struct rpc_inc *inc ) {
  /* get prog, vers, proc */
  uint32_t prog, proc;
  struct fvm_s state;
  int sts, handle, arglength, count;
  
  prog = inc->msg.u.call.prog;
  proc = inc->msg.u.call.proc;

  log_writef( NULL, LOG_LVL_INFO, "fvm_rpc_log progid=%u proc=%u", prog, proc );
  
  /* initialize state */
  sts = fvm_state_init( &state, prog, proc );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  /* copy args onto fvm stack and set r0 to length */
  arglength = inc->xdr.count - inc->xdr.offset;
  log_writef( NULL, LOG_LVL_INFO, "fvm_rpc_log offet=%u count=%u arglength = %u", inc->xdr.offset, inc->xdr.count, arglength );
  
  memcpy( &state.stack, inc->xdr.buf + inc->xdr.offset, arglength );
  state.reg[FVM_REG_R0] = htonl( arglength );
  sts = fvm_run( &state, fvm_max_steps( 0 ) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  log_writef( NULL, LOG_LVL_INFO, "success reply length %d", ntohl( state.reg[FVM_REG_R0] ) );
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  /* R0 contains length, R1 contains address of buffer */
  count = ntohl( state.reg[FVM_REG_R0] );
  if( count > 0 && count < FVM_MAX_STACK ) {
    char *p = fvm_getaddr( &state, ntohl( state.reg[FVM_REG_R1] ) );
    if( p ) {
      sts = xdr_encode_fixed( &inc->xdr, (uint8_t *)p, count );
      if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
    }
  }
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

int fvm_register_program( char *mname ) {
  struct fvm_module *m;
  struct rpc_program *pg;
  int i, nprocs;

  m = fvm_module_by_name( mname );
  if( !m ) return -1;

  /* collect symbols pointing to text segment, probably functions? */
  nprocs = 0;
  for( i = 0; i < m->header.symcount; i++ ) {
    if( m->symbols[i].addr >= FVM_ADDR_TEXT && m->symbols[i].addr <= (FVM_ADDR_TEXT + FVM_MAX_TEXT) ) {
      nprocs++;
    }
  }
  
  pg = alloc_program( m->header.progid, m->header.versid, nprocs, fvm_rpc_proc );
  rpc_program_register( pg );
  return 0;
}

int fvm_unregister_program( char *mname ) {
  struct fvm_module *m;
  struct rpc_program *p;
  struct rpc_version *vs;
  struct rpc_proc *pc;
  
  m = fvm_module_by_name( mname );
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

static int fvm_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm_proc_list( struct rpc_inc *inc ) {
  int handle;
  int i, j, n, m;
  struct fvm_module_info *minfo = NULL;
  struct fvm_symbol *sym = NULL;
  int nsym = 0;
  
  /* no args */
  n = fvm_module_list( NULL, 0 );
  if( n > 0 ) {
    minfo = malloc( sizeof(*minfo) * n );
    m = fvm_module_list( minfo, n );
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
    m = fvm_module_symbols( minfo[i].name, sym, nsym );
    if( m > nsym ) {
      nsym = m;
      sym = realloc( sym, sizeof(*sym) * nsym );
    }
    m = fvm_module_symbols( minfo[i].name, sym, nsym );
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

static int fvm_proc_load( struct rpc_inc *inc ) {
  int handle;
  char *bufp;
  int lenp, sts, registerp;
  char name[FVM_MAX_NAME];
  
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &registerp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_module_register( bufp, lenp, name );
  if( sts ) {
    fvm_module_unload( name );
    sts = fvm_module_register( bufp, lenp, name );
  }

  if( registerp ) {
    fvm_unregister_program( name );
    fvm_register_program( name );
  }
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_unload( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_module_unload( name );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int fvm_proc_register( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_register_program( name );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_unregister( struct rpc_inc *inc ) {
  int handle, sts;
  char name[FVM_MAX_NAME];

  sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  sts = fvm_unregister_program( name );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 1 : 0 );  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct rpc_proc fvm_procs[] = {
  { 0, fvm_proc_null },
  { 1, fvm_proc_list },
  { 2, fvm_proc_load },
  { 3, fvm_proc_unload },
  { 4, fvm_proc_register },
  { 5, fvm_proc_unregister },
  { 0, NULL }
};

static struct rpc_version fvm_vers = {
  NULL, FVM_RPC_VERS, fvm_procs
};

static struct rpc_program fvm_prog = {
  NULL, FVM_RPC_PROG, &fvm_vers
};

struct fvm_iterator {
  struct rpc_iterator iter;
  char mname[FVM_MAX_NAME];
  char pname[FVM_MAX_NAME];
};

static void fvm_iter_cb( struct rpc_iterator *it ) {
  struct fvm_iterator *fvm = (struct fvm_iterator *)it;
  struct fvm_s state;
  uint32_t procid;
  struct fvm_module *m;
  int i, sts;

  m = fvm_module_by_name( fvm->mname );
  if( !m ) return;

  procid = -1;
  for( i = 0; i < m->header.symcount; i++ ) {
    if( strcmp( m->symbols[i].name, fvm->pname ) == 0 ) {
      procid = i;
      break;
    }
  }
  if( procid == -1 ) return;
  
  sts = fvm_state_init( &state, m->header.progid, procid );
  if( sts ) return;
  fvm_run( &state, -1 );
}

void fvm_rpc_register( void ) {
  int sts;
  uint32_t nsteps;
  uint64_t key;
  struct freg_entry entry;
  char path[256];
  char name[FVM_MAX_NAME];

  /* load all these modules on startup */
  sts = freg_subkey( NULL, 0, "/fju/fvm/modules", FREG_CREATE, &key );
  if( !sts ) {
    sts = freg_next( NULL, key, 0, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_STRING ) {
	sts = freg_get( NULL, entry.id, NULL, path, sizeof(path), NULL );
	if( !sts ) {
	  log_writef( NULL, LOG_LVL_INFO, "FVM loading module %s", path );
	  fvm_module_load( path, name );
	}
      }
      sts = freg_next( NULL, key, entry.id, &entry );
    }
  }

  /* register all these modules as rpc programs on startup */
  sts = freg_subkey( NULL, 0, "/fju/fvm/programs", FREG_CREATE, &key );
  if( !sts ) {
    sts = freg_next( NULL, key, 0, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_STRING ) {
	sts = freg_get( NULL, entry.id, NULL, name, sizeof(name), NULL );
	if( !sts ) {
	  log_writef( NULL, LOG_LVL_INFO, "FVM registering program %s", name );
	  fvm_register_program( name );
	}
      }
	
      sts = freg_next( NULL, key, entry.id, &entry );
    }    
  }

  /* register service routines */
  sts = freg_subkey( NULL, 0, "/fju/fvm/service", FREG_CREATE, &key );
  if( !sts ) {
    sts = freg_next( NULL, key, 0, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
	char mname[FVM_MAX_NAME], pname[FVM_MAX_NAME];
	uint32_t period = 1000;
	
	sts = freg_get_by_name( NULL, entry.id, "module", FREG_TYPE_STRING, mname, sizeof(mname), NULL );
	if( !sts ) sts = freg_get_by_name( NULL, entry.id, "proc", FREG_TYPE_STRING, pname, sizeof(pname), NULL );
	sts = freg_get_by_name( NULL, entry.id, "period", FREG_TYPE_UINT32, (char *)&period, sizeof(period), NULL );
	if( sts ) {
	  period = 1000;
	  sts = 0;
	}
	
	if( !sts ) {
	  struct fvm_iterator *iter;
	  iter = malloc( sizeof(*iter) + 8 );
	  memset( iter, 0, sizeof(*iter) );
	  iter->iter.period = period;
	  iter->iter.cb = fvm_iter_cb;
	  strncpy( iter->mname, mname, FVM_MAX_NAME );
	  strncpy( iter->pname, pname, FVM_MAX_NAME );

	  log_writef( NULL, LOG_LVL_INFO, "FVM registering iterator %s %s", mname, pname );
	  rpc_iterator_register( &iter->iter );
	}
      }
	
      sts = freg_next( NULL, key, entry.id, &entry );
    }    
  }
  
  /* set max steps to limit runaway programs blocking rpcd */
  sts = freg_get_by_name( NULL, 0, "/fju/fvm/MaxSteps", FREG_TYPE_UINT32, (char *)&nsteps, sizeof(nsteps), NULL );
  if( !sts ) {
    fvm_max_steps( nsteps );
  }

  
  rpc_program_register( &fvm_prog );
}
