
/*
 * Clustering fvm applications can mean two different things so we need to be careful 
 * about what the expectations are: 
 * - Application runs on a specified node and the updated state is replicated to all nodes
 *   -> application runs using a temporary data segment so that the original is unchanged 
 *   -> raft command is simply the updated module data segment 
 *   -> This means application only ever runs once across the cluster
 *   -> provides a form of failover 
 *   -> side effects (e.g. writing local storage) only occur on the leader
 * - Application is scheduled to run with given input args.
 *   -> raft command is the input args
 *   -> All nodes in the cluster run it once the command is commited to the raft log 
 *   -> Guarantees side effects occur on all nodes
 * 
 * 
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <fju/fvm.h>
#include <fju/raft.h>
#include <fju/hostreg.h>
#include <fju/programs.h>
#include <fju/rpcd.h>

#include "fvm-private.h"

struct fvm_command_s {
  uint32_t progid;
  uint32_t mode;
#define FVM_MODE_UPDATESTATE 0   /* args are the updated state to apply */
#define FVM_MODE_RUN         1   /* run a given proc with specified args */
  union {
    struct {
      char *buf;
      int len;
    } updatestate;
    struct {
      uint64_t hostid;           /* if hostid=0 the given proc is run on all nodes, otherwise it is run only on the given node */
      uint32_t procid;           /* proc to run */
      char *args;                /* proc args */
      int len;
    } run;
  } u;
};

static int fvm_decode_command( struct xdr_s *xdr, struct fvm_command_s *x ) {
  int sts;
  sts = xdr_decode_uint32( xdr, &x->progid );
  if( !sts ) sts = xdr_decode_uint32( xdr, &x->mode );
  if( sts ) return sts;
  
  switch( x->mode ) {
  case FVM_MODE_UPDATESTATE:
    sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&x->u.updatestate.buf, &x->u.updatestate.len );
    break;
  case FVM_MODE_RUN:
    sts = xdr_decode_uint64( xdr, &x->u.run.hostid );
    if( !sts ) sts = xdr_decode_uint32( xdr, &x->u.run.procid );
    if( !sts ) sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&x->u.run.args, &x->u.run.len );
    break;
  }
  
  return sts;
}



static void fvm_command( struct raft_app *app, struct raft_cluster *cl, uint64_t seq, char *buf, int len ) {
  int sts;
  struct fvm_command_s cmd;
  struct xdr_s xdr;
  struct fvm_module *m;
    
  xdr_init( &xdr, (uint8_t *)buf, len );
  sts = fvm_decode_command( &xdr, &cmd );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "XDR error decoding command" );
    return;
  }

  m = fvm_module_by_progid( cmd.progid );
  if( !m ) {
    fvm_log( LOG_LVL_ERROR, "Unknown progid %u", cmd.progid );
    return;
  }
  
  switch( cmd.mode ) {
  case FVM_MODE_UPDATESTATE:
    if( cmd.u.updatestate.len != m->header.datasize ) {
      fvm_log( LOG_LVL_ERROR, "Bad datasize" );
      return;
    }
    fvm_log( LOG_LVL_TRACE, "fvm update data segment progid=%u", cmd.progid );
    memcpy( m->data, cmd.u.updatestate.buf, m->header.datasize );
    break;
  case FVM_MODE_RUN:
    {
      struct fvm_s state;

      fvm_log( LOG_LVL_TRACE, "fvm run progid=%u procid=%u", cmd.progid, cmd.u.run.procid );
      
      if( cmd.u.run.hostid == 0 ) {
	sts = fvm_state_init( &state, cmd.progid, cmd.u.run.procid );
	if( sts ) return;

	fvm_set_args( &state, cmd.u.run.args, cmd.u.run.len );
	fvm_run( &state, 0 );
      } else if( cmd.u.run.hostid == hostreg_localid() ) {
	/* run it here with a temp data segment, then forward the updated state to the leader */
	char *tmpdata;
	char tmpdatabuf[FVM_MAX_DATA + 12]; /* reserve some space for header */

	/* save data segment */
	tmpdata = (char *)m->data;

	/* set to temp data segment */
	memset( tmpdatabuf, 0, sizeof(tmpdatabuf) );
	memcpy( tmpdatabuf, tmpdata + 12, m->header.datasize );
	m->data = (uint8_t *)(tmpdatabuf + 12);

	/* run prog */
	sts = fvm_state_init( &state, cmd.progid, cmd.u.run.procid );
	if( sts ) {
	  fvm_log( LOG_LVL_ERROR, "failed to init state" );
	  m->data = (uint8_t *)tmpdata;
	  return;
	}
	
	fvm_set_args( &state, cmd.u.run.args, cmd.u.run.len );
	fvm_run( &state, 0 );
	
	/* restore data segment */
	m->data = (uint8_t *)tmpdata;

	/* send updated state to raft */
	xdr_init( &xdr, (uint8_t *)tmpdatabuf, sizeof(tmpdatabuf) );
	xdr_encode_uint32( &xdr, cmd.progid );
	xdr_encode_uint32( &xdr, FVM_MODE_UPDATESTATE );
	xdr_encode_uint32( &xdr, m->header.datasize );
	/* data follows, just update offset */
	xdr.offset += m->header.datasize;
	if( m->header.datasize % 4 ) xdr.offset += 4 - (m->header.datasize % 4);
	
	sts = raft_cluster_command( cl->clid, tmpdatabuf, xdr.offset, NULL );
      }
    }
    break;
  }
}


static struct raft_app fvm_app =
  {
   NULL,
   FVM_RPC_PROG,
   fvm_command,
   //   fvm_snapsave,  /* no need for snapshotting so we can ignore these */
   //   fvm_snapload,
  };


int fvm_cluster_run( uint64_t clid, uint32_t progid, uint32_t procid, char *args, int len ) {
  struct xdr_s buf;
  struct rpc_conn *c;
  int sts;
  
  if( clid == 0 ) {
    int i, n;
    uint64_t clidl[RAFT_MAX_CLUSTER];
    struct raft_cluster cl;
    
    n = raft_clid_list( clidl, RAFT_MAX_CLUSTER );
    for( i = 0; i < n; i++ ) {
      sts = raft_cluster_by_clid( clidl[i], &cl );
      if( sts ) continue;
      if( cl.appid == FVM_RPC_PROG ) {
	clid = clidl[i];
	break;
      }
    }

    if( clid == 0 ) return -1;
  }

  c = rpc_conn_acquire();
  if( !c ) return -1;

  xdr_init( &buf, (uint8_t *)c->buf, c->count );
  xdr_encode_uint32( &buf, progid );
  xdr_encode_uint32( &buf, FVM_MODE_RUN );
  xdr_encode_uint64( &buf, 0 );
  xdr_encode_uint32( &buf, procid );
  xdr_encode_opaque( &buf, (uint8_t *)args, len );
  sts = raft_cluster_command( clid, (char *)buf.buf, buf.offset, NULL );
  rpc_conn_release( c );
  
  return sts;
}

int fvm_cluster_updatestate( uint64_t clid, uint32_t progid ) {
  struct xdr_s buf;
  struct rpc_conn *c;
  int sts;
  struct fvm_module *m;
  
  m = fvm_module_by_progid( progid );
  if( !m ) return -1;
  
  if( clid == 0 ) {
    int i, n;
    uint64_t clidl[RAFT_MAX_CLUSTER];
    struct raft_cluster cl;
    
    n = raft_clid_list( clidl, RAFT_MAX_CLUSTER );
    for( i = 0; i < n; i++ ) {
      sts = raft_cluster_by_clid( clidl[i], &cl );
      if( sts ) continue;
      if( cl.appid == FVM_RPC_PROG ) {
	clid = clidl[i];
	break;
      }
    }

    if( clid == 0 ) return -1;
  }

  c = rpc_conn_acquire();
  if( !c ) return -1;

  xdr_init( &buf, (uint8_t *)c->buf, c->count );
  xdr_encode_uint32( &buf, progid );
  xdr_encode_uint32( &buf, FVM_MODE_UPDATESTATE );
  xdr_encode_opaque( &buf, (uint8_t *)m->data, m->header.datasize );
  sts = raft_cluster_command( clid, (char *)buf.buf, buf.offset, NULL );
  rpc_conn_release( c );
  
  return sts;
}

void fvm_cluster_register( void ) {
  raft_app_register( &fvm_app );
}

