
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
#include "fvm-private.h"

struct fvm_command_s {
  uint32_t progid;
  uint32_t mode;
#define FVM_MODE_UPDATESTATE 0   /* args are the updated state to apply */
#define FVM_MODE_COMMAND     1   /* args are commnd to be applied to given proc */
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
    } command;
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
  case FVM_MODE_COMMAND:
    sts = xdr_decode_uint64( xdr, &x->u.command.hostid );
    if( !sts ) sts = xdr_decode_uint32( xdr, &x->u.command.procid );
    if( !sts ) sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&x->u.command.args, &x->u.command.len );
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
  case FVM_MODE_COMMAND:
    {
      struct fvm_s state;

      fvm_log( LOG_LVL_TRACE, "fvm apply command progid=%u", cmd.progid );
      
      if( cmd.u.command.hostid == 0 ) {
	sts = fvm_state_init( &state, cmd.progid, cmd.u.command.procid );
	if( sts ) return;

	fvm_set_args( &state, cmd.u.command.args, cmd.u.command.len );
	fvm_run( &state, 0 );
      } else if( cmd.u.command.hostid == hostreg_localid() ) {
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
	sts = fvm_state_init( &state, cmd.progid, cmd.u.command.procid );
	if( sts ) {
	  fvm_log( LOG_LVL_ERROR, "failed to init state" );
	  m->data = (uint8_t *)tmpdata;
	  return;
	}
	
	fvm_set_args( &state, cmd.u.command.args, cmd.u.command.len );
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
   //   fvm_snapsave,
   //   fvm_snapload,
  };


void fvm_cluster_register( void ) {
  raft_app_register( &fvm_app );
}

