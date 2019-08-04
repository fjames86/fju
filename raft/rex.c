/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
*/
 
#include <fju/rex.h>

#include <fju/rpc.h>
#include <fju/hrauth.h>
#include <fju/hostreg.h>
#include <fju/raft.h>
#include <fju/mmf.h>
#include <fju/rpcd.h>
#include <fju/rex.h>

#ifdef WIN32
#define PRIx64 "llx"
#define PRIu64 "llu"
#endif

static void rex_state_save( uint64_t clid, char *buf, int size ) {
  int sts;
  struct mmf_s mmf;
  char name[256];

  if( size > REX_MAX_BUF ) size = REX_MAX_BUF;
  
  sprintf( name, "%"PRIx64".dat", clid );
  mmf_ensure_dir( mmf_default_path( "rex", NULL ) );
  
  sts = mmf_open( mmf_default_path( "rex", name, NULL ), &mmf );  
  if( sts ) return;

  rpc_log( RPC_LOG_DEBUG, "rex_state_save: size=%u\n", size );
  mmf_write( &mmf, buf, size, 0 );
  mmf_truncate( &mmf, size );
  mmf_sync( &mmf );  
  mmf_close( &mmf );
  
  return;  
}

static void rex_state_load( uint64_t clid, char *buf, int *size ) {
  int sts;
  struct mmf_s mmf;
  char name[256];
  int nbytes;
  
  sprintf( name, "%"PRIx64".dat", clid );
  mmf_ensure_dir( mmf_default_path( "rex", NULL ) );
  
  sts = mmf_open( mmf_default_path( "rex", name, NULL ), &mmf );
  if( sts ) return;

  if( buf && (*size > 0) ) {
    nbytes = mmf.fsize;
    if( *size < nbytes ) nbytes = *size;
    if( nbytes > 0 ) mmf_read( &mmf, buf, nbytes, 0 );
  }
  *size = mmf.fsize;

  mmf_close( &mmf );
  return;    
}

static int rex_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int rex_proc_read( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t clid;
  int nbytes;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  nbytes = inc->xdr.count - inc->xdr.offset - 4;
  rex_state_load( clid, (char *)inc->xdr.buf + inc->xdr.offset + 4, &nbytes );
  xdr_encode_uint32( &inc->xdr, nbytes );
  if( nbytes % 4 ) nbytes += 4 - (nbytes % 4);
  inc->xdr.offset += nbytes;

  rpc_complete_accept_reply( inc, handle );
  return 0;
}


struct rex_ping_cxt {
  uint64_t clid;
  uint64_t hostid;
  uint64_t stateseq;
};

static void rex_call_ping_cb( struct xdr_s *xdr, void *cxt ) {
  struct rex_ping_cxt *pcxt = (struct rex_ping_cxt *)cxt;
  int sts;
  struct raft_member member;
  struct raft_cluster cl;
  
  if( !xdr ) {
    //rpc_log( RPC_LOG_DEBUG, "rex_call_ping_cb timeout" );
    goto done;
  }

  sts = raft_cluster_by_id( pcxt->clid, &cl );
  if( sts ) goto done;
  if( cl.state != RAFT_STATE_LEADER ) goto done;
  
  sts = raft_member_by_hostid( pcxt->clid, pcxt->hostid, &member );
  if( sts ) goto done;

  member.nextseq = cl.stateseq;
  member.stateseq = pcxt->stateseq;
  raft_member_set( &member );
  
 done:
  free( pcxt );
  return;
}

static void rex_call_ping( struct raft_cluster *cl, uint64_t hostid ) {
  int sts;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  struct rex_ping_cxt *pcxt;
  struct rpc_conn *tmpconn;
  int nbytes;
  
  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) return;
  
  pcxt = malloc( sizeof(*pcxt) );
  pcxt->clid = cl->id;
  pcxt->hostid = hostid;
  pcxt->stateseq = cl->stateseq;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = REX_RPC_PROG;
  hcall.vers = REX_RPC_VERS;
  hcall.proc = 3;
  hcall.donecb = rex_call_ping_cb;
  hcall.cxt = pcxt;
  hcall.timeout = 500;
  hcall.service = HRAUTH_SERVICE_PRIV;
  xdr_init( &xdr, tmpconn->buf, tmpconn->count );
  xdr_encode_uint64( &xdr, cl->id );
  xdr_encode_uint64( &xdr, hostreg_localid() );
  xdr_encode_uint64( &xdr, cl->termseq );
  xdr_encode_uint64( &xdr, cl->stateseq );
  xdr_encode_uint64( &xdr, cl->stateterm );

  nbytes = xdr.count - xdr.offset - 4;
  rex_state_load( cl->id, (char *)xdr.buf + xdr.offset + 4, &nbytes );
  xdr_encode_uint32( &xdr, nbytes );
  if( nbytes % 4 ) nbytes += 4 - (nbytes % 4);
  xdr.offset += nbytes;
  sts = hrauth_call_udp_async( &hcall, &xdr, NULL );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "raft_call_ping: hrauth_call failed" );
    free( pcxt );
  }

  rpc_conn_release( tmpconn );
}

static void rex_send_pings( struct raft_cluster *cl ) {
  int i, n;
  struct raft_member member[32];
  
  n = raft_member_list( cl->id, member, 32 );
  for( i = 0; i < n; i++ ) {
    rex_call_ping( cl, member[i].hostid );
  }

}

static int rex_proc_write( struct rpc_inc *inc ) {
  int handle, sts, len;
  uint64_t clid;
  struct raft_cluster cl;
  char *buf;  
  struct rpc_conn *tmpconn = NULL;
  struct rpc_msg msg;
  
  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) return -1;
  buf = (char *)tmpconn->buf;
  len = tmpconn->count;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_opaque( &inc->xdr, (uint8_t *)buf, &len );
  if( sts ) {
    rpc_conn_release( tmpconn );
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  }

  msg = inc->msg;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) {
    rpc_log( RPC_LOG_DEBUG, "no cluster" );
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, 0 );
    goto done;
  }

  if( cl.state != RAFT_STATE_LEADER ) {
    struct xdr_s args;
    uint8_t argbuf[64];

    if( !cl.leaderid ) {
      rpc_log( RPC_LOG_DEBUG, "no leader" );
      xdr_encode_boolean( &inc->xdr, 0 );
      xdr_encode_uint64( &inc->xdr, cl.leaderid );      
      goto done;
    }
    
    rpc_log( RPC_LOG_DEBUG, "sending proxy call" );
    xdr_init( &args, argbuf, sizeof(argbuf) );
    xdr_encode_uint64( &args, clid );
    xdr_encode_opaque( &args, (uint8_t *)buf, len );
    inc->msg = msg;
    sts = hrauth_call_udp_proxy( inc, cl.leaderid, &args );
    if( sts ) rpc_log( RPC_LOG_DEBUG, "proxy call failed!\n" );
    rpc_conn_release( tmpconn );
    return 1;
  }

  cl.stateterm = cl.termseq;
  cl.stateseq++;
  cl.commitseq = cl.stateseq;
  raft_cluster_set( &cl );

  rpc_log( RPC_LOG_DEBUG, "buf=%p len=%u", buf, len );
  rex_state_save( cl.id, buf, len );
  
  /* TODO: don't acknowlege until replicated on a quorum number of members? */

  rpc_log( RPC_LOG_DEBUG, "success?" );
  xdr_encode_boolean( &inc->xdr, 1 );
  xdr_encode_uint64( &inc->xdr, cl.leaderid );
  
 done:

  rpc_complete_accept_reply( inc, handle );
  rpc_conn_release( tmpconn );
  
  rex_send_pings( &cl );
  
  return 0;
}

static int rex_proc_ping( struct rpc_inc *inc ) {
  int handle, sts, len;
  uint64_t clid, termseq, leaderid, stateseq, stateterm;
  struct raft_cluster cl;
  struct rpc_conn *tmpconn = NULL;
  char *buf;
    
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &leaderid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &termseq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &stateseq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &stateterm );

  tmpconn = rpc_conn_acquire();
  if( !tmpconn ) return -1;
  buf = (char *)tmpconn->buf;
  len = tmpconn->count;
  if( !sts ) sts = xdr_decode_opaque( &inc->xdr, (uint8_t *)buf, &len );
  if( sts ) {
    rpc_conn_release( tmpconn );
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  }
	      
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, 0 );
    goto done;
  }

  if( cl.state == RAFT_STATE_LEADER ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, cl.leaderid );
    goto done;
  }

  if( cl.leaderid != leaderid ) {
    /* bad leader */
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, cl.leaderid );
    goto done;
  }
  
  if( termseq != cl.termseq ) {
    /* bad term seq */
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, cl.leaderid );
    goto done;
  }

  if( stateseq < cl.stateseq ) {
    /* old state, ignore */
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, cl.leaderid );
    goto done;
  }
  
  /* all good, write state */
  xdr_encode_boolean( &inc->xdr, 1 );
  xdr_encode_uint64( &inc->xdr, cl.leaderid );

  cl.commitseq = stateseq;
  cl.stateseq = stateseq;
  cl.stateterm = stateterm;
  raft_cluster_set( &cl );

  rex_state_save( cl.id, buf, len );  

 done:

  rpc_complete_accept_reply( inc, handle );
  rpc_conn_release( tmpconn );
  return 0;
}

static struct rpc_proc rex_procs[] = {
  { 0, rex_proc_null },
  { 1, rex_proc_read },
  { 2, rex_proc_write },
  { 3, rex_proc_ping },
  { 0, NULL }
};

static struct rpc_version rex_vers = {
  NULL, REX_RPC_VERS, rex_procs
};

static struct rpc_program rex_prog = {
  NULL, REX_RPC_PROG, &rex_vers
};


static void rex_notify( raft_notify_t evt, struct raft_cluster *cl, void *cxt, void *reserved ) {

    switch( evt ) {
    case RAFT_NOTIFY_LEADER:
    case RAFT_NOTIFY_SEND_PING:
	/* resend data whenever we become leader or are sending raft ping messages */
	rex_send_pings( cl );
    default:
	break;
    }
  
}

static struct raft_notify_context rex_notify_cxt = {
  NULL,
  rex_notify,
  NULL
};

void rex_register( void ) {
  rpc_program_register( &rex_prog );
  raft_notify_register( &rex_notify_cxt );  
}
