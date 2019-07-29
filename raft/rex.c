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
 
#include "rex.h"

#include <rpc.h>
#include <hrauth.h>
#include <hostreg.h>
#include "raft.h"
#include <mmf.h>
#include <rpcd.h>
#include <rex.h>

#ifdef WIN32
#define PRIx64 "llx"
#define PRIu64 "llu"
#endif

struct rex_state {
  uint64_t termseq;
  uint64_t stateseq;
  uint8_t buf[REX_MAX_BUF];
};

static void rex_state_save( uint64_t clid, struct rex_state *state ) {
  int sts;
  struct mmf_s mmf;
  char name[256];
  
  sprintf( name, "%"PRIx64".dat", clid );
  mmf_ensure_dir( mmf_default_path( "rex", NULL ) );
  sts = mmf_open( mmf_default_path( "rex", name, NULL ), &mmf );
  if( sts ) return;

  sts = mmf_remap( &mmf, sizeof(*state) );
  if( sts ) goto done;

  memcpy( mmf.file, state, sizeof(*state) );
  mmf_sync( &mmf );
 done:
  mmf_close( &mmf );
  return;  
}

static void rex_state_load( uint64_t clid, struct rex_state *state ) {
  int sts;
  struct mmf_s mmf;
  char name[256];

  memset( state, 0, sizeof(*state) );

  sprintf( name, "%"PRIx64".dat", clid );
  mmf_ensure_dir( mmf_default_path( "rex", NULL ) );
  sts = mmf_open( mmf_default_path( "rex", name, NULL ), &mmf );
  if( sts ) return;

  sts = mmf_remap( &mmf, sizeof(*state) );
  if( sts ) goto done;

  memcpy( state, mmf.file, sizeof(*state) );
  
 done:
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
  struct raft_cluster cl;
  struct rex_state state;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rex_state_load( clid, &state );
  xdr_encode_opaque( &inc->xdr, (uint8_t *)state.buf, REX_MAX_BUF );
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
  uint8_t xdr_buf[256];
  struct rex_ping_cxt *pcxt;
  struct rex_state state;
  
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
  xdr_init( &xdr, xdr_buf, sizeof(xdr_buf) );
  xdr_encode_uint64( &xdr, cl->id );
  xdr_encode_uint64( &xdr, hostreg_localid() );
  xdr_encode_uint64( &xdr, cl->termseq );
  xdr_encode_uint64( &xdr, cl->stateseq );
  xdr_encode_uint64( &xdr, cl->stateterm );

  rex_state_load( cl->id, &state );
  xdr_encode_opaque( &xdr, (uint8_t *)state.buf, REX_MAX_BUF );
  sts = hrauth_call_udp( &hcall, &xdr );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "raft_call_ping: hrauth_call failed" );
    free( pcxt );
  }

}

static void rex_send_pings( struct raft_cluster *cl ) {
  int i, n;
  struct raft_member member[32];
  
  n = raft_member_list( cl->id, member, 32 );
  for( i = 0; i < n; i++ ) {
    rex_call_ping( cl, member[i].hostid );
  }

}

#if 0

struct rex_proxy_cxt {
  uint32_t xid;
  struct rpc_provider *pvr;
  void *pcxt;
  struct sockaddr_storage raddr;
  uint32_t raddr_len;
  uint64_t leaderid;
};

static void rex_proxy_write_cb( struct xdr_s *xdr, void *cxt ) {
  struct rex_proxy_cxt *pcxt = (struct rex_proxy_cxt *)cxt;
  struct rpc_listen *listen;
  int sts, handle;
  struct rpc_inc inc;
  uint8_t buf[1024];
  
  /* send reply message */
  memset( &inc, 0, sizeof(inc) );
  inc.pvr = pcxt->pvr;
  inc.pcxt = pcxt->pcxt;
  xdr_init( &inc.xdr, buf, sizeof(buf) );
  rpc_init_accept_reply( &inc, pcxt->xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  if( xdr ) {
    xdr_encode_fixed( &inc.xdr, xdr->buf + xdr->offset, xdr->count - xdr->offset );
  } else {
    xdr_encode_boolean( &inc.xdr, 0 );
    xdr_encode_boolean( &inc.xdr, pcxt->leaderid );
  }
  rpc_complete_accept_reply( &inc, handle );

  listen = rpcd_listen_by_type( RPC_LISTEN_UDP );
  if( listen ) sendto( listen->fd, inc.xdr.buf, inc.xdr.offset, 0, (struct sockaddr *)&pcxt->raddr, pcxt->raddr_len );
  
 done:
  free( pcxt );
  return;
}

static int rex_proxy_write( uint64_t hostid, struct xdr_s *args, struct rpc_inc *inc ) {
  int sts;
  struct hrauth_call hcall;
  struct rex_proxy_cxt *pcxt;

  pcxt = malloc( sizeof(*pcxt) );
  pcxt->xid = inc->msg.xid;
  pcxt->pvr = inc->pvr;
  pcxt->pcxt = inc->pcxt;
  memcpy( &pcxt->raddr, &inc->raddr, inc->raddr_len );
  pcxt->raddr_len = inc->raddr_len;
  pcxt->leaderid = hostid;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = REX_RPC_PROG;
  hcall.vers = REX_RPC_VERS;
  hcall.proc = 2;
  hcall.donecb = rex_proxy_write_cb;
  hcall.cxt = pcxt;
  hcall.timeout = 500;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_udp( &hcall, args );
  if( sts ) {
    free( pcxt );
  }

  return sts;
}
#endif

static int rex_proc_write( struct rpc_inc *inc ) {
  int handle, sts, len;
  uint64_t clid;
  struct raft_cluster cl;
  uint8_t buf[REX_MAX_BUF];
  struct rex_state state;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  len = REX_MAX_BUF;
  if( !sts ) sts = xdr_decode_opaque( &inc->xdr, buf, &len );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
	      
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  sts = raft_cluster_by_id( clid, &cl );
  if( sts ) {
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, 0 );
    goto done;
  }

  if( cl.state != RAFT_STATE_LEADER ) {
#if 0
    xdr_encode_boolean( &inc->xdr, 0 );
    xdr_encode_uint64( &inc->xdr, cl.leaderid );
#else
    struct xdr_s args;
    uint8_t argbuf[64];

    if( !cl.leaderid ) {
      xdr_encode_boolean( &inc->xdr, 0 );
      xdr_encode_uint64( &inc->xdr, cl.leaderid );      
      goto done;
    }
    
    xdr_init( &args, argbuf, sizeof(argbuf) );
    xdr_encode_uint64( &args, clid );
    xdr_encode_opaque( &args, buf, len );
    hrauth_call_proxy( inc, cl.leaderid, &args );
//    rex_proxy_write( cl.leaderid, &args, inc );
    return 1;
#endif
    goto done;
  }

  cl.stateterm = cl.termseq;
  cl.stateseq++;
  cl.commitseq = cl.stateseq;
  raft_cluster_set( &cl );

  if( len > REX_MAX_BUF ) len = REX_MAX_BUF;
  memset( &state, 0, sizeof(state) );
  state.termseq = cl.termseq;
  state.stateseq = cl.stateseq;
  memcpy( state.buf, buf, len );
  rex_state_save( cl.id, &state );
  
  /* TODO: don't acknowlege until replicated on a quorum number of members? */
  
  xdr_encode_boolean( &inc->xdr, 1 );
  xdr_encode_uint64( &inc->xdr, cl.leaderid );
  
 done:

  rpc_complete_accept_reply( inc, handle );

  rex_send_pings( &cl );
  
  return 0;
}

static int rex_proc_ping( struct rpc_inc *inc ) {
  int handle, sts, len;
  uint64_t clid, termseq, leaderid, stateseq, stateterm;
  struct raft_cluster cl;
  uint8_t buf[REX_MAX_BUF];
  struct rex_state state;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &leaderid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &termseq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &stateseq );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &stateterm );
  len = REX_MAX_BUF;
  if( !sts ) sts = xdr_decode_opaque( &inc->xdr, buf, &len );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
	      
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

  if( len > REX_MAX_BUF ) len = REX_MAX_BUF;
  memset( &state, 0, sizeof(state) );
  state.termseq = cl.termseq;
  state.stateseq = cl.stateseq;
  memcpy( state.buf, buf, len );
  rex_state_save( cl.id, &state );
  

 done:

  rpc_complete_accept_reply( inc, handle );
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
