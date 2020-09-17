
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include <fju/cht.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/programs.h>
#include <fju/hrauth.h>
#include <fju/nls.h>

#include "cht-private.h"


/*
 * This implements a form of remote sync for cht tables
 *  - The keys for all cht writes are written to a log 
 *  - that log is shared using nls 
 *  - The nls client (remote machine) reads these entries and writes locally 
 */

/* reprents both client side (nls_remote) and server side (nls_share) */
struct cht_rsync_context {
  struct cht_rsync_context *next;

  int type;
#define CHT_RSYNC_LOCAL 0
#define CHT_RSYNC_REMOTE 1
  uint64_t hshare;
  struct cht_s cht;
  uint64_t log_id;
  uint64_t next_id;
  uint64_t hostid;
};
static struct cht_rsync_context *cht_contexts;
static struct cht_rsync_context *cht_rsync_by_hshare( uint64_t hshare, int type ) {
  struct cht_rsync_context *c;
  c = cht_contexts;
  while( c ) {
    if( c->hshare == hshare ) return c;
    c = c->next;
  }
  return NULL;
}

static void call_read_donecb( struct xdr_s *xdr, void *pcxt ) {
  /* write result back into local table */
  struct cht_rsync_context *cxt = (struct cht_rsync_context *)pcxt;
  struct cht_entry entry;
  char *bufp;
  int lenp, sts;
  
  if( !xdr ) {
    /* timeout */
    return;
  }

  sts = xdr_decode_fixed( xdr, entry.key, CHT_KEY_SIZE );
  if( !sts ) sts = xdr_decode_uint32( xdr, &entry.flags );
  if( !sts ) sts = xdr_decode_uint32( xdr, &entry.seq );
  if( !sts ) sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return;
  
  sts = cht_write( &cxt->cht, &entry, bufp, lenp );

  cxt->log_id = cxt->next_id;
}

static void cht_rsync_call_read( struct cht_rsync_context *cxt, struct cht_entry *entry ) {
  /* send call to remote host to read the given entry */
  struct hrauth_call hcall;
  char xdrbuf[64];
  struct xdr_s xdr;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = cxt->hostid;
  hcall.prog = CHT_RSYNC_RPC_PROG;
  hcall.vers = CHT_RSYNC_RPC_VERS;
  hcall.proc = 1;
  hcall.timeout = 500;
  hcall.service = HRAUTH_SERVICE_PRIV;
  hcall.donecb = call_read_donecb;
  hcall.cxt = cxt;

  xdr_init( &xdr, (uint8_t *)xdrbuf, sizeof(xdrbuf) );
  xdr_encode_uint64( &xdr, cxt->hshare );
  xdr_encode_fixed( &xdr, entry->key, CHT_KEY_SIZE );
  hrauth_call_udp_async( &hcall, &xdr, NULL );
}

static int cht_alog_read( struct cht_s *cht, uint64_t log_id, uint32_t *op, struct cht_entry *entry, uint64_t *next_id ) {
  struct log_entry le;
  struct log_iov iov[2];
  int sts, ne;
  
  memset( &le, 0, sizeof(le) );
  iov[0].buf = (char *)op;
  iov[0].len = sizeof(*op);
  iov[1].buf = (char *)entry;
  iov[1].len = sizeof(*entry);
  sts = log_read( &cht->alog, log_id, &le, 1, &ne );
  if( sts || !ne ) return -1;

  *next_id = le.id;
  
  return 0;
}

static void cht_rsync_update( uint64_t hshare ) {
  int sts;
  struct cht_rsync_context *cxt;
  uint32_t op;
  struct cht_entry centry;
  
  /* get table for this hshare */
  cxt = cht_rsync_by_hshare( hshare, CHT_RSYNC_REMOTE );
  if( !cxt ) return;

  sts = cht_alog_read( &cxt->cht, cxt->log_id, &op, &centry, &cxt->next_id );
  if( sts ) return;

  switch( op ) {
  case CHT_ALOG_OP_WRITE:    
  
    /* issue rpc back to remote host to read the new entry */
    cht_rsync_call_read( cxt, &centry );
    return;
    
    break;
  case CHT_ALOG_OP_DELETE:
    cht_delete( &cxt->cht, (char *)centry.key );
    break;
  case CHT_ALOG_OP_SETFLAGS:
    cht_set_flags( &cxt->cht, (char *)centry.key, centry.flags << 16, centry.flags & 0xffff0000 );
    break;
  case CHT_ALOG_OP_PURGE:
    cht_purge( &cxt->cht, centry.flags << 16, centry.flags & 0xffff0000 );    
    break;
  }

  cxt->log_id = cxt->next_id;
  return;
}


static int cht_rsync_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int cht_rsync_proc_read( struct rpc_inc *inc ) {
  int handle, sts;
  char key[CHT_KEY_SIZE];
  uint64_t hshare;
  struct cht_rsync_context *cht;
  struct cht_entry entry;
  
  sts = xdr_decode_uint64( &inc->xdr, &hshare );
  if( !sts ) sts = xdr_decode_fixed( &inc->xdr, (uint8_t *)key, CHT_KEY_SIZE );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
    
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  cht = cht_rsync_by_hshare( hshare, CHT_RSYNC_LOCAL );  
  if( cht ) {
    /* 
     * Read by passing a buffer that points into the xdr buffer at the exact location
     * we need. We can do this because the header is fixed size (32 bytes).
     */
    sts = cht_read( &cht->cht, key, (char *)inc->xdr.buf + inc->xdr.offset + 32, CHT_BLOCK_SIZE, &entry );
    if( sts ) {
      xdr_encode_boolean( &inc->xdr, 0 );
    } else {
      xdr_encode_boolean( &inc->xdr, 1 ); /*4*/
      xdr_encode_fixed( &inc->xdr, entry.key, CHT_KEY_SIZE ); /*20*/
      xdr_encode_uint32( &inc->xdr, entry.flags ); /*24*/
      xdr_encode_uint32( &inc->xdr, entry.seq ); /*28*/
      xdr_encode_uint32( &inc->xdr, entry.flags & CHT_SIZE_MASK );/*32*/
    }
  } else {
    xdr_encode_boolean( &inc->xdr, 0 );
  }
  
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct rpc_proc cht_rsync_procs[] = {
  { 0, cht_rsync_proc_null },
  { 1, cht_rsync_proc_read },
  
  { 0, NULL }
};

static struct rpc_version cht_rsync_vers = {
  NULL, CHT_RSYNC_RPC_VERS, cht_rsync_procs
};

static struct rpc_program cht_rsync_prog = {
  NULL, CHT_RSYNC_RPC_PROG, &cht_rsync_vers
};

static struct rpcd_subscriber cht_rsync_nlsevent;
static uint32_t cht_rsync_nlsevents[] = { NLS_RPC_PROG, 0 };

static void cht_rsync_nlsevent_cb( struct rpcd_subscriber *sc, uint32_t cat, uint32_t evt, void *parm, int parmsize ) {
  
  if( cat != NLS_RPC_PROG ) return;
  switch( evt ) {
  case NLS_EVENT_REMOTEAPPEND:
    {
      struct nls_remote *remote = (struct nls_remote *)parm;

      /* find the cht this remote is associated with */
      cht_rsync_update( remote->hshare );
      
    }
    break;
  }
}

void cht_rsync_initialize( void ) {

  /* read shares from registry */



  
  /* register rpc program */
  rpc_program_register( &cht_rsync_prog );

  /* register to receive nls events */
  cht_rsync_nlsevent.category = cht_rsync_nlsevents;
  cht_rsync_nlsevent.cb = cht_rsync_nlsevent_cb;
  rpcd_event_subscribe( &cht_rsync_nlsevent );
}
