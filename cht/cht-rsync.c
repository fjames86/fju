
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include <fju/cht.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>

#include "cht-private.h"


/*
 * This implements a form of remote sync for cht tables
 *  - The keys for all cht writes are written to a log 
 *  - that log is shared using nls 
 *  - The nls client (remote machine) reads these entries and writes locally 
 */

struct cht_rsync_context {
  struct cht_rsync_context *next;
  
  uint64_t hshare;
  struct cht_s cht;
};
static struct cht_rsync_context *cht_contexts;
static struct cht_s *cht_rsync_by_hshare( uint64_t hshare ) {
  struct cht_rsync_context *c;
  c = cht_contexts;
  while( c ) {
    if( c->hshare == hshare ) return &c;
    c = c->next;
  }
  return NULL;
}

static void call_read_donecb( void ) {
  /* write result back into local table */   
}

static void cht_rsync_call_read( uint64_t hostid, char *key ) {
  /* send call to remote host to read the given entry */
}

static void cht_rsync_update( uint64_t hshare ) {
  /* get table for this hshare */
  cht = cht_rsync_by_hshare( hshare );
  if( !cht ) return;
  
  /* read new entry from log */
  sts = log_read( &cht->cht.alog, cht->log_id, &entry, 1, &ne );
  if( sts || !ne ) return;

  cht->log_id = entry.id;

  switch( entry.op ) {
  case CHT_ALOG_OP_WRITE:    
  
    /* issue rpc back to remote host to read the new entry */
    cht_sync_call_read( cht->hostid, entry.key );

    break;
  case CHT_ALOG_OP_DELETE:
    cht_delete( &cht->cht, entry.key );
    break;
  case CHT_ALOG_OP_SETFLAGS:
    cht_set_flags( &cht->cht, entry.key, entry.flags << 16, entry.flags & 0xffff0000 );
    break;
  case CHT_ALOG_OP_PURGE:
    cht_purge( &cht->cht, entry.flags << 16, entry.flags & 0xffff0000 );    
    break;
  }

}


static int cht_rsync_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct rpc_proc cht_rsync_procs[] = {
  { 0, cht_rsync_proc_null },
  
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
      cht = cht_rsync_by_hshare( remote->hshare );
      cht_rsync_update( cht );
      
    }
    break;
  }
}

void cht_rsync_initialize( void ) {
  rpc_program_register( &cht_rsync_prog );

  cht_rsync_nlsevent.category = cht_rsync_nlsevents;
  cht_rsync_nlsevent.cb = cht_rsync_nlsevent_cb;
  rpcd_event_subscribe( &cht_rsync_nlsevent );
}
