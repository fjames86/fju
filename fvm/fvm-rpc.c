

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/fvm.h>
#include <fju/sec.h>
#include <fju/log.h>
#include <fju/nls.h>
#include <fju/freg.h>

struct loaded_fvm {
  struct loaded_fvm *next;
  uint32_t id;
  uint32_t flags;
#define FVM_RPC_AUTOUNLOAD 0x0001
#define FVM_RPC_INLOG      0x0002 
#define FVM_RPC_OUTLOG     0x0004
  struct fvm_state fvm;
  uint64_t inlog_id;
  struct log_s inlog;
  uint64_t outlog_id;
  struct log_s outlog;
  uint64_t runtime;
  char name[64];
};

struct event_prog {
  uint32_t category;
  uint32_t eventid;
  uint64_t progdata;
  uint32_t flags;
  uint64_t inlogid;
  uint64_t outlogid;
};

static struct {
  struct loaded_fvm *progs;
  struct rpcd_subscriber subsc;
  int nevtprogs;
#define FVM_MAX_EVTPROGS 64
  struct event_prog evtprogs[FVM_MAX_EVTPROGS];
} glob;

static void fvm_iter_cb( struct rpc_iterator *iter );
static void reload_evtprogs( struct rpc_iterator *iter );

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

static struct loaded_fvm *fvm_load_prog( uint8_t *bufp, int buflen, int start, uint32_t flags, uint64_t inid, uint64_t outid, char *name ) {
    int sts;
    struct loaded_fvm *lf;
    struct nls_share nls;
    
    lf = malloc( sizeof(*lf) );
    memset( lf, 0, sizeof(*lf) );
    lf->id = sec_rand_uint32();
    strncpy( lf->name, name, sizeof(lf->name) - 1 );
    sts = fvm_load( &lf->fvm, (char *)bufp, buflen );
    if( sts ) {
	free( lf );
	return NULL;
    }
    
    if( flags & FVM_RPC_INLOG ) {
	sts = nls_share_by_hshare( inid, &nls );
	if( !sts ) sts = nls_share_open( &nls, &lf->inlog );
	if( sts ) flags &= ~FVM_RPC_INLOG;
	else {
	    lf->fvm.inlog = &lf->inlog;
	    lf->inlog_id = inid;
	}
	lf->fvm.inlog_id = 0;
    }
    
    if( flags & FVM_RPC_OUTLOG ) {
	sts = nls_share_by_hshare( outid, &nls );
	if( !sts ) sts = nls_share_open( &nls, &lf->outlog );
	if( sts ) flags &= ~FVM_RPC_OUTLOG;
	else {
	    lf->fvm.outlog = &lf->outlog;
	    lf->outlog_id = outid;
	}
    }
    lf->flags = flags;
    
    /* push onto list */
    lf->next = glob.progs;
    glob.progs = lf;
    
    /* ensure the iterator runs immediately */
    fvm_iter.timeout = 0;
    if( start ) lf->fvm.flags |= FVM_FLAG_RUNNING;
    else lf->fvm.flags &= ~FVM_FLAG_RUNNING;

    return lf;
}

static int fvm_proc_load( struct rpc_inc *inc ) {
  int handle, buflen, sts, start;
  char *bufp;
  struct loaded_fvm *lf;
  uint32_t flags;
  uint64_t inid, outid;
  char name[64];
	    
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &buflen );
  if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &start );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &inid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &outid );
  if( !sts ) sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) {
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  }

  lf = fvm_load_prog( (uint8_t *)bufp, buflen, start, flags, inid, outid, name );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_uint32( &inc->xdr, lf ? lf->id : 0);
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
      if( lf->flags & FVM_RPC_INLOG ) log_close( &lf->inlog );
      if( lf->flags & FVM_RPC_OUTLOG ) log_close( &lf->outlog );
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
    xdr_encode_uint64( &inc->xdr, lf->fvm.tickcount );
    xdr_encode_uint64( &inc->xdr, lf->runtime );
    xdr_encode_uint64( &inc->xdr, lf->inlog_id );
    xdr_encode_uint64( &inc->xdr, lf->outlog_id );
    xdr_encode_string( &inc->xdr, lf->name );
    lf = lf->next;    
  }
  xdr_encode_boolean( &inc->xdr, 0 );

  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}


static int fvm_proc_pause( struct rpc_inc *inc ) {
  int handle, sts;
  struct loaded_fvm *lf;
  uint32_t id, stop;

  sts = xdr_decode_uint32( &inc->xdr, &id );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &stop );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );  
  lf = glob.progs;
  while( lf ) {
      if( lf->id == id ) {
	  if( stop == 0 ) {
	      /* continue */
	      lf->fvm.flags |= FVM_FLAG_RUNNING;
	  } else if( stop == 1 ) {
	      /* stop running */
	      lf->fvm.flags &= ~FVM_FLAG_RUNNING;
	  }
	  else if( stop == 2 ) {
	      /* reset */
	      fvm_reset( &lf->fvm );
	      lf->fvm.flags |= FVM_FLAG_RUNNING;
	  } else {
	      /* other */
	  }
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

void fvm_rpc_force_iter( void ) {
  fvm_iter.timeout = 0;
}

static void fvm_iter_cb( struct rpc_iterator *iter ) {
  struct loaded_fvm *lf, *prev, *next;
  uint64_t timeout, start;

  timeout = iter->timeout;
  lf = glob.progs;
  prev = NULL;
  while( lf ) {
    next = lf->next;

    start = rpc_now();
    fvm_run_nsteps( &lf->fvm, 1000 );
    lf->runtime += rpc_now() - start;
    if( lf->fvm.flags & FVM_FLAG_RUNNING ) {
      /* program still needs to run, but we yield here and schedule ourselves to be run again ASAP */
      timeout = 0;
      prev = lf;
    } else if( (lf->fvm.flags & FVM_FLAG_DONE) ) {
	rpcd_event_publish( FVM_EVENT_CATEGORY, FVM_EVENT_PROGDONE, &lf->id );
	if( lf->flags & FVM_RPC_AUTOUNLOAD ) {
	    if( prev ) prev->next = next;
	    else glob.progs = next;
	    if( lf->flags & FVM_RPC_INLOG ) log_close( &lf->inlog );
	    if( lf->flags & FVM_RPC_OUTLOG ) log_close( &lf->outlog );
	    free( lf );
	}
    } else if( lf->fvm.sleep_timeout && (rpc_now() >= lf->fvm.sleep_timeout) ) {
	lf->fvm.flags |= FVM_FLAG_RUNNING;
	lf->fvm.sleep_timeout = 0;
	timeout = 0;
    } else {
	/* stopped running but not set to auto unload when stopped */
	prev = lf;
    }
    lf = next;
  }

  iter->timeout = timeout;
}

static void load_startup_prog( struct freg_entry *entry ) {
    int sts;
    char *progdata;
    char path[256];
    struct mmf_s mmf;
    int proglen, start;
    uint32_t flags;
    uint64_t inlog, outlog;
    int len;
    uint64_t id;

    id = entry->id;
    progdata = NULL;
    
    sts = freg_get_by_name( NULL, id, "progdata", FREG_TYPE_OPAQUE, NULL, 0, &len );
    if( sts ) {
	/* try loading by path */
	sts = freg_get_by_name( NULL, id, "path", FREG_TYPE_STRING, path, sizeof(path), NULL );
	if( sts ) return;

	sts = mmf_open2( path, &mmf, MMF_OPEN_EXISTING );
	if( sts ) {
	  log_writef( NULL, LOG_LVL_ERROR, "mmf_open2 failed \"%s\"", path );
	} else {
	  sts = mmf_remap( &mmf, mmf.fsize );
	  if( sts ) {
	    log_writef( NULL, LOG_LVL_ERROR, "mmf_remap failed" );
	  } else {
	    progdata = malloc( mmf.fsize );
	    proglen = mmf.fsize;
	    
	    memcpy( progdata, mmf.file, mmf.fsize );
	  }
	  
	  mmf_close( &mmf );
	}
    } else {
	/* load from program data directly */
        proglen = len;
	progdata = malloc( proglen );
	sts = freg_get_by_name( NULL, id, "progdata", FREG_TYPE_OPAQUE, progdata, proglen, &proglen );
	if( sts ) {
	  log_writef( NULL, LOG_LVL_ERROR, "Unexpected failure reading progdata" );
	  free( progdata );
	  return;
	}
    }
    if( !progdata ) return;
    
    /* get other params or use defaults */
    start = 1;
    flags = FVM_RPC_AUTOUNLOAD;
    inlog = 0;
    outlog = 0;
    
    freg_get_by_name( NULL, id, "start", FREG_TYPE_UINT32, (char *)&start, sizeof(start), NULL );
    freg_get_by_name( NULL, id, "flags", FREG_TYPE_UINT32, (char *)&flags, sizeof(flags), NULL );
    freg_get_by_name( NULL, id, "inlog", FREG_TYPE_UINT64, (char *)&inlog, sizeof(inlog), NULL );
    freg_get_by_name( NULL, id, "outlog", FREG_TYPE_UINT64, (char *)&outlog, sizeof(outlog), NULL );

    fvm_load_prog( (uint8_t *)progdata, proglen, start, flags, inlog, outlog, entry->name );
    log_writef( NULL, LOG_LVL_INFO, "Loaded startup program \"%s\" len=%d start=%s flags=%x inlog=%"PRIx64" outlog=%"PRIx64"",
		entry->name, proglen, start ? "true" : "false", flags, inlog, outlog );
    
    free( progdata );
    progdata = NULL;
    
}

static void load_startup_progs( void ) {
    int sts;
    uint64_t fvmid, id;
    struct freg_entry entry;
    
    sts = freg_subkey( NULL, 0, "/fju/fvm/startup", FREG_CREATE, &fvmid );
    if( sts ) return;

    id = 0;
    do {
	sts = freg_next( NULL, fvmid, id, &entry );
	if( sts ) break;
	if( (entry.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) goto cont;

	log_writef( NULL, LOG_LVL_INFO, "Loading startup program \"%s\"", entry.name );
	load_startup_prog( &entry );
    cont:
	id = entry.id;
    } while( 1 );
}


static int fvm_rpc_runprogram( struct event_prog *ep ) {
    int sts;
    uint32_t flags;
    char name[32];
    char *progdata;
    int proglen, len;
    
    sts = freg_get( NULL, ep->progdata, &flags, NULL, 0, &len );
    if( sts ) {
      log_writef( NULL, LOG_LVL_ERROR, "event progdata missing" );
      return -1;
    }
    
    if( (flags & FREG_TYPE_MASK) != FREG_TYPE_OPAQUE ) {
      log_writef( NULL, LOG_LVL_ERROR, "event progdata not opaque" );
      return -1;
    }
    
    proglen = len;
    progdata = malloc( proglen );
    sts = freg_get( NULL, ep->progdata, NULL, progdata, len, NULL );
    if( sts ) {
      log_writef( NULL, LOG_LVL_ERROR, "Unexpected failure reading progdata" );
      free( progdata );
      return -1;
    }

    sprintf( name, "evt:%u:%u", ep->category, ep->eventid );
    fvm_load_prog( (uint8_t *)progdata, proglen, 1, ep->flags, ep->inlogid, ep->outlogid, name );
    log_writef( NULL, LOG_LVL_INFO, "Loaded event program category=%u eventid=%u len=%d inlog=%"PRIx64" outlog=%"PRIx64"",
		ep->category, ep->eventid, proglen, ep->inlogid, ep->outlogid );
    
    free( progdata );

    return 0;
}

static void fvm_evt_cb( struct rpcd_subscriber *sc, uint32_t category, uint32_t id, void *parm ) {
  struct event_prog *ep;
  int i;
  
  (void)(parm); /* currently unused */

  for( i = 0; i < glob.nevtprogs; i++ ) {
    ep = &glob.evtprogs[i];
    
    if( (ep->category == category) && (ep->eventid == id) ) {
      log_writef( NULL, LOG_LVL_INFO, "fvm_evt_cb category=%u eventid=%u freg_id=%"PRIx64"", category, id, ep->progdata );
      fvm_rpc_runprogram( ep );
    }
    
  }
  
}

static void reload_evtprogs( struct rpc_iterator *iter ) {
  int sts, i;
  uint64_t fvmid, id;
  struct freg_entry entry, tlentry;

  (void)(iter);
  
  glob.nevtprogs = 0;
  
  sts = freg_subkey( NULL, 0, "/fju/fvm/event", FREG_CREATE, &fvmid );
  if( sts ) return;

  id = 0;
  do {
    sts = freg_next( NULL, fvmid, id, &entry );
    if( sts ) break;
    if( (entry.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) goto cont;
    tlentry = entry;
    
    log_writef( NULL, LOG_LVL_TRACE, "Loading event trigger program \"%s\"", entry.name );

    i = glob.nevtprogs;
    sts = freg_get_by_name( NULL, tlentry.id, "category", FREG_TYPE_UINT32, (char *)&glob.evtprogs[i].category, sizeof(uint32_t), NULL );
    
    if( !sts ) freg_get_by_name( NULL, tlentry.id, "eventid", FREG_TYPE_UINT32, (char *)&glob.evtprogs[i].eventid, sizeof(uint32_t), NULL );
    if( !sts ) {
      sts = freg_entry_by_name( NULL, tlentry.id, "progdata", &entry, NULL );
      if( !sts ) glob.evtprogs[i].progdata = entry.id;
    }

    if( sts ) {
      log_writef( NULL, LOG_LVL_ERROR, "Failed to load event program \"%s\"", tlentry.name );
      goto cont;
    }

    glob.evtprogs[i].flags = FVM_RPC_AUTOUNLOAD;
    glob.evtprogs[i].inlogid = 0;
    glob.evtprogs[i].outlogid = 0;
    
    sts = freg_get_by_name( NULL, tlentry.id, "inlogid", FREG_TYPE_UINT64, (char *)&glob.evtprogs[i].inlogid, sizeof(uint64_t), NULL );
    if( !sts ) glob.evtprogs[i].flags |= FVM_RPC_INLOG;
    
    sts = freg_get_by_name( NULL, tlentry.id, "outlogid", FREG_TYPE_UINT64, (char *)&glob.evtprogs[i].outlogid, sizeof(uint64_t), NULL );
    if( !sts ) glob.evtprogs[i].flags |= FVM_RPC_OUTLOG;

    log_writef( NULL, LOG_LVL_TRACE, "Loaded event program \"%s\"", tlentry.name );
    glob.nevtprogs++;
    if( glob.nevtprogs >= FVM_MAX_EVTPROGS ) break;
						    
  cont:
    id = entry.id;
  } while( 1 );

}

static struct rpc_iterator fvm_evtprog_iter = {
    NULL,
    0,
    30000,
    reload_evtprogs,
    NULL
};


void fvm_register( void ) {
  rpc_program_register( &fvm_prog );
  rpc_iterator_register( &fvm_iter );
  load_startup_progs();

  glob.subsc.cb = fvm_evt_cb;
  rpcd_event_subscribe( &glob.subsc );

  rpc_iterator_register( &fvm_evtprog_iter );
}
