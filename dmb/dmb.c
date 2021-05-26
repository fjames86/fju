
#define _CRT_SECURE_NO_WARNINGS

#include <inttypes.h>

#include <fju/dmb.h>
#include <fju/freg.h>
#include <fju/fvm.h>
#include <fju/log.h>
#include <fju/rpc.h>
#include <fju/hostreg.h>
#include <fju/hrauth.h>
#include <fju/programs.h>

static void dmb_iter_cb( struct rpc_iterator *iter );
static RPC_ITERATOR(dmb_iter,1000,dmb_iter_cb);

static log_deflogger(dmb_log,"DMB")

static void dmb_invoke_subscribers( uint64_t hostid, uint64_t seq, uint32_t msgid, uint32_t flags, char *buf, int len );

/* host descriptor */
struct dmb_host {
  uint64_t hostid; /* host identifier */
  uint64_t lastid; /* last id acked by host */
  int sent;        /* true if message sent and waiting for reply */
  uint64_t hkey;   /* registry handle */
};

/* fvm subscription descriptor */
struct dmb_fvmsc {
  uint32_t phandle;   /* proc handle */
  uint32_t msgid; /* msgid filter */
  uint32_t flags;
};

#define DMB_MAX_HOST  32    /* max number of hosts */
#define DMB_MAX_FVMSC 64    /* max number of fvm subscribers */

static struct {
  uint32_t ocount;                        /* library open count */
  struct log_s log;                       /* log handle */

  struct dmb_host local;                  /* local host descriptor */
  struct dmb_host host[DMB_MAX_HOST];     /* host descriptors */
  int nhost;
  struct dmb_fvmsc fvmsc[DMB_MAX_FVMSC];  /* fvm subscribers */
  int nfvmsc;
  struct dmb_subscriber *sc;              /* native subscribers */

  uint64_t curhostid;
  uint64_t curseq;
  uint32_t curmsgid;
  char *curbuf;
  uint32_t curlen;
} glob;


/* message header stored in log, data follows */
struct dmb_header {
  uint32_t msgid;
  uint32_t flags;
};


static int dmb_set_lastid( uint64_t hkey, uint64_t lastid, uint64_t seq ) {
  int sts;
  sts = freg_put( NULL, hkey, "lastid", FREG_TYPE_UINT64, (char *)&lastid, sizeof(lastid), NULL );
  if( !sts ) sts = freg_put( NULL, hkey, "seq", FREG_TYPE_UINT64, (char *)&seq, sizeof(seq), NULL );
  
  if( sts ) {
    dmb_log( LOG_LVL_ERROR, "dmb failed to set lastid hkey=%"PRIx64" %"PRIx64"",
	     hkey, lastid );
  }
  
  return sts;
}

static void invoke_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {
  uint64_t entryid;
  int i, sts;
  struct log_entry entry;
  
  entryid = hcallp->cxt[0];
  
  for( i = 0; i < glob.nhost; i++ ) {
    if( glob.host[i].hostid == hcallp->hostid ) {
      dmb_log( LOG_LVL_TRACE, "dmb invoke_cb %s hostid=%"PRIx64" entryid=%"PRIx64"", xdr ? "success" : "failure", hcallp->hostid, entryid );      
      if( xdr ) {
	glob.host[i].lastid = entryid;
	glob.host[i].sent = 0;

	memset( &entry, 0, sizeof(entry) );
	sts = log_read_entry( &glob.log, entryid, &entry );	
	dmb_set_lastid( glob.host[i].hkey, entryid, sts ? 0 : entry.seq );
	dmb_iter.timeout = 0;
      } else {
	glob.host[i].sent = 0;
      }
      break;
    }
  }
  
}

static int dmb_call_invoke( uint64_t hostid, uint64_t seq, uint64_t entryid, uint32_t msgid, uint32_t flags, char *buf, int len ) {
  struct hrauth_call hcall;
  struct xdr_s args[2];
  char argbuf[64];
  int sts;
  
  xdr_init( &args[0], (uint8_t *)argbuf, sizeof(argbuf) );
  xdr_encode_uint64( &args[0], hostreg_localid() );
  xdr_encode_uint64( &args[0], seq );
  xdr_encode_uint32( &args[0], msgid );
  xdr_encode_uint32( &args[0], flags );
  xdr_encode_uint32( &args[0], len );
  xdr_init( &args[1], (uint8_t *)buf, len );
  args[1].offset = len;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = DMB_RPC_PROG;
  hcall.vers = DMB_RPC_VERS;
  hcall.proc = 2; /* invoke proc, using hrauth async calls */
  hcall.donecb = invoke_cb;
  hcall.timeout = 500;
  hcall.cxt[0] = entryid;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, args, 2 );
  if( sts ) {
    dmb_log( LOG_LVL_ERROR, "dmb_call_invoke failed" );
    return sts;
  }

  return 0;
}

static void dmb_iter_cb( struct rpc_iterator *iter ) {
  int i, sts, ne;
  struct log_entry entry;
  struct log_iov iov[2];
  struct dmb_header hdr;
  char msgbuf[DMB_MAX_MSG];

  if( !glob.ocount ) return;

  /* invoke locally if necessary */
  memset( &entry, 0, sizeof(entry) );
  entry.iov = iov;
  entry.niov = 2;
  iov[0].buf = (char *)&hdr;
  iov[0].len = sizeof(hdr);
  iov[1].buf = msgbuf;
  iov[1].len = sizeof(msgbuf);  
  sts = log_read( &glob.log, glob.local.lastid, &entry, 1, &ne );
  if( !sts && (ne == 1) ) {
    if( !(hdr.flags & DMB_REMOTE) ) {
      dmb_invoke_subscribers( hostreg_localid(), entry.seq, hdr.msgid, hdr.flags, msgbuf, iov[1].len );
    } else {
      dmb_log( LOG_LVL_TRACE, "dmb skipping remote entry %"PRIx64"", entry.id );
    }
    glob.local.lastid = entry.id;
  }
  
  for( i = 0; i < glob.nhost; i++ ) {
    memset( &entry, 0, sizeof(entry) );
    entry.iov = iov;
    entry.niov = 2;
    iov[0].buf = (char *)&hdr;
    iov[0].len = sizeof(hdr);
    iov[1].buf = msgbuf;
    iov[1].len = sizeof(msgbuf);
    
    sts = log_read( &glob.log, glob.host[i].lastid, &entry, 1, &ne );
    if( sts || !ne ) {
      //dmb_log( LOG_LVL_ERROR, "dmb failed to read next entry lastid=%"PRIx64"", glob.host[i].lastid );
      /* check this is even a known entry in the log, if not then reset to first entry */
      if( glob.host[i].lastid && log_read_buf( &glob.log, glob.host[i].lastid, NULL, 0, NULL ) ) {
	dmb_log( LOG_LVL_ERROR, "dmb unknown entry %"PRIx64" resetting to start", glob.host[i].lastid );
	glob.host[i].lastid = 0;
	glob.host[i].sent = 0;
	dmb_set_lastid( glob.host[i].hkey, 0, 0 );
      }
    } else {    
      /* send this entry to the host */
      if( !glob.host[i].sent ) {
	if( !(hdr.flags & DMB_LOCAL) ) {
	  dmb_log( LOG_LVL_INFO, "dmb call invoke hostid=%"PRIx64" entryid=%"PRIx64" msgid=%08x len=%u", glob.host[i].hostid, entry.id, hdr.msgid, iov[1].len );
	  
	  sts = dmb_call_invoke( glob.host[i].hostid, entry.seq, entry.id, hdr.msgid, hdr.flags, msgbuf, iov[1].len );
	  if( !sts ) glob.host[i].sent = 1;
	} else {
	  dmb_log( LOG_LVL_INFO, "dmb skipping local entryid %"PRIx64"", entry.id );
	  glob.host[i].sent = 1;
	  glob.host[i].lastid = entry.id;
	}
      } else {
	//dmb_log( LOG_LVL_TRACE, "dmb skipping entry already sent" );
      }
    }
  }
  
}

static int dmb_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static void dmb_invoke_subscribers( uint64_t hostid, uint64_t seq, uint32_t msgid, uint32_t flags, char *buf, int len ) {
  struct dmb_subscriber *sc;
  struct xdr_s args;
  struct fvm_module *m;
  int i, procid, sts;
  char argbuf[DMB_MAX_MSG + 64];
  
  sc = glob.sc;
  while( sc ) {
    if( (sc->mask == 0) || (sc->mask & msgid) ) {
      glob.curhostid = hostid;
      glob.curseq = seq;
      glob.curmsgid = msgid;
      glob.curbuf = buf;
      glob.curlen = len;      
      sc->cb( hostid, seq, msgid, buf, len );
      glob.curhostid = 0;
      glob.curseq = 0;
      glob.curmsgid = 0;
      glob.curbuf = NULL;
      glob.curlen = 0;      
    }
    sc = sc->next;
  }

  for( i = 0; i < glob.nfvmsc; i++ ) {
    if( (glob.fvmsc[i].msgid == 0) || (glob.fvmsc[i].msgid == msgid) ) {
      if( glob.fvmsc[i].flags & DMB_FVMSC_APPLY ) {
	/* if filtering on a specific msg then pass buffer as args directly */
	xdr_init( &args, (uint8_t *)buf, len );
      } else {
	/* raw mode: pass buffer as opaque, signature (msgid,len,buf) */
	xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
	xdr_encode_uint32( &args, msgid );
	xdr_encode_opaque( &args, (uint8_t *)buf, len );
      }
      args.offset = 0;
      
      sts = fvm_proc_by_handle( glob.fvmsc[i].phandle, &m, &procid );
      if( !sts ) {
	dmb_log( LOG_LVL_TRACE, "dmb_invoke_subscribers %s/%s msgid=%08x scmsgid=%08x",
		 m->name, m->procs[procid].name, msgid, glob.fvmsc[i].msgid );
		 
	glob.curhostid = hostid;
	glob.curseq = seq;
	glob.curmsgid = msgid;
	glob.curlen = len;
	sts = fvm_run( m, procid, &args, NULL );
	if( sts ) dmb_log( LOG_LVL_ERROR, "dmb invoke subscriber failed %s/%s arglen=%u", m->name, m->procs[procid].name, args.count );
	glob.curhostid = 0;
	glob.curseq = 0;
	glob.curmsgid = 0;
	glob.curlen = 0;
      } else {
	dmb_log( LOG_LVL_ERROR, "dmb invoke subscriber invalid method handle %x", glob.fvmsc[i].phandle );
      }
    }
  }

}

void dmb_msginfo( uint64_t *hostid, uint64_t *seq, uint32_t *msgid, char **buf, uint32_t *len ) {
  if( hostid ) *hostid = glob.curhostid;
  if( seq ) *seq = glob.curseq;
  if( msgid ) *msgid = glob.curmsgid;
  if( buf ) *buf = glob.curbuf;
  if( len ) *len = glob.curlen;
}

static int dmb_proc_invoke( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t hostid, seq;
  uint32_t msgid, flags;
  char *bufp;
  int lenp, replyp;

  /* 
   * if this is called through proc=2 then reply on outgoing hrauth connection,
   * otherwise reply as normal 
   */
  replyp = 0;
  if( inc->msg.u.call.proc == 2 ) replyp = 1;
  
  bufp = NULL;
  lenp = 0;

  sts = xdr_decode_uint64( &inc->xdr, &hostid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &seq );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &msgid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  dmb_log( LOG_LVL_INFO, "dmb_proc_invoke hostid=%"PRIx64" msgid=%08x flags=%x len=%u", hostid, msgid, flags, lenp );
  dmb_invoke_subscribers( hostid, seq, msgid, flags, bufp, lenp );
  
  if( replyp ) {
    return hrauth_reply( inc, NULL, 0 );
  } else {
    rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
    rpc_complete_accept_reply( inc, handle );
  }
  
  return 0;
}

static int dmb_proc_publish( struct rpc_inc *inc ) {
  int handle, sts;
  uint32_t msgid, flags;
  uint64_t seq;
  char *bufp;
  int lenp;

  bufp = NULL;
  lenp = 0;

  sts = xdr_decode_uint32( &inc->xdr, &msgid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  dmb_log( LOG_LVL_INFO, "dmb_proc_publish msgid=%08x flags=%x len=%u", msgid, flags, lenp );
  sts = dmb_publish( msgid, flags, bufp, lenp, &seq );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_uint64( &inc->xdr, sts ? 0 : seq );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static struct rpc_proc dmb_procs[] = {
  { 0, dmb_proc_null },
  { 1, dmb_proc_invoke },
  { 2, dmb_proc_invoke }, /* register twice so we can call using standard client or hrauth client async calls */
  { 3, dmb_proc_publish },
  { 0, NULL }
};

static struct rpc_version dmb_vers = {
  NULL, DMB_RPC_VERS, dmb_procs
};

static uint32_t dmb_prog_auths[] = { RPC_AUTH_HRAUTH, 0 };
static struct rpc_program dmb_prog = {
    NULL, DMB_RPC_PROG, &dmb_vers, dmb_prog_auths
};



int dmb_open( void ) {
  int sts;
  struct freg_entry entry;
  struct log_opts opts;
  uint64_t hkey, hhosts;
  struct log_prop prop;
  
  if( glob.ocount > 0 ) {
    glob.ocount++;
    return 0;
  }
  
  memset( &opts, 0, sizeof(opts) );
  opts.mask = LOG_OPT_COOKIE;
  strcpy( opts.cookie, "dmb" );
  sts = log_open( mmf_default_path( "dmb.log", NULL ), &opts, &glob.log );
  if( sts ) return sts;
  glob.log.flags |= LOG_NOLOCK;
  
  log_prop( &glob.log, &prop );
  glob.local.lastid = prop.last_id;
  
  /* get toplevel handle */
  sts = freg_subkey( NULL, 0, "/fju/dmb", FREG_CREATE, &hkey );
  if( sts ) return sts;

  /* read hosts from reg */
  sts = freg_subkey( NULL, hkey, "hosts", FREG_CREATE, &hhosts );
  if( sts ) return sts;  
  sts = freg_next( NULL, hhosts, 0, &entry );
  while( !sts ) {
    if( ((entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY) && (glob.nhost < DMB_MAX_HOST) ) {
      glob.host[glob.nhost].hkey = entry.id;
      sts = freg_get_by_name( NULL, entry.id, "hostid", FREG_TYPE_UINT64, (char *)&glob.host[glob.nhost].hostid, sizeof(uint64_t), NULL );
      if( !sts ) {
	sts = freg_get_by_name( NULL, entry.id, "lastid", FREG_TYPE_UINT64, (char *)&glob.host[glob.nhost].lastid, sizeof(uint64_t), NULL );

	dmb_log( LOG_LVL_INFO, "dmb init hostid=%"PRIx64" lastid=%"PRIx64"", glob.host[glob.nhost].hostid, glob.host[glob.nhost].lastid );
	
	glob.nhost++;
      }
    }
    
    sts = freg_next( NULL, hhosts, entry.id, &entry );
  }

  /* register rpc iterator that checks log and sends any pending messages */
  rpc_iterator_register( &dmb_iter );
  rpc_program_register( &dmb_prog );
  
  glob.ocount = 1;
  return 0;
}


int dmb_close( void ) {
  if( glob.ocount == 0 ) return -1;
  glob.ocount--;
  if( glob.ocount > 0 ) return 0;
  
  log_close( &glob.log );
  memset( &glob, 0, sizeof(glob) );
  glob.ocount = 0;
  return 0;
}

int dmb_subscribe( struct dmb_subscriber *sc ) {
  sc->next = glob.sc;
  glob.sc = sc;
  return 0;
}

int dmb_subscribe_fvm( char *modname, char *procname, uint32_t msgid, uint32_t flags ) {
  int i, sts;
  uint32_t phandle;
  
  sts = fvm_handle_by_name( modname, procname, &phandle );
  if( sts ) {
    dmb_log( LOG_LVL_ERROR, "Unknown proc %s/%s", modname, procname );
    return -1;
  }
  
  dmb_log( LOG_LVL_TRACE, "dmb subscribe %s/%s %u", modname, procname, msgid );
  
  for( i = 0; i < glob.nfvmsc; i++ ) {
    if( glob.fvmsc[i].phandle == phandle ) {
      glob.fvmsc[i].msgid = msgid;
      glob.fvmsc[i].flags = flags;
      return 0;
    }
  }
	
  if( glob.nfvmsc >= (DMB_MAX_FVMSC - 1) ) return -1;

  glob.fvmsc[glob.nfvmsc].phandle = phandle;
  glob.fvmsc[glob.nfvmsc].msgid = msgid;
  glob.fvmsc[glob.nfvmsc].flags = flags;
  glob.nfvmsc++;

  return 0;
}

int dmb_unsubscribe_fvm( char *modname, char *procname ) {
  int i, sts;
  uint32_t phandle;

  sts = fvm_handle_by_name( modname, procname, &phandle );
  if( sts ) return -1;
  
  dmb_log( LOG_LVL_TRACE, "dmb unsubscribe %s/%s", modname, procname );
  
  for( i = 0; i < glob.nfvmsc; i++ ) {
    if( glob.fvmsc[i].phandle == phandle ) {
      if( i != (glob.nfvmsc - 1) ) glob.fvmsc[i] = glob.fvmsc[glob.nfvmsc - 1];
      glob.nfvmsc--;
      return 0;
    }
  }
	
  return -1;
}

int dmb_publish( uint32_t msgid, uint32_t flags, char *buf, int size, uint64_t *seq ) {
  /* write into log */
  struct log_entry entry;
  struct log_iov iov[2];
  struct dmb_header hdr;
  int sts;

  if( !glob.ocount ) {
    dmb_log( LOG_LVL_ERROR, "dmb_publish library not open" );
    return -1;
  }

  if( size > DMB_MAX_MSG ) {
    dmb_log( LOG_LVL_ERROR, "dmb_publish size %u > max size %u", size, DMB_MAX_MSG );
    return -1;
  }
  
  /* write into log and trigger iterator to send to remote hosts */
  memset( &hdr, 0, sizeof(hdr) );
  hdr.msgid = msgid;
  hdr.flags = flags;
  
  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = (char *)&hdr;
  iov[0].len = sizeof(hdr);
  iov[1].buf = buf;
  iov[1].len = size;
  entry.iov = iov;
  entry.niov = 2;
  entry.flags = LOG_BINARY;
  sts = log_write( &glob.log, &entry );
  if( sts ) {
    dmb_log( LOG_LVL_ERROR, "dmb_publish failed to write entry" );
    return -1;
  }

  dmb_log( LOG_LVL_TRACE, "dmb_publish msgid=%08x flags=%x len=%u entryid=%"PRIx64"", msgid, flags, size, entry.id );  
  dmb_iter.timeout = 0;

  if( seq ) *seq = entry.seq;
  
  return 0;
}

int dmb_host_info( uint64_t hostid, uint64_t *lastid, uint64_t *seq ) {
  int sts, i;
  uint64_t hkey;
  
  if( lastid ) *lastid = 0;
  if( seq ) *seq = 0;
  
  for( i = 0; i < glob.nhost; i++ ) {
    if( glob.host[i].hostid == hostid ) {
      hkey = glob.host[i].hkey;
      
      if( lastid ) sts = freg_get_by_name( NULL, hkey, "lastid", FREG_TYPE_UINT64, (char *)lastid, sizeof(*lastid), NULL );
      
      if( seq ) sts = freg_get_by_name( NULL, hkey, "seq", FREG_TYPE_UINT64, (char *)seq, sizeof(*seq), NULL );
      
      return 0;
    }
  }

  return -1;
}

