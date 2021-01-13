
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

log_deflogger(dmb_log,DMB_RPC_PROG)
  
struct dmb_host {
  uint64_t hostid;
  uint64_t lastid;
  uint64_t sentseq;
  uint64_t hkey;
};

struct dmb_fvmsc {
  char modname[FVM_MAX_NAME];
  char procname[FVM_MAX_NAME];
  uint32_t category;
};

#define DMB_MAX_HOST 32
#define DMB_MAX_FVMSC 32

static struct {
  uint32_t ocount;
  struct log_s log;
  
  struct dmb_host host[DMB_MAX_HOST];
  int nhost;
  struct dmb_fvmsc fvmsc[DMB_MAX_FVMSC];
  int nfvmsc;
  struct dmb_subscriber *sc;
} glob;


struct dmb_header {
  uint32_t msgid;
  uint32_t flags;
};


static int dmb_set_lastid( uint64_t hkey, uint64_t lastid ) {
  int sts;
  sts = freg_put( NULL, hkey, "lastid", FREG_TYPE_UINT64, (char *)&lastid, sizeof(lastid), NULL );
  return sts;
}

static void publish_cb( struct xdr_s *xdr, struct hrauth_call *hcallp ) {
  uint64_t entryid;
  int i;
	       
  entryid = hcallp->cxt2;
  
  if( !xdr ) {
    dmb_log( LOG_LVL_TRACE, "dmb msg failure hostid=%"PRIx64" entryid=%"PRIx64"", hcallp->hostid, entryid );
    return;
  }


  for( i = 0; i < glob.nhost; i++ ) {
    if( glob.host[i].hostid == hcallp->hostid ) {
      dmb_log( LOG_LVL_TRACE, "dmb msg success hostid=%"PRIx64" entryid=%"PRIx64"", hcallp->hostid, entryid );
      
      glob.host[i].lastid = entryid;
      dmb_set_lastid( glob.host[i].hkey, entryid );
      dmb_iter.timeout = 0;
      break;
    }
  }
  
}

static int dmb_call_publish( uint64_t hostid, uint64_t entryid, uint32_t msgid, uint32_t flags, char *buf, int len ) {
  struct hrauth_call hcall;
  struct xdr_s args[2];
  char argbuf[64];
  int sts;
  
  xdr_init( &args[0], (uint8_t *)argbuf, sizeof(argbuf) );
  xdr_encode_uint64( &args[0], hostreg_localid() );
  xdr_encode_uint32( &args[0], msgid );
  xdr_encode_uint32( &args[0], flags );
  xdr_encode_uint32( &args[0], len );
  xdr_init( &args[1], (uint8_t *)buf, len );
  args[1].offset = len;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = DMB_RPC_PROG;
  hcall.vers = DMB_RPC_VERS;
  hcall.proc = 2; /* publish proc, using hrauth async calls */
  hcall.donecb = publish_cb;
  hcall.cxt2 = entryid;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, args, 2 );
  if( sts ) return sts;

  return 0;
}

static void dmb_iter_cb( struct rpc_iterator *iter ) {
  int i, sts, ne;
  struct log_entry entry;
  struct log_iov iov[2];
  struct dmb_header hdr;
  char msgbuf[DMB_MAX_MSG];

  if( !glob.ocount ) return;
  
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
      /* check this is even a known entry in the log, if not then reset to first entry */
      if( log_read_buf( &glob.log, glob.host[i].lastid, NULL, 0, NULL ) ) {
	glob.host[i].lastid = 0;
	glob.host[i].sentseq = 0;
	dmb_set_lastid( glob.host[i].hkey, 0 );
      }
      break;
    }
    
    /* send this entry to the host */
    if( glob.host[i].sentseq < entry.seq ) {
      dmb_log( LOG_LVL_INFO, "dmb publish hostid=%"PRIx64" entryid=%"PRIx64" len=%u", glob.host[i].hostid, entry.id, iov[1].len );
      
      sts = dmb_call_publish( glob.host[i].hostid, entry.id, hdr.msgid, hdr.flags, msgbuf, iov[1].len );
      if( !sts ) glob.host[i].sentseq = entry.seq;
    }
  }
  
}

static int dmb_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int dmb_proc_publish( struct rpc_inc *inc ) {
  int handle, sts, i;
  uint64_t hostid;
  uint32_t msgid, flags;
  char *bufp;
  int lenp, replyp;
  struct xdr_s args;
  struct dmb_subscriber *sc;
  struct fvm_module *m;
  char argbuf[DMB_MAX_MSG + 64];

  /* 
   * if this is called through proc=2 then reply on outgoing hrauth connection,
   * otherwise reply as normal 
   */
  replyp = 0;
  if( inc->msg.u.call.proc == 2 ) replyp = 1;
  
  bufp = NULL;
  lenp = 0;

  sts = xdr_decode_uint64( &inc->xdr, &hostid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &msgid );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  dmb_log( LOG_LVL_INFO, "dmb recv hostid=%"PRIx64" msgid=%u flags=%x len=%u", hostid, msgid, flags, lenp );
  
  sc = glob.sc;
  while( sc ) {
    if( (sc->category == 0) || (sc->category == ((msgid & 0xffff0000) >> 16)) ) {
      sc->cb( hostid, msgid, flags, bufp, lenp );
    }
    sc = sc->next;
  }

  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  xdr_encode_uint64( &args, hostid );
  xdr_encode_uint32( &args, msgid );
  xdr_encode_uint32( &args, flags );
  xdr_encode_opaque( &args, (uint8_t *)bufp, lenp );
  args.offset = 0;
  for( i = 0; i < glob.nfvmsc; i++ ) {
    if( (glob.fvmsc[i].category == 0) || (glob.fvmsc[i].category == ((msgid & 0xffff0000) >> 16)) ) {
      m = fvm_module_by_name( glob.fvmsc[i].modname );
      if( m ) {
	fvm_run( m, fvm_procid_by_name( m, glob.fvmsc[i].procname ), &args, NULL );
      }
    }
  }

  if( replyp ) {
    return hrauth_reply( inc, NULL, 0 );
  } else {
    rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
    rpc_complete_accept_reply( inc, handle );
  }
  
  return 0;
}

static struct rpc_proc dmb_procs[] = {
  { 0, dmb_proc_null },
  { 1, dmb_proc_publish },
  { 2, dmb_proc_publish }, /* register twice so we can call using standard client or hrauth client async calls */
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

  if( glob.ocount > 0 ) {
    glob.ocount++;
    return 0;
  }
  
  memset( &opts, 0, sizeof(opts) );
  opts.mask = LOG_OPT_COOKIE;
  strcpy( opts.cookie, "dmb" );
  sts = log_open( mmf_default_path( "dmb.log", NULL ), &opts, &glob.log );
  if( sts ) return sts;
  
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

  /* read fvm subscribers */
  sts = freg_subkey( NULL, hkey, "fvm", FREG_CREATE, &hhosts );
  if( sts ) return sts;
  sts = freg_next( NULL, hhosts, 0, &entry );
  while( !sts ) {
    if( ((entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY) && (glob.nfvmsc < DMB_MAX_FVMSC) ) {
      sts = freg_get_by_name( NULL, entry.id, "modname", FREG_TYPE_STRING, glob.fvmsc[glob.nfvmsc].modname, FREG_MAX_NAME, NULL );
      if( !sts ) sts = freg_get_by_name( NULL, entry.id, "procname", FREG_TYPE_STRING, glob.fvmsc[glob.nfvmsc].procname, FREG_MAX_NAME, NULL );
      if( !sts ) sts = freg_get_by_name( NULL, entry.id, "category", FREG_TYPE_UINT32, (char *)&glob.fvmsc[glob.nfvmsc].category, sizeof(uint32_t), NULL );      
      if( !sts ) {
	dmb_log( LOG_LVL_INFO, "dmb init modname=%s procname=%s category=%u",
		 glob.fvmsc[glob.nfvmsc].modname,
		 glob.fvmsc[glob.nfvmsc].procname,
		 glob.fvmsc[glob.nfvmsc].category );
	
	glob.nfvmsc++;
      }
    }
   
    sts = freg_next( NULL, hhosts, entry.id, &entry );
  }

  /* register rpc iterator that checks log and sends any pending messages */
  rpc_iterator_register( &dmb_iter );
  rpc_program_register( &dmb_prog );
  
  glob.ocount = 0;
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

int dmb_subscribe_fvm( char *modname, char *procname, uint32_t category ) {
  int i;

  dmb_log( LOG_LVL_TRACE, "dmb subscribe %s/%s %u", modname, procname, category );
  
  for( i = 0; i < glob.nfvmsc; i++ ) {
    if( (strcmp( glob.fvmsc[i].modname, modname ) == 0) &&
	(strcmp( glob.fvmsc[i].procname, procname ) == 0) ) {
      glob.fvmsc[i].category = category;
      return 0;
    }
  }
	
  if( glob.nfvmsc >= (DMB_MAX_FVMSC - 1) ) return -1;

  strncpy( glob.fvmsc[glob.nfvmsc].modname, modname, FVM_MAX_NAME - 1 );
  strncpy( glob.fvmsc[glob.nfvmsc].procname, procname, FVM_MAX_NAME - 1 );
  glob.fvmsc[glob.nfvmsc].category = category;
  glob.nfvmsc++;

  return 0;
}

int dmb_publish( uint32_t msgid, uint32_t flags, char *buf, int size ) {
  /* write into log */
  struct log_entry entry;
  struct log_iov iov[2];
  struct dmb_header hdr;
  int sts;

  if( !glob.ocount ) return -1;
  
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
  sts = log_write( &glob.log, &entry );
  if( sts ) return -1;

  dmb_iter.timeout = 0;
  return 0;
}


