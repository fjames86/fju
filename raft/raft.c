
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <inttypes.h>

#include <fju/raft.h>
#include <fju/mmf.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/hostreg.h>
#include <fju/hrauth.h>
#include <fju/programs.h>
#include <fju/log.h>
#include <fju/sec.h>

static log_deflogger(raft_log, RAFT_RPC_PROG)
  
struct raft_file {
  struct raft_prop prop;
  struct raft_cluster cluster[RAFT_MAX_CLUSTER];
};

struct raft_log {
  uint64_t clid;
  struct log_s log;
};

static struct {
  uint32_t ocount;
  struct mmf_s mmf;
  struct raft_file *file;

  struct raft_prop prop;
  struct raft_cluster cl[RAFT_MAX_CLUSTER];
  struct raft_log clog[RAFT_MAX_CLUSTER];
  uint32_t ncl;
  struct raft_app *app;

  char buf[RAFT_MAX_COMMAND];
} glob;

static uint64_t raft_term_timeout( void ) {
  return rpc_now() + glob.prop.term_low +
    (sec_rand_uint32() % (glob.prop.term_high - glob.prop.term_low));
}

static uint64_t raft_elec_timeout( void ) {
  return rpc_now() + glob.prop.elec_low +
    (sec_rand_uint32() % (glob.prop.elec_high - glob.prop.elec_low));
}

static void raft_lock( void ) {
  mmf_lock( &glob.mmf );
}
static void raft_unlock( void ) {
  mmf_unlock( &glob.mmf );
}

int raft_open( void ) {
  int sts;

  if( glob.ocount > 0 ) {
    glob.ocount++;
    return 0;
  }

  sts = mmf_open( mmf_default_path( "raft", "raft.dat", NULL ), &glob.mmf );
  if( sts ) return sts;

  sts = mmf_remap( &glob.mmf, sizeof(*glob.file) );

  glob.file = (struct raft_file *)glob.mmf.file;

  if( glob.file->prop.magic != RAFT_MAGIC ) {
    glob.file->prop.magic = RAFT_MAGIC;
    glob.file->prop.version = RAFT_VERSION;
    glob.file->prop.seq = 1;
    glob.file->prop.count = 0;
    glob.file->prop.flags = 0;
    glob.file->prop.elec_low = 1000;
    glob.file->prop.elec_high = 3000;
    glob.file->prop.term_low = 3000;
    glob.file->prop.term_high = 8000;
    glob.file->prop.rpc_timeout = 200;
    glob.file->prop.snapth = 90;
  }
  
  glob.ocount = 1;
  return 0;
}

int raft_close( void ) {
  if( glob.ocount == 0 ) return -1;
  if( glob.ocount > 1 ) {
    glob.ocount--;
    return 0;
  }
  mmf_close( &glob.mmf );
  glob.ocount = 0;
  return 0;
}

int raft_prop( struct raft_prop *prop ) {
  if( !glob.ocount ) return -1;
  raft_lock();
  *prop = glob.file->prop;
  raft_unlock();
  return 0;
}

int raft_prop_set( uint32_t mask, struct raft_prop *prop ) {
  if( !glob.ocount ) return -1;
  raft_lock();
  if( mask & RAFT_PROP_ELEC_LOW ) {
    glob.file->prop.elec_low = prop->elec_low;
  }
  if( mask & RAFT_PROP_ELEC_HIGH ) {
    glob.file->prop.elec_high = prop->elec_high;
  }
  if( mask & RAFT_PROP_TERM_LOW ) {
    glob.file->prop.term_low = prop->term_low;
  }
  if( mask & RAFT_PROP_TERM_HIGH ) {
    glob.file->prop.term_high = prop->term_high;
  }
  if( mask & RAFT_PROP_RPC_TIMEOUT ) {
    glob.file->prop.rpc_timeout = prop->rpc_timeout;
  }
  if( mask & RAFT_PROP_SNAPTH ) {
    glob.file->prop.snapth = prop->snapth;
  }
  raft_unlock();
  return 0;
}  


/* database functions */
int raft_cluster_list( struct raft_cluster *cl, int ncl ) {
  int i;
  if( !glob.ocount ) return -1;
  raft_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( i < ncl ) cl[i] = glob.file->cluster[i];
  }
  i = glob.file->prop.count;
  raft_unlock();
  return i;
}

int raft_clid_list( uint64_t *clid, int ncl ) {
  int i;
  if( !glob.ocount ) return -1;
  raft_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( i < ncl ) clid[i] = glob.file->cluster[i].clid;
  }
  i = glob.file->prop.count;
  raft_unlock();
  return i;  
}

uint64_t raft_clid_by_appid( uint32_t appid ) {
  int i;
  uint64_t clid = 0;
  if( !glob.ocount ) return -1;
  raft_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( glob.file->cluster[i].appid == appid ) {
      clid = glob.file->cluster[i].clid;
      break;
    }
  }
  raft_unlock();
  return clid;
}

uint64_t raft_clid_by_cookie( char *cookie ) {
  int i;
  uint64_t clid = 0;
  if( !glob.ocount ) return -1;
  raft_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( strcmp( glob.file->cluster[i].cookie, cookie ) == 0 ) {
      clid = glob.file->cluster[i].clid;
      break;
    }
  }
  raft_unlock();
  return clid;
}

int raft_cluster_by_clid( uint64_t clid, struct raft_cluster *cl ) {
  int i, sts;

  sts = -1;
  if( !glob.ocount ) return -1;
  raft_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( glob.file->cluster[i].clid == clid ) {
      if( cl ) *cl = glob.file->cluster[i];
      sts = 0;
      break;
    }
  }
  raft_unlock();
  return sts;
}

int raft_cluster_set( struct raft_cluster *cl ) {
  int i, sts;

  if( !cl->clid ) sec_rand( &cl->clid, sizeof(cl->clid) );
  
  sts = -1;
  if( !glob.ocount ) return -1;
  raft_lock();
  
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( glob.file->cluster[i].clid == cl->clid ) {
      if( cl ) {
	glob.file->cluster[i] = *cl;
	glob.file->prop.seq++;
      }
      sts = 0;
      goto done;
    }
  }

  if( glob.file->prop.count >= RAFT_MAX_CLUSTER ) goto done;

  i = glob.file->prop.count;
  cl->term = 1;
  cl->appliedseq = 0;
  glob.file->cluster[i] = *cl;
  glob.file->prop.count++;
  glob.file->prop.seq++;
  sts = 0;
  
 done:
  raft_unlock();
  return sts;  
}

int raft_cluster_rem( uint64_t clid ) {
  int i, sts;

  sts = -1;
  if( !glob.ocount ) return -1;
  raft_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( glob.file->cluster[i].clid == clid ) {
      if( i != (glob.file->prop.count - 1) ) glob.file->cluster[i] = glob.file->cluster[glob.file->prop.count - 1];

      /* delete the command log */
      {
	char clstr[64];
	sprintf( clstr, "%"PRIx64".log", clid );
	mmf_delete_file( mmf_default_path( "raft", clstr, NULL ) );
      }
      
      glob.file->prop.count--;
      sts = 0;
      break;
    }
  }
  raft_unlock();
  return sts;    
}

int raft_log_open( uint64_t clid, struct log_s *log ) {
  char clstr[64];
  struct log_opts opts;
  
  sprintf( clstr, "%"PRIx64".log", clid );
  memset( &opts, 0, sizeof(opts) );
  opts.mask = LOG_OPT_COOKIE|LOG_OPT_FLAGS;
  strcpy( opts.cookie, "raft" );
  opts.flags = LOG_FLAG_FIXED;
  return log_open( mmf_default_path( "raft", clstr, NULL ), &opts, log );
}

/* ------------ rpc -------------------- */

static void raft_convert_follower( struct raft_cluster *cl, uint64_t term, uint64_t leaderid );
static void raft_convert_candidate( struct raft_cluster *cl );
static void raft_convert_leader( struct raft_cluster *cl );
static void raft_set_iter_timeout( void );
static struct raft_app *raft_app_by_appid( uint32_t appid );
static void raft_call_putcmd( struct raft_cluster *cl, uint64_t hostid, uint64_t cseq );
static void raft_apply_commands( struct raft_cluster *cl );
static void raft_call_snapsave( struct raft_cluster *cl, uint64_t hostid, uint32_t offset );
static void raft_app_command( struct raft_app *app, struct raft_cluster *cl, uint64_t seq, char *buf, int len );

static struct raft_cluster *cl_by_id( uint64_t clid ) {
  int i;
  for( i = 0; i < glob.ncl; i++ ) {
    if( glob.cl[i].clid == clid ) return &glob.cl[i];
  }
  return NULL;
}
static struct log_s *clog_by_id( uint64_t clid ) {
  int i, sts;
  /* return already open log */
  for( i = 0; i < RAFT_MAX_CLUSTER; i++ ) {
    if( glob.clog[i].clid == clid ) return &glob.clog[i].log;
  }

  /* open a new log descriptor */
  for( i = 0; i < RAFT_MAX_CLUSTER; i++ ) {
    if( glob.clog[i].clid == 0 ) {
      sts = raft_log_open( clid, &glob.clog[i].log );
      if( sts ) return NULL;
      
      glob.clog[i].clid = clid;      
      return &glob.clog[i].log;
    }
  }

  /* close a log we don't need anymore */
  for( i = 0; i < RAFT_MAX_CLUSTER; i++ ) {
    if( !cl_by_id( glob.clog[i].clid ) ) {
      log_close( &glob.clog[i].log );
      glob.clog[i].clid = 0;
      
      sts = raft_log_open( clid, &glob.clog[i].log );
      if( sts ) return NULL;
      
      glob.clog[i].clid = clid;      
      return &glob.clog[i].log;
    }
  }

  /* failed? "this should never happen" */
  raft_log( LOG_LVL_ERROR, "Failed to open command log clid=%"PRIx64"", clid ); 
  return NULL;    
}

struct raft_cmd_header {
  uint64_t term;
  uint64_t seq;
};

/* read command buffer. returns -1 on error or command buffer length on success */
static int raft_command_by_seq( uint64_t clid, uint64_t seq, uint64_t *term, char *buf, int len, uint64_t *entryid ) {
  int sts, ne;
  struct log_s *log;
  struct log_entry entry;
  struct log_iov iov[2];
  struct raft_cmd_header hdr;
  
  log = clog_by_id( clid );
  if( !log ) return -1;

  /* 
   * Read from most recently written entry until we find a matching entry
   * Note that this is somewhat inefficient but hopefully we won't be looking too far 
   * back in time very often so it should be acceptable. 
   * It would be better to have a constant time lookup mapping seq => entry.id but this 
   * is not easy to do using fjlogs (we'd need a custom logger)
   */
  
  memset( &entry, 0, sizeof(entry) );
  entry.iov = iov;
  iov[0].buf = (char *)&hdr;
  iov[0].len = sizeof(hdr);
  iov[1].buf = buf;
  iov[1].len = len;
  entry.niov = 2;

  /* start at final entry */
  sts = log_read_end( log, 0, &entry, 1, &ne );
  if( sts || !ne ) return -1;

  do {  
    if( hdr.seq == seq ) {
      /* found it */
      if( term ) *term = hdr.term;
      if( entryid ) *entryid = entry.id;
      return entry.msglen - sizeof(hdr);
    }

    /* try previous entry, if any */
    if( entry.prev_id == 0 ) return -1;

    sts = log_read_entry( log, entry.prev_id, &entry );
    if( sts ) return -1;
  } while( 1 );

  return -1;  
}

/* get highest seq written */
int raft_command_seq( uint64_t clid, uint64_t *term, uint64_t *seq ) {
  int sts, ne;
  struct log_s *log;
  struct log_entry entry;
  struct log_iov iov[1];
  struct raft_cmd_header hdr;

  if( term ) *term = 0;
  if( seq ) *seq = 0;

  log = clog_by_id( clid );
  if( !log ) return -1;

  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = (char *)&hdr;
  iov[0].len = sizeof(hdr);
  entry.iov = iov;
  entry.niov = 1;
  sts = log_read_end( log, 0, &entry, 1, &ne );
  if( sts ) return -1;
  if( term ) *term = ne ? hdr.term : 0;
  if( seq ) *seq = ne ? hdr.seq : 0;
  return 0;    
}

/* user function for listing stored commands */
int raft_command_list( uint64_t clid, struct raft_command_info *clist, int n ) {
  int sts, i, ne;
  struct log_s log;
  struct log_entry entry;
  struct log_iov iov[1];
  struct raft_cmd_header hdr;
  uint64_t id;
  
  sts = raft_log_open( clid, &log );
  if( sts ) return sts;
  
  i = 0;
  id = 0;
  memset( &entry, 0, sizeof(entry) );
  entry.iov = iov;
  entry.niov = 1;
  iov[0].buf = (char *)&hdr;
  iov[0].len = sizeof(hdr);
  while( 1 ) {
    sts = log_read( &log, id, &entry, 1, &ne );
    if( sts || !ne ) break;

    if( i < n ) {
      clist[i].term = hdr.term;
      clist[i].seq = hdr.seq;
      clist[i].stored = entry.timestamp;      
      clist[i].len = entry.msglen - sizeof(hdr);
    }
    i++;
    
    id = entry.id;
  }

  log_close( &log );
  return i;
}

/* store a command buffer */
static int raft_command_put( uint64_t clid, uint64_t term, uint64_t seq, char *buf, int len ) {
  int sts;
  struct log_s *log;
  struct log_entry entry;
  struct log_iov iov[2];
  struct raft_cmd_header hdr;
  uint64_t nseq;
  struct log_prop prop;
  
  if( len > RAFT_MAX_COMMAND ) return -1;
  
  /* get highest seq written */
  sts = raft_command_seq( clid, NULL, &nseq );
  if( sts ) return sts;

  /* disallow writing entries out of order */
  if( (nseq != 0) && (seq != (nseq + 1)) ) return -1;

  raft_log( LOG_LVL_TRACE, "raft_command_put nseq=%"PRIu64" seq=%"PRIu64"", nseq, seq );
  
  log = clog_by_id( clid );
  if( !log ) return -1;

  hdr.term = term;
  hdr.seq = seq;
  
  memset( &entry, 0, sizeof(entry) );
  entry.iov = iov;
  entry.niov = 2;
  iov[0].buf = (char *)&hdr;
  iov[0].len = sizeof(hdr);
  iov[1].buf = buf;
  iov[1].len = len;
  entry.flags = LOG_BINARY;
  log_write( log, &entry );
  raft_log( LOG_LVL_TRACE, "raft_command_put entry.id=%"PRIx64" entry.prev_id=%"PRIx64"", entry.id, entry.prev_id );
  
  /* wait for command to be written to stable storage */
  log_sync( log, MMF_SYNC_NOW );

  /* check log capacity */
  log_prop( log, &prop );
  if( ((100 * prop.count) / prop.lbacount) >= glob.prop.snapth ) {
    struct raft_app *app;
    struct raft_cluster *cl;
    raft_log( LOG_LVL_INFO, "Requesting snapshot clid=%"PRIx64"", clid );
    cl = cl_by_id( clid );
    if( cl ) {
      app = raft_app_by_appid( cl->appid );
      if( app && app->snapsave ) {
	app->snapsave( app, cl, term, seq );
      }
    }
  }
  
  return 0;
}

static int raft_cluster_quorum( struct raft_cluster *cl ) {
  return ((1 + cl->nmember) / 2) + 1;
}

static int raft_highest_storedseq( uint64_t clid, uint64_t *term, uint64_t *seq ) {
  int sts;
  struct raft_snapshot_info info;
  
  sts = raft_command_seq( clid, term, seq );
  if( sts ) {
    sts = raft_snapshot_info( clid, &info, NULL );
    if( sts ) return sts;
    if( term) *term = info.term;
    if( seq ) *seq = info.seq;
  }
  
  return 0;  
}

/* find the highest storedseq confirmed on quorum of nodes */
static uint64_t raft_quorum_commitseq( struct raft_cluster *cl ) {
  int i, q, c;
  uint64_t lseq;

  /* get quorum */
  q = raft_cluster_quorum( cl );

  /* get local highest stored seq */
  raft_command_seq( cl->clid, NULL, &lseq );

  while( lseq ) {
    c = 1;
    for( i = 0; i < cl->nmember; i++ ) {
      if( cl->member[i].storedseq >= lseq ) c++;
    }
    if( c >= q ) break;
    lseq--;
  }
  
  return lseq;
}

static void raft_check_commitseq( struct raft_cluster *cl ) {
  uint64_t comseq;

  comseq = raft_quorum_commitseq( cl );
  if( comseq > cl->commitseq ) {
    raft_log( LOG_LVL_TRACE, "Commitseq changed %"PRIu64" -> %"PRIu64"", cl->commitseq, comseq );    
    cl->commitseq = comseq;
    raft_cluster_set( cl );

    raft_apply_commands( cl );
  }
  
}

static void raft_apply_commands( struct raft_cluster *cl ) {
  struct raft_app *app;
  uint64_t s;
  int sts;
  
  /* apply any commands commited but not yet applied */
  app = raft_app_by_appid( cl->appid );
  while( (cl->appliedseq + 1) <= cl->commitseq ) {
    s = cl->appliedseq + 1;
    raft_log( LOG_LVL_INFO, "raft_apply_command seq=%"PRIu64"", s );
      
    sts = raft_command_by_seq( cl->clid, s, NULL, glob.buf, sizeof(glob.buf), NULL );
    if( sts < 0 ) {
      raft_log( LOG_LVL_ERROR, "Failed to get command seq=%"PRIu64"", s );
      break;
    }
    
    /* send command to application */
    if( app && app->command ) {
      app->command( app, cl, s, glob.buf, sts );
    }
    
    cl->appliedseq = s;
    raft_cluster_set( cl );
  }
  
}

#if 0
static uint64_t raft_cluster_min_storedseq( struct raft_cluster *cl ) {
  int i;
  uint64_t seq = 0;
  for( i = 0; i < cl->nmember; i++ ) {
    if( cl->member[i].storedseq < seq ) seq = cl->member[i].storedseq;
  }
  return seq;
}
#endif






static void raft_ping_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t clid, hostid, term, seq;
  struct raft_cluster *cl;
  struct raft_member *mp;
  int i, success, sts;

  clid = hcallp->cxt2;
  hostid = hcallp->hostid;
  
  raft_log( LOG_LVL_TRACE, "raft_ping_cb clid=%"PRIx64" hostid=%"PRIx64" %s",
	    clid, hostid, res ? "" : "Timeout" );
  
  if( !res ) return;
  
  cl = cl_by_id( clid );
  if( !cl ) {
    raft_log( LOG_LVL_TRACE, "Unknown cluster" );
    return;
  }


  mp = NULL;
  for( i = 0; i < cl->nmember; i++ ) {
    if( cl->member[i].hostid == hostid ) {
      mp = &cl->member[i];
      break;
    }
  }
  if( !mp ) {
    raft_log( LOG_LVL_ERROR, "Unknown member" );
    return;
  }

  sts = xdr_decode_boolean( res, &success );
  if( !sts ) sts = xdr_decode_uint64( res, &term );
  if( !sts ) sts = xdr_decode_uint64( res, &seq );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "Xdr decode error" );
    return;
  }

  raft_log( LOG_LVL_TRACE, "raft_ping_cb success=%s term=%"PRIu64" storeseq=%"PRIu64"", success ? "true" : "false", term, seq );
  if( term > cl->term ) {
    raft_log( LOG_LVL_TRACE, "Remote term higher - convert to follower leaderid=%"PRIx64"", hcallp->hostid );
    cl->voteid = 0;
    raft_convert_follower( cl, term, hcallp->hostid );
  }
  
  if( !success ) {
    raft_log( LOG_LVL_TRACE, "ping rejected" );
  }
  
  mp->lastseen = time( NULL );
  mp->storedseq = seq;
  raft_cluster_set( cl );

  /* check commitseq */
  raft_check_commitseq( cl );


  return;  
}

static void raft_call_ping( struct raft_cluster *cl, uint64_t hostid ) {
  struct hrauth_call hcall;
  struct xdr_s args;
  char argbuf[256];
  int sts;
  uint64_t pterm, pseq;

  raft_log( LOG_LVL_TRACE, "raft_call_ping clid=%"PRIx64" host=%"PRIx64" term=%"PRIu64" commitseq=%"PRIu64"",
	    cl->clid, hostid, cl->term, cl->commitseq );
  
  raft_command_seq( cl->clid, &pterm, &pseq );
  
  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  /* encode args */
  xdr_encode_uint64( &args, cl->clid );
  xdr_encode_uint64( &args, cl->term ); /* current term */
  xdr_encode_uint64( &args, cl->commitseq ); /* leaders commit seq */
  xdr_encode_uint64( &args, pterm ); /* previous log term/seq */
  xdr_encode_uint64( &args, pseq );
  xdr_encode_fixed( &args, (uint8_t *)cl->cookie, RAFT_MAX_COOKIE );
  xdr_encode_boolean( &args, 0 ); /* no commands follow */
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT_RPC_PROG;
  hcall.vers = RAFT_RPC_VERS;
  hcall.proc = 1; /* append */
  hcall.donecb = raft_ping_cb;
  hcall.cxt2 = cl->clid;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, &args, 1 );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "hrauth_call_async failed" );
    return;
  }

  
}

static void raft_send_pings( struct raft_cluster *cl ) {
  int i;
  uint64_t seq;

  raft_log( LOG_LVL_TRACE, "raft_send_pings cl=%"PRIx64" nmember %u", cl->clid, cl->nmember );
  
  raft_command_seq( cl->clid, NULL, &seq );
  for( i = 0; i < cl->nmember; i++ ) {
    raft_log( LOG_LVL_TRACE, "raft_send_pings cl=%"PRIx64" i=%u hostid=%"PRIx64"",
	      cl->clid, i, cl->member[i].hostid );
    
    if( cl->member[i].storedseq < seq ) {
      raft_call_putcmd( cl, cl->member[i].hostid, 0 );
    } else {
      raft_call_ping( cl, cl->member[i].hostid );
    }
  }
}

static void raft_vote_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t clid, hostid, term;
  struct raft_cluster *cl;
  struct raft_member *mp;
  int i, success, sts, count;

  clid = hcallp->cxt2;
  hostid = hcallp->hostid;
  
  raft_log( LOG_LVL_DEBUG, "raft_vote_cb clid=%"PRIx64" hostid=%"PRIx64" %s", clid, hostid, res ? "" : "timeout" );
  if( !res ) return;
  
  clid = hcallp->cxt2;
  cl = cl_by_id( clid );
  if( !cl ) {
    raft_log( LOG_LVL_TRACE, "Unknown cluster %"PRIx64"", clid );
    return;
  }


  mp = NULL;
  for( i = 0; i < cl->nmember; i++ ) {
    if( cl->member[i].hostid == hostid ) {
      mp = &cl->member[i];
      break;
    }
  }
  if( !mp ) {
    raft_log( LOG_LVL_ERROR, "Unknown member" );
    return;
  }

  sts = xdr_decode_boolean( res, &success );
  if( !sts ) sts = xdr_decode_uint64( res, &term );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "Xdr decode error" );
    return;
  }

  if( term > cl->term ) {
    raft_log( LOG_LVL_TRACE, "Vote returned newer term - convert to follower leaderid=%"PRIx64"", hostid );
    raft_convert_follower( cl, term, hostid );
  } else if( !success ) {
    raft_log( LOG_LVL_TRACE, "Vote request rejected clid=%"PRIx64" hostid=%"PRIx64"", clid, hostid );
  } else if( success ) {
    mp->flags |= RAFT_MEMBER_VOTED;
    count = 1;
    for( i = 0; i < cl->nmember; i++ ) {
      if( cl->member[i].flags & RAFT_MEMBER_VOTED ) count++;
    }

    raft_log( LOG_LVL_TRACE, "Vote success clid=%"PRIx64" count=%u", cl->clid, count );
    if( count >= raft_cluster_quorum( cl ) ) {
      raft_log( LOG_LVL_TRACE, "Sufficient votes received - convert to leader" );
      raft_convert_leader( cl );
    }
  }
  
  mp->lastseen = time( NULL );
  raft_cluster_set( cl );
  return;  
}

static void raft_call_vote( struct raft_cluster *cl, uint64_t hostid ) {
  struct hrauth_call hcall;
  struct xdr_s args;
  char argbuf[256];
  int sts;
  uint64_t lastterm, lastseq;
  
  raft_highest_storedseq( cl->clid, &lastterm, &lastseq );
  
  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  /* encode args */
  xdr_encode_uint64( &args, cl->clid );
  xdr_encode_uint64( &args, cl->term );
  xdr_encode_uint64( &args, lastterm ); /* last command term */
  xdr_encode_uint64( &args, lastseq ); /* last command seq */

  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT_RPC_PROG;
  hcall.vers = RAFT_RPC_VERS;
  hcall.proc = 2; /* vote */
  hcall.donecb = raft_vote_cb;
  hcall.cxt2 = cl->clid;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, &args, 1 );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "hrauth_call_async failed" );
    return;
  }

  
}


static void raft_putcmd_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t clid, hostid, term, seq;
  struct raft_cluster *cl;
  struct raft_member *mp;
  int i, success, sts;
  int sendsnap = 0;
  
  raft_log( LOG_LVL_TRACE, "raft_putcmd_cb %s", res ? "Success" : "Timeout" );
  
  clid = hcallp->cxt2;
  cl = cl_by_id( clid );
  if( !cl ) {
    raft_log( LOG_LVL_TRACE, "Unknown cluster" );
    return;
  }
  hostid = hcallp->hostid;

  mp = NULL;
  for( i = 0; i < cl->nmember; i++ ) {
    if( cl->member[i].hostid == hostid ) {
      mp = &cl->member[i];
      break;
    }
  }
  if( !mp ) {
    raft_log( LOG_LVL_ERROR, "Unknown member" );
    return;
  }

  if( !res ) {
    raft_log( LOG_LVL_TRACE, "raft_putcmd_cb timeout" );
    //raft_call_putcmd( cl, hostid, 0 );
    return;
  }

  
  sts = xdr_decode_boolean( res, &success );
  if( !sts ) sts = xdr_decode_uint64( res, &term );
  if( !sts ) sts = xdr_decode_uint64( res, &seq );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "Xdr decode error" );
    return;
  }

  if( mp->storedseq == seq ) {
    raft_log( LOG_LVL_TRACE, "storedseq unchanged - send snapshot" );
    sendsnap = 1;
  } else {
    mp->storedseq = seq;
  }
  mp->lastseen = time( NULL );
  raft_cluster_set( cl );

  /* check commitseq */
  raft_check_commitseq( cl );

  /* apply any commands now acked by quorum */
  raft_apply_commands( cl );

  /* send next command if any */
  if( sendsnap ) {
    raft_call_snapsave( cl, hostid, 0 );
  } else {
    raft_call_putcmd( cl, hostid, 0 );
  }
  
  return;  
}

static void raft_call_putcmd( struct raft_cluster *cl, uint64_t hostid, uint64_t cseq ) {
  struct hrauth_call hcall;
  struct xdr_s args[3];
  char argbuf[256], argbuf2[32];
  int sts, len, i;
  uint64_t cterm, pterm, pseq;
  
  if( cseq == 0 ) {
    for( i = 0; i < cl->nmember; i++ ) {
      if( cl->member[i].hostid == hostid ) {
	cseq = cl->member[i].storedseq + 1;
	break;
      }
    }
    if( cseq == 0 ) return;
  }

  len = raft_command_by_seq( cl->clid, cseq, &cterm, glob.buf, sizeof(glob.buf), NULL );
  if( len < 0 ) {
    raft_log( LOG_LVL_ERROR, "raft_call_putcmd: failed to find command clid=%"PRIx64" seq=%"PRIu64"", cl->clid, cseq );
    raft_call_ping( cl, hostid );
    return;
  }

  pseq = cseq - 1;
  sts = raft_command_by_seq( cl->clid, pseq, &pterm, NULL, 0, NULL );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "Failed to find previous entry seq=%"PRIu64"", pseq );
    pseq = 0;
    pterm = 0;
  }
    
  raft_log( LOG_LVL_TRACE, "raft_call_putcmd clid=%"PRIx64" hostid=%"PRIx64" cseq=%"PRIu64"", cl->clid, hostid, cseq );
  
  xdr_init( &args[0], (uint8_t *)argbuf, sizeof(argbuf) );
  xdr_init( &args[2], (uint8_t *)argbuf2, sizeof(argbuf2) );
  
  /* encode args */
  xdr_encode_uint64( &args[0], cl->clid );
  xdr_encode_uint64( &args[0], cl->term ); /* current term */
  xdr_encode_uint64( &args[0], cl->commitseq ); /* commitseq */
  xdr_encode_uint64( &args[0], pterm ); /* prev term/seq */
  xdr_encode_uint64( &args[0], pseq );
  xdr_encode_fixed( &args[0], (uint8_t *)cl->cookie, RAFT_MAX_COOKIE );
  xdr_encode_boolean( &args[0], 1 ); /* sending commands */  
  xdr_encode_uint64( &args[0], cterm ); /* command term */
  xdr_encode_uint64( &args[0], cseq ); /* command seq */
  xdr_encode_uint32( &args[0], len );

  xdr_init( &args[1], (uint8_t *)glob.buf, len );
  if( len % 4 ) len += 4 - (len % 4);
  args[1].offset = len;

  xdr_encode_boolean( &args[2], 0 ); /* no more commands */

  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT_RPC_PROG;
  hcall.vers = RAFT_RPC_VERS;
  hcall.proc = 1; /* append */
  hcall.donecb = raft_putcmd_cb;
  hcall.cxt2 = cl->clid;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, args, 3 );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "hrauth_call_async failed" );
    return;
  }

}

static void raft_snapsave_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t clid, hostid, term;
  struct raft_cluster *cl;
  struct raft_member *mp;
  int i, success, sts;
  uint32_t offset;
  
  raft_log( LOG_LVL_TRACE, "raft_snapsave_cb %s", res ? "Success" : "Timeout" );
  
  clid = hcallp->cxt2;
  cl = cl_by_id( clid );
  if( !cl ) {
    raft_log( LOG_LVL_TRACE, "Unknown cluster" );
    return;
  }
  hostid = hcallp->hostid;

  mp = NULL;
  for( i = 0; i < cl->nmember; i++ ) {
    if( cl->member[i].hostid == hostid ) {
      mp = &cl->member[i];
      break;
    }
  }
  if( !mp ) {
    raft_log( LOG_LVL_ERROR, "Unknown member" );
    return;
  }

  if( !res ) {
    raft_log( LOG_LVL_TRACE, "raft_snapsave_cb timeout - retrying" );
    raft_call_snapsave( cl, hostid, 0 );
    return;
  }

  
  sts = xdr_decode_boolean( res, &success );
  if( !sts ) sts = xdr_decode_uint64( res, &term );
  if( !sts ) sts = xdr_decode_uint32( res, &offset );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "Xdr decode error" );
    return;
  }

  mp->lastseen = time( NULL );
  raft_cluster_set( cl );

  if( term > cl->term ) {
    raft_convert_follower( cl, term, hostid );
    return;
  }
  
  /* send next block of snapshot, if any */
  if( offset > 0 ) raft_call_snapsave( cl, hostid, offset );
  
  return;  
}

static void raft_call_snapsave( struct raft_cluster *cl, uint64_t hostid, uint32_t offset ) {
  struct hrauth_call hcall;
  struct xdr_s args[2];
  char argbuf[256];
  int sts, len;
  struct raft_snapshot_info info;
  char *buf;
  
  raft_log( LOG_LVL_TRACE, "raft_call_snapsave clid=%"PRIx64" hostid=%"PRIx64"", cl->clid, hostid );
  sts = raft_snapshot_info( cl->clid, &info, NULL );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "Failed to find local snapshot - sending empty snapshot data" );
    memset( &info, 0, sizeof(info) );
    raft_command_seq( cl->clid, &info.term, &info.seq );
  }

  if( offset == info.size ) {
    len = 0;
    buf = NULL;
  } else if( offset > info.size ) {
    raft_log( LOG_LVL_ERROR, "snapshot offset too high" );
    return;
  } else {
    len = raft_snapshot_load( cl->clid, offset, glob.buf, sizeof(glob.buf) );
    buf = glob.buf;
  }

  xdr_init( &args[0], (uint8_t *)argbuf, sizeof(argbuf) );
  xdr_encode_uint64( &args[0], cl->clid );
  xdr_encode_uint64( &args[0], info.term );
  xdr_encode_uint64( &args[0], info.seq );
  xdr_encode_uint32( &args[0], offset );
  xdr_encode_uint32( &args[0], len );
  xdr_init( &args[1], (uint8_t *)buf, len );
  args[1].offset = len;
  if( len % 4 ) args[1].offset += 4 - (len % 4);
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT_RPC_PROG;
  hcall.vers = RAFT_RPC_VERS;
  hcall.proc = 4; /* snapsave */
  hcall.donecb = raft_snapsave_cb;
  hcall.cxt2 = cl->clid;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, args, 2 );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "hrauth_call_async failed" );
    return;
  }

}











static void raft_convert_follower( struct raft_cluster *cl, uint64_t term, uint64_t leaderid ) {
  int i;
  
  raft_log( LOG_LVL_INFO, "Convert to follower term=%"PRIu64" leader=%"PRIx64"", term, leaderid );  
  cl->state = RAFT_STATE_FOLLOWER;
  cl->term = term;
  cl->timeout = raft_term_timeout();
  cl->leaderid = leaderid;
  cl->voteid = 0;
  for( i = 0; i < cl->nmember; i++ ) {
    cl->member[i].flags &= ~RAFT_MEMBER_VOTED;
  }
  raft_cluster_set( cl );
  raft_set_iter_timeout();
}

static void raft_convert_candidate( struct raft_cluster *cl ) {
  int i;
  raft_log( LOG_LVL_INFO, "Convert to candidate" );
  cl->state = RAFT_STATE_CANDIDATE;
  cl->term++;
  cl->timeout = raft_elec_timeout();
  cl->leaderid = 0;
  cl->voteid = hostreg_localid();  
  for( i = 0; i < cl->nmember; i++ ) {
    cl->member[i].flags &= ~RAFT_MEMBER_VOTED;
    raft_call_vote( cl, cl->member[i].hostid );
  }
  raft_cluster_set( cl );
  raft_set_iter_timeout();  
}

static void raft_convert_leader( struct raft_cluster *cl ) {
  int i;
  uint64_t seq;
  
  raft_log( LOG_LVL_INFO, "Convert to leader cl=%"PRIx64"", cl->clid );

  cl->state = RAFT_STATE_LEADER;
  cl->leaderid = hostreg_localid();
  for( i = 0; i < cl->nmember; i++ ) {
    cl->member[i].flags &= ~RAFT_MEMBER_VOTED;
  }
  raft_cluster_set( cl );

  raft_command_seq( cl->clid, NULL, &seq );
  for( i = 0; i < cl->nmember; i++ ) {
    raft_log( LOG_LVL_TRACE, "raft_send_pings cl=%"PRIx64" i=%u hostid=%"PRIx64"",
	      cl->clid, i, cl->member[i].hostid );    
    raft_call_ping( cl, cl->member[i].hostid );
  }

  cl->timeout = rpc_now() + glob.prop.term_low / 2;  
  raft_cluster_set( cl );
  raft_set_iter_timeout();  
}


static int raft_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

/* 
 * send from leaders to keep their term alive and append new log entries 
 */
static int raft_proc_append( struct rpc_inc *inc ) {
  int handle, sts, success, i;
  uint64_t clid, hostid, term, commitseq, s, storedseq;
  uint64_t prevlogterm, prevlogseq, plogterm, entryid, pterm, pseq;
  struct raft_cluster *clp;
  struct hrauth_context *hc;
  struct xdr_s res;
  char resbuf[64];
  char *bufp;
  int len, b, found;
  char cookie[RAFT_MAX_COOKIE];
 
  /* we can guarantee this because we set a mandatory authenticator */
  hc = (struct hrauth_context *)inc->pcxt;
  hostid = hc->remoteid;
  success = 0;
  storedseq = 0;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &term ); /* current term */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &commitseq ); /* leaders commit seq */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &prevlogterm ); /* log term/seq immediately before the entries included */  
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &prevlogseq );
  if( !sts ) sts = xdr_decode_fixed( &inc->xdr, (uint8_t *)cookie, RAFT_MAX_COOKIE );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  raft_log( LOG_LVL_TRACE, "raft_proc_append clid=%"PRIx64" leaderid=%"PRIx64" term=%"PRIu64" commitseq=%"PRIu64"",
	     clid, hostid, term, commitseq );
  
  /* lookup cluster */
  clp = cl_by_id( clid );
  if( !clp ) {
    term = 0;
    goto done;
  }

  /* update last seen timestamp */
  found = 0;
  for( i = 0; i < clp->nmember; i++ ) {
    if( clp->member[i].hostid == hostid ) {
      clp->member[i].lastseen = time( NULL );
      found = 1;
      break;
    }
  }
  if( !found ) {
    raft_log( LOG_LVL_ERROR, "Unknown member clid=%"PRIx64" hostid=%"PRIx64"", clid, hostid );
    term = 0;    
    goto done;
  }  
  raft_cluster_set( clp );
  
  /* check term */
  if( term < clp->term ) {
    raft_log( LOG_LVL_WARN, "Term old - rejected" );
    term = clp->term;
    goto done;
  }

  if( term > clp->term ) {
    /* term increased, convert to follower */
    raft_log( LOG_LVL_INFO, "Term increased %"PRIu64" -> %"PRIu64" - Convert to follower %"PRIx64"", clp->term, term, hostid );
    clp->voteid = 0;
    raft_convert_follower( clp, term, hostid );
  }
  
  /* check we have this entry */
  if( prevlogseq > 0 ) {
    sts = raft_command_by_seq( clid, prevlogseq, &plogterm, NULL, 0, &entryid );
    if( sts < 0 ) {
      raft_log( LOG_LVL_ERROR, "Failed to find command at seq %"PRIu64"", prevlogseq );
#if 0
      clp->timeout = raft_term_timeout();
      raft_cluster_set( clp );
      goto done;
#endif
    }

    if( !sts && (plogterm != prevlogterm) ) {
      struct log_s *logp;
      raft_log( LOG_LVL_WARN, "Conflicting log entry found" );
      logp = clog_by_id( clid );
      if( logp ) log_truncate( logp, entryid, 0 );
    }
  }

  /* store any commands included */
  sts = xdr_decode_boolean( &inc->xdr, &b );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  while( b ) {
    bufp = NULL;
    sts = xdr_decode_uint64( &inc->xdr, &pterm );
    if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &pseq );
    if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &len );
    if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

    sts = raft_command_put( clid, pterm, pseq, bufp, len );
    if( sts ) {
      raft_log( LOG_LVL_ERROR, "Failed to store command seq=%"PRIu64"", pseq );
    }
    
    sts = xdr_decode_boolean( &inc->xdr, &b );
    if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );    
  }
  
  /* checks successful, we now accept the update and apply any changes required */
  clp->leaderid = hostid;
  clp->timeout = raft_term_timeout();
  clp->state = RAFT_STATE_FOLLOWER;
  memcpy( clp->cookie, cookie, RAFT_MAX_COOKIE );
  raft_cluster_set( clp );
  
  /* leader has incremented commitseq indicating quorum have received this entry */
  if( commitseq > clp->commitseq ) {
    raft_command_seq( clp->clid, NULL, &s );
    clp->commitseq = commitseq < s ? commitseq : s; /* set commitseq to min(commitseq, storedseq) */
    raft_cluster_set( clp );
  }

  /* apply any commands commited but not yet applied */
  raft_apply_commands( clp );

  /* reply with highest locally stored seq */
  raft_highest_storedseq( clid, NULL, &storedseq );
  success = 1;

 done:
  xdr_init( &res, (uint8_t *)resbuf, sizeof(resbuf) );
  xdr_encode_boolean( &res, success );
  xdr_encode_uint64( &res, term );
  xdr_encode_uint64( &res, storedseq );
  return hrauth_reply( inc, &res, 1 );
}

/* 
 * sent from candidates to gather votes 
 */
static int raft_proc_vote( struct rpc_inc *inc ) {
  int handle, sts, success, i, found;
  uint64_t clid, hostid, term, lastterm, lastseq, seq;
  struct raft_cluster *clp;
  struct hrauth_context *hc;
  struct xdr_s res;
  char resbuf[16];

  /* we can guarantee this because we set a mandatory authenticator */
  hc = (struct hrauth_context *)inc->pcxt;
  hostid = hc->remoteid;
  success = 0;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &term ); /* current term */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &lastterm ); /* last command term */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &lastseq ); /* last command seq */
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  raft_log( LOG_LVL_TRACE, "raft_proc_vote clid=%"PRIx64" candidateid=%"PRIx64" term=%"PRIu64" lastterm=%"PRIu64" lastseq=%"PRIu64"",
	     clid, hostid, term, lastterm, lastseq );
  
  clp = cl_by_id( clid );
  if( !clp ) {
    raft_log( LOG_LVL_ERROR, "Unknown cluster %"PRIx64"", clid );
    goto done;
  }

  /* update last seen timestamp */
  found = 0;
  for( i = 0; i < clp->nmember; i++ ) {
    if( clp->member[i].hostid == hostid ) {      
      clp->member[i].lastseen = time( NULL );
      found = 1;
      break;
    }
  }
  if( !found ) {
    raft_log( LOG_LVL_ERROR, "Unknown member clid=%"PRIx64" hostid=%"PRIx64"", clid, hostid );
    goto done;
  }
  raft_cluster_set( clp );
  
  if( term < clp->term ) {
    /* term too old, reject */
    raft_log( LOG_LVL_WARN, "Old term %"PRIu64" < %"PRIu64" - rejecting", term, clp->term );
    term = clp->term;
    goto done;
  }

  if( term > clp->term ) {
    /* term increased, convert to follower */
    raft_log( LOG_LVL_INFO, "Term increased %"PRIu64" -> %"PRIu64" - convert to follower leader=%"PRIx64"", clp->term, term, hostid );
    raft_convert_follower( clp, term, hostid );
  }

  /* grant vote if not voted yet or voted for this host already AND the candidate is at least as up to date as us */
  sts = raft_highest_storedseq( clp->clid, NULL, &seq );
  if( !sts && ((clp->voteid == 0) || (clp->voteid == hostid)) && (lastseq >= seq) ) {
    raft_log( LOG_LVL_DEBUG, "Granting vote" );
    clp->voteid = hostid;
    success = 1;
  } else {
    raft_log( LOG_LVL_INFO, "Denying vote request (%s) clid=%"PRIx64" hostid=%"PRIx64"",
	      sts ? "Failed to get command seq" : 
	      (clp->voteid != 0) ? "Already voted this election" :
	      lastseq < seq ? "Candidate seq too low" :
	      "Other",
	      clid, hostid );
  }

  
 done:
  xdr_init( &res, (uint8_t *)resbuf, sizeof(resbuf) );
  xdr_encode_boolean( &res, success );
  xdr_encode_uint64( &res, term );
  return hrauth_reply( inc, &res, 1 );
}

/*
 * rpc interface to allow appending commands 
 */
static int raft_proc_command( struct rpc_inc *inc ) {
  int handle, sts;
  char *bufp = NULL;
  int len;
  uint64_t clid, cseq;
  struct raft_cluster *cl;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &len );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  raft_log( LOG_LVL_TRACE, "raft_proc_command clid=%"PRIx64" len=%u", clid, len );

  cl = cl_by_id( clid );
  if( !cl ) {
    raft_log( LOG_LVL_TRACE, "Unknown cluster" );
    sts = -1;
  } else if( cl->state != RAFT_STATE_LEADER ) {
    raft_log( LOG_LVL_TRACE, "Not leader" );
    sts = -1;
  } else {
    sts = raft_cluster_command( clid, bufp, len, &cseq );
    if( sts ) raft_log( LOG_LVL_TRACE, "raft_proc_command failure" );
  }
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
  xdr_encode_uint64( &inc->xdr, sts ? 0 : cseq );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

/* internal procedure used to distribute snapshots */
static int raft_proc_snapsave( struct rpc_inc *inc ) {
  int handle, sts;
  char *bufp = NULL;
  int len;
  uint64_t clid, term, seq, bterm, entryid;
  struct raft_cluster *cl;
  uint32_t offset;
  struct xdr_s res;
  char resbuf[64];
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &term );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &seq );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &offset );  
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &len );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  raft_log( LOG_LVL_TRACE, "raft_proc_snapsave clid=%"PRIx64" term=%"PRIu64" seq=%"PRIu64" len=%u", clid, term, seq, len );

  cl = cl_by_id( clid );
  
  sts = raft_snapshot_save( clid, term, seq, offset, bufp, len );
  if( len == 0 ) {
    struct raft_app *app;
    
    raft_log( LOG_LVL_INFO, "reloading state from snapshot" );
    app = raft_app_by_appid( cl->appid );
    if( app && app->snapload ) {
      struct mmf_s mmf;
      char clstr[64];
      sprintf( clstr, "%"PRIx64"-snapshot.dat", clid );
      
      sts = mmf_open2( mmf_default_path( "raft", clstr, NULL ), &mmf, MMF_OPEN_EXISTING );
      if( !sts ) {
	sts = mmf_remap( &mmf, mmf.fsize );
	app->snapload( app, cl, (char *)mmf.file + sizeof(struct raft_snapshot_info), mmf.fsize - sizeof(struct raft_snapshot_info) );
	mmf_close( &mmf );
      }
    }

    /* truncate log to here */
    sts = raft_command_by_seq( clid, seq, &bterm, NULL, 0, &entryid );
    if( !sts && entryid ) log_truncate( clog_by_id( clid ), entryid, LOG_TRUNC_END );

    sts = 0;
    offset = 0;
  } else {
    offset += len;
  }

  xdr_init( &res, (uint8_t *)resbuf, sizeof(resbuf) );
  xdr_encode_boolean( &res, sts ? 0 : 1 );
  xdr_encode_uint64( &res, cl->term );
  xdr_encode_uint32( &res, offset );
  return hrauth_reply( inc, &res, 1 );
}

/*  
 * rpc interface to instruct raft to take a snapshot and compact log 
 */
static int raft_proc_snapshot( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t clid;
  struct raft_app *app;
  struct raft_cluster *cl;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );
  
  raft_log( LOG_LVL_TRACE, "raft_proc_snapshot clid=%"PRIx64"", clid );
  cl = cl_by_id( clid );
  if( cl ) {
    raft_log( LOG_LVL_INFO, "Requesting snapshot clid=%"PRIx64"", clid );
    app = raft_app_by_appid( cl->appid );
    if( app && app->snapsave ) {
      app->snapsave( app, cl, cl->term, cl->appliedseq );
    }
  }
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, cl ? 1 : 0 );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

/*
 * rpc interface that allows distributing cluster changes
 */
static int raft_proc_change( struct rpc_inc *inc ) {
  int sts, handle;  
  char cmdbuf[256];
  struct xdr_s xdr;
  int i, bmembers, bcookie, bappid, breset;
  uint64_t rclid, clid;
  char cookie[RAFT_MAX_COOKIE];
  uint64_t members[RAFT_MAX_MEMBER];
  uint32_t nmembers, appid;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( sts ) goto bad;
  sts = xdr_decode_boolean( &inc->xdr, &bcookie );
  if( sts ) goto bad;
  if( bcookie ) {
    sts = xdr_decode_fixed( &inc->xdr, (uint8_t *)cookie, RAFT_MAX_COOKIE );
    if( sts ) goto bad;
  }
  sts = xdr_decode_boolean( &inc->xdr, &bmembers );
  if( sts ) goto bad;
  if( bmembers ) {
    sts = xdr_decode_uint32( &inc->xdr, &nmembers );
    if( sts ) goto bad;
    if( nmembers > RAFT_MAX_MEMBER ) {
      sts = -1;
      goto bad;
    }
    for( i = 0; i < nmembers; i++ ) {
      sts = xdr_decode_uint64( &inc->xdr, &members[i] );
      if( sts ) goto bad;
    }
  }
  
  sts = xdr_decode_boolean( &inc->xdr, &bappid );
  if( sts ) goto bad;
  if( bappid ) {
    sts = xdr_decode_uint32( &inc->xdr, &appid );
    if( sts ) goto bad;
  }
  sts = xdr_decode_boolean( &inc->xdr, &breset );
  if( sts ) goto bad;
  
 bad:
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  raft_log( LOG_LVL_TRACE, "raft_proc_change clid=%"PRIx64" %s %s %s %s",
	    clid,
	    bcookie ? "cookie" : "",
	    bmembers ? "members" : "",
	    bappid ? "appid" : "",
	    breset ? "reset" : "" );
  
  /* 
   * Lookup first cluster with appid=RAFT_RPC_PROG. This is the raft metadata distributor cluster
   * i.e. all nodes in that cluster get sent the change command 
   */
  rclid = raft_clid_by_appid( RAFT_RPC_PROG );
  if( !rclid ) goto done;
  
  xdr_init( &xdr, (uint8_t *)cmdbuf, sizeof(cmdbuf) );
  xdr_encode_uint64( &xdr, clid );
  xdr_encode_boolean( &xdr, bcookie );
  if( bcookie ) xdr_encode_fixed( &xdr, (uint8_t *)cookie, RAFT_MAX_COOKIE );
  xdr_encode_boolean( &xdr, bmembers );
  if( bmembers ) {
    xdr_encode_uint32( &xdr, nmembers );
    for( i = 0; i < nmembers; i++ ) {
      xdr_encode_uint64( &xdr, members[i] );
    }
  }
  xdr_encode_boolean( &xdr, bappid );
  if( bappid ) xdr_encode_uint32( &xdr, appid );
  xdr_encode_boolean( &xdr, breset );
  
  raft_cluster_command( rclid, cmdbuf, xdr.offset, NULL );


 done:
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static struct rpc_proc raft_procs[] = {
  { 0, raft_proc_null },
  { 1, raft_proc_append },
  { 2, raft_proc_vote },  
  { 3, raft_proc_command },
  { 4, raft_proc_snapsave },
  { 5, raft_proc_snapshot },
  { 6, raft_proc_change },
  { 0, NULL }
};

static struct rpc_version raft_vers = {
  NULL, RAFT_RPC_VERS, raft_procs
};

static uint32_t raft_prog_auths[] = { RPC_AUTH_HRAUTH, 0 };
static struct rpc_program raft_prog = {
    NULL, RAFT_RPC_PROG, &raft_vers, raft_prog_auths
};


static void raft_iter_cb( struct rpc_iterator *iter ) {
  uint64_t now;
  int i, j;
  
  //raft_log( LOG_LVL_TRACE, "raft iterator" );
  raft_prop( &glob.prop );
  glob.ncl = raft_cluster_list( glob.cl, RAFT_MAX_CLUSTER );

  /* open all logs */
  for( i = 0; i < glob.ncl; i++ ) {
    clog_by_id( glob.cl[i].clid );
  }
  
  /* ensure connections registered */
  for( i = 0; i < glob.ncl; i++ ) {
    for( j = 0; j < glob.cl[i].nmember; j++ ) {
      hrauth_conn_register( glob.cl[i].member[j].hostid, NULL );
    }
  }
  
  /* check timeouts */
  now = rpc_now();
  for( i = 0; i < glob.ncl; i++ ) {
    if( now >= glob.cl[i].timeout ) {
      switch( glob.cl[i].state ) {
      case RAFT_STATE_FOLLOWER:
	/* term timeout - convert to candidate */
	if( !(glob.cl[i].flags & RAFT_CLUSTER_WITNESS) ) {
	  raft_log( LOG_LVL_TRACE, "%"PRIx64" Term timeout %u > %u - convert to candidate", glob.cl[i].clid, (uint32_t)now, (uint32_t)glob.cl[i].timeout );
	  raft_convert_candidate( &glob.cl[i] );
	} else {
	  raft_log( LOG_LVL_ERROR, "%"PRIx64" Term timeout - witness node cannot be candidate", glob.cl[i].clid );
	  glob.cl[i].timeout = raft_term_timeout();
	  raft_cluster_set( &glob.cl[i] );
	}
	break;
      case RAFT_STATE_CANDIDATE:
	/* election timeout - start new election */
	raft_log( LOG_LVL_TRACE, "%"PRIx64" Election timeout - new election", glob.cl[i].clid );
	raft_convert_candidate( &glob.cl[i] );
	break;
      case RAFT_STATE_LEADER:
	/* send pings to keep term alive */
	raft_log( LOG_LVL_TRACE, "%"PRIx64" Send pings to keep term alive", glob.cl[i].clid );
	raft_send_pings( &glob.cl[i] );
	glob.cl[i].timeout = rpc_now() + glob.prop.term_low / 2;
	raft_cluster_set( &glob.cl[i] );
	break;
      }
    }
  }

  /* set timeout to min cluster timeout */
  raft_set_iter_timeout();
}


static struct rpc_iterator raft_iter =
  {
   NULL,
   0,
   1000,
   raft_iter_cb,
   NULL
  };

static void raft_set_iter_timeout( void ) {
  int i;
  uint64_t to;
  
  /* set timeout to min cluster timeout */
  to = raft_iter.timeout;
  for( i = 0; i < glob.ncl; i++ ) {
    if( glob.cl[i].timeout < to ) to = glob.cl[i].timeout;
  }
  raft_iter.timeout = to;  
}

static struct raft_app raft_app =
{
   NULL,
   RAFT_RPC_PROG,
   raft_app_command,
};


void raft_register( void ) {
  int i;
  
  raft_open();
  glob.ncl = raft_cluster_list( glob.cl, RAFT_MAX_CLUSTER );

  /* initialize by immediately going into follower state */
  for( i = 0; i < glob.ncl; i++ ) {
    glob.cl[i].state = RAFT_STATE_FOLLOWER;
    glob.cl[i].timeout = 0;
    glob.cl[i].voteid = 0;
    raft_cluster_set( &glob.cl[i] );
  }

  rpc_program_register( &raft_prog );
  rpc_iterator_register( &raft_iter );
  raft_app_register( &raft_app );
}

int raft_app_register( struct raft_app *app ) {
  struct raft_app *ap;
  ap = glob.app;
  while( ap ) {
    if( ap == app ) return -1;
    ap = ap->next;
  }
  app->next = glob.app;
  glob.app = app;
  return 0;    
}

static struct raft_app *raft_app_by_appid( uint32_t appid ) {
  struct raft_app *app;
  app = glob.app;
  while( app ) {
    if( app->appid == appid ) return app;
    app = app->next;
  }
  return NULL;
}
    
  
static int raft_call_command( struct raft_cluster *cl, char *buf, int len ) {
  struct hrauth_call hcall;
  struct xdr_s args[3];
  char argbuf[256], tmpbuf[4];
  int sts;

  if( !cl->leaderid ) {
    raft_log( LOG_LVL_ERROR, "No leader" );
    return -1;
  }
  
  xdr_init( &args[0], (uint8_t *)argbuf, sizeof(argbuf) );
  /* encode args */
  xdr_encode_uint64( &args[0], cl->clid );
  xdr_encode_uint32( &args[0], len );
  xdr_init( &args[1], (uint8_t *)buf, len );
  args[1].offset = len;
  /* extra to cope if len is not a multiple of 4 */
  if( len % 4 ) {
    memset( tmpbuf, 0, 4 );
    xdr_init( &args[2], (uint8_t *)tmpbuf, 4 );
    args[2].offset = 4 - (len % 4);
  } else {
    xdr_init( &args[2], NULL, 0 );
  }
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = cl->leaderid;
  hcall.prog = RAFT_RPC_PROG;
  hcall.vers = RAFT_RPC_VERS;
  hcall.proc = 3; /* command */
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, args, 3 );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "hrauth_call_async failed" );
    return -1;
  }

  return 0;
}

int raft_cluster_command( uint64_t clid, char *buf, int len, uint64_t *cseq ) {
  struct raft_cluster *cl;
  int sts, i;
  uint64_t seq;

  raft_log( LOG_LVL_TRACE, "raft_cluster_command clid=%"PRIx64" len=%u", clid, len );
  
  if( cseq ) *cseq = 0;

  if( len > RAFT_MAX_COMMAND ) return -1;

  if( !rpcdp() ) {
    /* not running in rpcd so need to make rpc call out to local daemon */
    
    struct xdr_s args[2];
    struct hrauth_call hcall;
    char argbuf[32];

    xdr_init( &args[0], (uint8_t *)argbuf, sizeof(argbuf) );
    xdr_encode_uint64( &args[0], clid );
    xdr_encode_uint32( &args[0], len );
    xdr_init( &args[1], (uint8_t *)buf, len );
    args[1].offset = len;

    memset( &hcall, 0, sizeof(hcall) );
    hcall.hostid = hostreg_localid();
    hcall.prog = RAFT_RPC_PROG;
    hcall.vers = RAFT_RPC_VERS;
    hcall.proc = 3; /* command */
    sts = hrauth_call_udp_async( &hcall, args, 2, NULL );
    if( sts ) return -1;

    return 0;
  }

  
  /* lookup cluster */
  cl = cl_by_id( clid );
  if( !cl ) {
    raft_log( LOG_LVL_TRACE, "Unknown cluster" );
    return -1;
  }
    
  /* if not leader then send it */
  if( cl->state != RAFT_STATE_LEADER ) {
    raft_log( LOG_LVL_INFO, "Not leader - forwarding command" );
    return raft_call_command( cl, buf, len );
  }
    
  /* save buffer locally */
  sts = raft_command_seq( clid, NULL, &seq );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "Failed to get command seq" );
    return sts;
  }
    
  sts = raft_command_put( clid, cl->term, seq + 1, buf, len );
  if( sts ) {
    raft_log( LOG_LVL_ERROR, "Failed to store command buffer seq=%"PRIu64"", seq + 1 );
    return -1;
  }
    
  /* distribute buffer */
  for( i = 0; i < cl->nmember; i++ ) {
    raft_call_putcmd( cl, cl->member[i].hostid, 0 );
  }
    
  if( cseq ) *cseq = seq + 1;
  
  return 0;
}


/* ---- snapshots --- */

int raft_snapshot_save( uint64_t clid, uint64_t term, uint64_t seq, uint32_t offset, char *buf, int len ) {
  int sts;
  struct mmf_s mmf;
  char clstr[64];
  char *path;
  struct raft_snapshot_info info;
  
  sprintf( clstr, "%"PRIx64"-snapshot.dat.new", clid );
  path = mmf_default_path( "raft", clstr, NULL );
  if( offset == 0 ) {
    /* start new file */
    mmf_delete_file( path );
  }

  sts = mmf_open2( path, &mmf, offset == 0 ? 0 : MMF_OPEN_EXISTING );
  if( sts ) return sts;

  /* offset==0 means new file so write header */
  if( offset == 0 ) {
    memset( &info, 0, sizeof(info) );
    info.magic = RAFT_SNAPSHOT_MAGIC;
    info.version = RAFT_SNAPSHOT_VERSION;
    info.clid = clid;
    info.term = term;
    info.seq = seq;
    mmf_write( &mmf, (char *)&info, sizeof(info), 0 );
  }
  
  if( len > 0 ) {
    offset += sizeof(info);
    mmf_write( &mmf, buf, len, offset );

    mmf_read( &mmf, (char *)&info, sizeof(info), 0 );
    info.size += len;
    mmf_write( &mmf, (char *)&info, sizeof(info), 0 );    
  } else if( len == 0 ) {
    /* final block - mark as complete */
    mmf_read( &mmf, (char *)&info, sizeof(info), 0 );
    info.complete = 1;
    mmf_write( &mmf, (char *)&info, sizeof(info), 0 );
  }
  
  mmf_close( &mmf );
  
  if( len == 0 ) {
    /* this was final block - rename over the top of the old snapshot file */
    char clstr2[64];
    struct log_s *log;
    struct log_entry entry;
    int ne;
    
    sprintf( clstr2, "%"PRIx64"-snapshot.dat", clid );
    mmf_rename( mmf_default_path( "raft", NULL ), clstr, clstr2 );

    /* truncate log to free space */
    log = clog_by_id( clid );
    if( log ) {
      memset( &entry, 0, sizeof(entry) );
      sts = log_read_end( log, 0, &entry, 1, &ne );
      if( !sts && ne ) log_truncate( log, entry.id, LOG_TRUNC_END );
    }
  }

  return 0;
}

int raft_snapshot_info( uint64_t clid, struct raft_snapshot_info *info, struct raft_snapshot_info *newinfo ) {
  int sts;
  struct mmf_s mmf;
  char clstr[64];

  if( info ) {
    sprintf( clstr, "%"PRIx64"-snapshot.dat", clid );
    sts = mmf_open2( mmf_default_path( "raft", clstr, NULL ), &mmf, MMF_OPEN_EXISTING );
    if( sts ) return sts;
    mmf_read( &mmf, (char *)info, sizeof(*info), 0 );
    mmf_close( &mmf );
  }
  
  if( newinfo ) {
    sprintf( clstr, "%"PRIx64"-snapshot.dat.new", clid );
    sts = mmf_open2( mmf_default_path( "raft", clstr, NULL ), &mmf, MMF_OPEN_EXISTING );
    if( sts ) return sts;    
    mmf_read( &mmf, (char *)newinfo, sizeof(*newinfo), 0 );    
    mmf_close( &mmf );    
  }

  
  return 0;
}

int raft_snapshot_load( uint64_t clid, uint32_t offset, char *buf, int len ) {
  int sts;
  char clstr[64];
  struct mmf_s mmf;

  sprintf( clstr, "%"PRIx64"-snapshot.dat", clid );
  sts = mmf_open2( mmf_default_path( "raft", clstr, NULL ), &mmf, MMF_OPEN_EXISTING );
  if( sts ) return sts;

  sts = mmf_read( &mmf, buf, len, offset + sizeof(struct raft_snapshot_info) );
  
  mmf_close( &mmf );
  return sts;
}



/* --------------------------- raft app ----------------------- */

/*
 * We register our own application to facilitate distributing raft cluster configuration changes
 * across clusters.
 */

static void raft_app_command( struct raft_app *app, struct raft_cluster *cl, uint64_t seq, char *buf, int len ) {
  struct xdr_s xdr;
  int sts, i, bcookie, bmembers, bappid, breset;
  uint64_t clid;
  char cookie[RAFT_MAX_COOKIE];
  uint32_t nmember, appid;
  uint64_t member[RAFT_MAX_MEMBER];
  struct raft_cluster *clp;
  
  xdr_init( &xdr, (uint8_t *)buf, len );
  sts = xdr_decode_uint64( &xdr, &clid );
  if( sts ) return;

  sts = xdr_decode_boolean( &xdr, &bcookie );
  if( sts ) return;
  if( bcookie ) {
    sts = xdr_decode_fixed( &xdr, (uint8_t *)cookie, RAFT_MAX_COOKIE );
    if( sts ) return;
  }

  sts = xdr_decode_boolean( &xdr, &bmembers );
  if( sts ) return;
  if( bmembers ) {
    sts = xdr_decode_uint32( &xdr, &nmember );
    if( sts ) return;
    if( nmember > RAFT_MAX_MEMBER ) return;
    for( i = 0; i < nmember; i++ ) {
      sts = xdr_decode_uint64( &xdr, &member[i] );
      if( sts ) return;
    }
  }

  sts = xdr_decode_boolean( &xdr, &bappid );
  if( sts ) return;
  if( bappid ) {
    sts = xdr_decode_uint32( &xdr, &appid );
    if( sts ) return;
  }
  sts = xdr_decode_boolean( &xdr, &breset );
  if( sts ) return;
  
  raft_log( LOG_LVL_INFO, "raft change %"PRIx64" %s %s %s %s",
	    clid,
	    bcookie ? "cookie" : "", bmembers ? "members" : "",
	    bappid ? "appid" : "", breset ? "reset" : "" );
  
  clp = cl_by_id( clid );
  if( !clp ) {
    struct raft_cluster clpp;
    memset( &clpp, 0, sizeof(clpp) );
    clpp.clid = clid;
    if( bcookie ) memcpy( clpp.cookie, cookie, RAFT_MAX_COOKIE );
    if( bmembers ) {
      clpp.nmember = 0;
      for( i = 0; i < nmember; i++ ) {
	if( member[i] != hostreg_localid() ) {
	  clpp.member[clpp.nmember].hostid = member[i];
	  clpp.nmember++;
	}
      }
    }
    if( bappid ) clpp.appid = appid;
    raft_cluster_set( &clpp );
    return;
  }

  if( bcookie ) {
    if( memcmp( clp->cookie, (uint8_t *)cookie, RAFT_MAX_COOKIE ) != 0 ) {
      raft_log( LOG_LVL_INFO, "Raft cookie changing" );
      memcpy( clp->cookie, cookie, RAFT_MAX_COOKIE );
    }
  }

  if( bmembers ) {
    clp->nmember = 0;
    for( i = 0; i < nmember; i++ ) {
      if( (member[i] != hostreg_localid()) ) {
	raft_log( LOG_LVL_INFO, "Raft member changing %"PRIx64" -> %"PRIx64"", clp->member[clp->nmember].hostid, member[i] );
	
	clp->member[clp->nmember].hostid = member[i];
	clp->nmember++;
      }
    }
  }

  if( bappid ) {
    clp->appid = appid;
  }

  if( breset ) {
    struct log_s *logp;
    char clstr[64];
    char *path;
    
    clp->term = 1;
    clp->commitseq = 0;
    clp->appliedseq = 0;
    clp->state = RAFT_STATE_FOLLOWER;
    clp->leaderid = 0;
    clp->voteid = 0;
    
    /* delete log entries */
    logp = clog_by_id( clp->clid );
    if( logp ) log_reset( logp );
    
    /* delete snapshot */
    sprintf( clstr, "%"PRIx64"-snapshot.dat.new", cl->clid );
    path = mmf_default_path( "raft", clstr, NULL );
    mmf_delete_file( path );
  }
  
  raft_cluster_set( clp );
  
}


