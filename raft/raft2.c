
#include <fju/raft2.h>
#include <fju/mmf.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/hostreg.h>
#include <fju/hrauth.h>
#include <fju/programs.h>
#include <fju/log.h>
#include <fju/sec.h>

static log_deflogger(raft2_log, RAFT2_RPC_PROG)
  
struct raft2_file {
  struct raft2_prop prop;
  struct raft2_cluster cluster[RAFT2_MAX_CLUSTER];
};

struct raft2_log {
  uint64_t clid;
  struct log_s log;
};

static struct {
  uint32_t ocount;
  struct mmf_s mmf;
  struct raft2_file *file;

  struct raft2_prop prop;
  struct raft2_cluster cl[RAFT2_MAX_CLUSTER];
  struct raft2_log clog[RAFT2_MAX_CLUSTER];
  uint32_t ncl;
  struct raft2_app *app;

  char buf[RAFT2_MAX_COMMAND];
} glob;

static uint64_t raft2_term_timeout( void ) {
  return rpc_now() + glob.prop.term_low +
    (sec_rand_uint32() % (glob.prop.term_high - glob.prop.term_low));
}

static uint64_t raft2_elec_timeout( void ) {
  return rpc_now() + glob.prop.elec_low +
    (sec_rand_uint32() % (glob.prop.elec_high - glob.prop.elec_low));
}

static void raft2_lock( void ) {
  mmf_lock( &glob.mmf );
}
static void raft2_unlock( void ) {
  mmf_unlock( &glob.mmf );
}

int raft2_open( void ) {
  int sts;

  if( glob.ocount > 0 ) {
    glob.ocount++;
    return 0;
  }

  sts = mmf_open( mmf_default_path( "raft", "raft2.dat", NULL ), &glob.mmf );
  if( sts ) return sts;

  sts = mmf_remap( &glob.mmf, sizeof(*glob.file) );

  glob.file = (struct raft2_file *)glob.mmf.file;

  if( glob.file->prop.magic != RAFT2_MAGIC ) {
    glob.file->prop.magic = RAFT2_MAGIC;
    glob.file->prop.version = RAFT2_VERSION;
    glob.file->prop.seq = 1;
    glob.file->prop.count = 0;
    glob.file->prop.flags = 0;
    glob.file->prop.elec_low = 1000;
    glob.file->prop.elec_high = 3000;
    glob.file->prop.term_low = 3000;
    glob.file->prop.term_high = 8000;
    glob.file->prop.rpc_timeout = 200;
  }
  
  glob.ocount = 1;
  return 0;
}

int raft2_close( void ) {
  if( glob.ocount == 0 ) return -1;
  if( glob.ocount > 1 ) {
    glob.ocount--;
    return 0;
  }
  mmf_close( &glob.mmf );
  glob.ocount = 0;
  return 0;
}

int raft2_prop( struct raft2_prop *prop ) {
  if( !glob.ocount ) return -1;
  raft2_lock();
  *prop = glob.file->prop;
  raft2_unlock();
  return 0;
}

int raft2_prop_set( uint32_t mask, struct raft2_prop *prop ) {
  if( !glob.ocount ) return -1;
  raft2_lock();
  if( mask & RAFT2_PROP_ELEC_LOW ) {
    glob.file->prop.elec_low = prop->elec_low;
  }
  if( mask & RAFT2_PROP_ELEC_HIGH ) {
    glob.file->prop.elec_high = prop->elec_high;
  }
  if( mask & RAFT2_PROP_TERM_LOW ) {
    glob.file->prop.term_low = prop->term_low;
  }
  if( mask & RAFT2_PROP_TERM_HIGH ) {
    glob.file->prop.term_high = prop->term_high;
  }
  if( mask & RAFT2_PROP_RPC_TIMEOUT ) {
    glob.file->prop.rpc_timeout = prop->rpc_timeout;
  }
  raft2_unlock();
  return 0;
}  


/* database functions */
int raft2_cluster_list( struct raft2_cluster *cl, int ncl ) {
  int i;
  if( !glob.ocount ) return -1;
  raft2_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( i < ncl ) cl[i] = glob.file->cluster[i];
  }
  i = glob.file->prop.count;
  raft2_unlock();
  return i;
}

int raft2_clid_list( uint64_t *clid, int ncl ) {
  int i;
  if( !glob.ocount ) return -1;
  raft2_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( i < ncl ) clid[i] = glob.file->cluster[i].clid;
  }
  i = glob.file->prop.count;
  raft2_unlock();
  return i;  
}

int raft2_cluster_by_clid( uint64_t clid, struct raft2_cluster *cl ) {
  int i, sts;

  sts = -1;
  if( !glob.ocount ) return -1;
  raft2_lock();
  for( i = 0; i < glob.file->prop.count; i++ ) {
    if( glob.file->cluster[i].clid == clid ) {
      if( cl ) *cl = glob.file->cluster[i];
      sts = 0;
      break;
    }
  }
  raft2_unlock();
  return sts;
}

int raft2_cluster_set( struct raft2_cluster *cl ) {
  int i, sts;

  if( !cl->clid ) sec_rand( &cl->clid, sizeof(cl->clid) );
  
  sts = -1;
  if( !glob.ocount ) return -1;
  raft2_lock();
  
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

  if( glob.file->prop.count >= RAFT2_MAX_CLUSTER ) goto done;

  i = glob.file->prop.count;
  cl->term = 1;
  cl->appliedseq = 0;
  glob.file->cluster[i] = *cl;
  glob.file->prop.count++;
  glob.file->prop.seq++;
  sts = 0;
  
 done:
  raft2_unlock();
  return sts;  
}

int raft2_cluster_rem( uint64_t clid ) {
  int i, sts;

  sts = -1;
  if( !glob.ocount ) return -1;
  raft2_lock();
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
  raft2_unlock();
  return sts;    
}

int raft2_log_open( uint64_t clid, struct log_s *log ) {
  char clstr[64];
  struct log_opts opts;
  
  sprintf( clstr, "%"PRIx64".log", clid );
  memset( &opts, 0, sizeof(opts) );
  opts.mask = LOG_OPT_COOKIE|LOG_OPT_FLAGS;
  strcpy( opts.cookie, "raft" );
  opts.flags = LOG_FLAG_FIXED|LOG_FLAG_GROW;
  return log_open( mmf_default_path( "raft", clstr, NULL ), &opts, log );
}

/* ------------ rpc -------------------- */

static void raft2_convert_follower( struct raft2_cluster *cl, uint64_t term, uint64_t leaderid );
static void raft2_convert_candidate( struct raft2_cluster *cl );
static void raft2_convert_leader( struct raft2_cluster *cl );
static void raft2_set_iter_timeout( void );
static struct raft2_app *raft2_app_by_appid( uint32_t appid );
static void raft2_call_putcmd( struct raft2_cluster *cl, uint64_t hostid, uint64_t cseq );

static struct raft2_cluster *cl_by_id( uint64_t clid ) {
  int i;
  for( i = 0; i < glob.ncl; i++ ) {
    if( glob.cl[i].clid == clid ) return &glob.cl[i];
  }
  return NULL;
}
static struct log_s *clog_by_id( uint64_t clid ) {
  int i;
  /* return already open log */
  for( i = 0; RAFT2_MAX_CLUSTER; i++ ) {
    if( glob.clog[i].clid == clid ) return &glob.clog[i].log;
  }

  /* open a new log descriptor */
  for( i = 0; i < RAFT2_MAX_CLUSTER; i++ ) {
    if( glob.clog[i].clid == 0 ) {
      glob.clog[i].clid = clid;
      raft2_log_open( clid, &glob.clog[i].log );
      return &glob.clog[i].log;
    }
  }

  /* all a log we don't need anymore */
  for( i = 0; i < RAFT2_MAX_CLUSTER; i++ ) {
    if( !cl_by_id( glob.clog[i].clid ) ) {
      log_close( &glob.clog[i].log );
      glob.clog[i].clid = clid;
      raft2_log_open( clid, &glob.clog[i].log );
      return &glob.clog[i].log;
    }
  }

  /* failed? "this should never happen" */
  raft2_log( LOG_LVL_ERROR, "Failed to open command log clid=%"PRIx64"", clid ); 
  return NULL;    
}

struct raft2_cmd_header {
  uint64_t term;
  uint64_t seq;
};

/* read command buffer. returns -1 on error or command buffer length on success */
static int raft2_command_by_seq( uint64_t clid, uint64_t seq, uint64_t *term, char *buf, int len, uint64_t *entryid ) {
  int sts, ne;
  struct log_s *log;
  struct log_entry entry;
  struct log_iov iov[1];
  struct raft2_cmd_header hdr;
  uint64_t id;
  
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
  entry.niov = 1;
  id = 0;
  while( 1 ) {
    sts = log_read_end( log, id, &entry, 1, &ne );
    if( sts || !ne ) break;

    if( hdr.seq == seq ) {
      if( term ) *term = hdr.term;
      log_read_buf( log, entry.id, buf, len, NULL );
      if( entryid ) *entryid = entry.id;
      return entry.msglen;
    }

    id = entry.id;
  }

  return -1;  
}

/* get highest seq written */
static int raft2_command_seq( uint64_t clid, uint64_t *term, uint64_t *seq ) {
  int sts, ne;
  struct log_s *log;
  struct log_entry entry;
  struct log_iov iov[1];
  struct raft2_cmd_header hdr;

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

/* store a command buffer */
static int raft2_command_put( uint64_t clid, uint64_t term, uint64_t seq, char *buf, int len ) {
  int sts;
  struct log_s *log;
  struct log_entry entry;
  struct log_iov iov[2];
  struct raft2_cmd_header hdr;
  uint64_t nseq;

  /* get highest seq written */
  sts = raft2_command_seq( clid, NULL, &nseq );
  if( sts ) return sts;

  /* disallow writing entries out of order */
  if( (nseq == 0) || (seq != (nseq - 1)) ) return -1;
  
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

  /* wait for command to be written to stable storage */
  log_sync( log, MMF_SYNC_NOW );
  return 0;
}

static int raft2_cluster_quorum( struct raft2_cluster *cl ) {
  return ((1 + cl->nmember) / 2) + 1;
}

/* find the highest storedseq confirmed on quorum of nodes */
static uint64_t raft2_quorum_commitseq( struct raft2_cluster *cl ) {
  int i, q, c;
  uint64_t lseq;

  /* get quorum */
  q = raft2_cluster_quorum( cl );

  /* get local highest stored seq */
  raft2_command_seq( cl->clid, NULL, &lseq );

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

static void raft2_apply_commands( struct raft2_cluster *cl ) {
  struct raft2_app *app;
  uint64_t s;
  int sts;
  
  /* apply any commands commited but not yet applied */
  app = raft2_app_by_appid( cl->appid );
  if( app && cl->appliedseq < cl->commitseq ) {
    for( s = cl->appliedseq + 1; s <= cl->commitseq; s++ ) {
      raft2_log( LOG_LVL_INFO, "raft2_apply_command seq=%"PRIu64"", s );
      sts = raft2_command_by_seq( cl->clid, s, NULL, glob.buf, sizeof(glob.buf), NULL );
      if( sts ) break; /* command buffer not found */

      app->command( cl, glob.buf, sts );
      cl->appliedseq = s;
      raft2_cluster_set( cl );
    }
  }
  
}


#if 0
static uint64_t raft2_min_storedseq( struct raft2_cluster *cl ) {
  int i;
  uint64_t seq = 0;
  for( i = 0; i < cl->nmember; i++ ) {
    if( cl->member[i].storedseq < seq ) seq = cl->member[i].storedseq;
  }
  return seq;
}
#endif







static void raft2_ping_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t clid, hostid, term, seq;
  struct raft2_cluster *cl;
  struct raft2_member *mp;
  int i, success, sts;
  
  raft2_log( LOG_LVL_TRACE, "raft2_ping_cb %s", res ? "Success" : "Timeout" );
  if( !res ) return;
  
  clid = hcallp->cxt2;
  cl = cl_by_id( clid );
  if( !cl ) {
    raft2_log( LOG_LVL_TRACE, "Unknown cluster" );
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
    raft2_log( LOG_LVL_ERROR, "Unknown member" );
    return;
  }

  sts = xdr_decode_boolean( res, &success );
  if( !sts ) sts = xdr_decode_uint64( res, &term );
  if( !sts ) sts = xdr_decode_uint64( res, &seq );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "Xdr decode error" );
    return;
  }

  raft2_log( LOG_LVL_TRACE, "raft2_vote_cb success=%s term=%"PRIu64" storeseq=%"PRIu64"", success ? "true" : "false", term, seq );
  if( term > cl->term ) {
    raft2_log( LOG_LVL_TRACE, "Remote term higher - convert to follower" );
    cl->voteid = 0;
    raft2_convert_follower( cl, term, hcallp->hostid );
  }
  
  if( !success ) {
    raft2_log( LOG_LVL_TRACE, "ping rejected" );
  }
  
  mp->lastseen = time( NULL );
  mp->storedseq = seq;
  raft2_cluster_set( cl );
  return;  
}

static void raft2_call_ping( struct raft2_cluster *cl, uint64_t hostid ) {
  struct hrauth_call hcall;
  struct xdr_s args;
  char argbuf[256];
  int sts;

  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  /* encode args */
  xdr_encode_uint64( &args, cl->clid );
  xdr_encode_uint64( &args, cl->leaderid ); /* leaderid (must match sender id!) */
  xdr_encode_uint64( &args, cl->term ); /* current term */
  xdr_encode_uint64( &args, cl->commitseq ); /* leaders commit seq */

  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT2_RPC_PROG;
  hcall.vers = RAFT2_RPC_VERS;
  hcall.proc = 1; /* ping */
  hcall.donecb = raft2_ping_cb;
  hcall.cxt2 = cl->clid;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, &args, 1 );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "hrauth_call_async failed" );
    return;
  }

  
}

static void raft2_send_pings( struct raft2_cluster *cl ) {
  int i;
  for( i = 0; i < cl->nmember; i++ ) {
    raft2_call_ping( cl, cl->member[i].hostid );
  }
}

static void raft2_vote_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t clid, hostid, term;
  struct raft2_cluster *cl;
  struct raft2_member *mp;
  int i, success, sts, count;
  
  raft2_log( LOG_LVL_DEBUG, "raft2_vote_cb %s", res ? "Success" : "timeout" );
  if( !res ) return;
  
  clid = hcallp->cxt2;
  cl = cl_by_id( clid );
  if( !cl ) {
    raft2_log( LOG_LVL_TRACE, "Unknown cluster %"PRIx64"", clid );
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
    raft2_log( LOG_LVL_ERROR, "Unknown member" );
    return;
  }

  sts = xdr_decode_boolean( res, &success );
  if( !sts ) sts = xdr_decode_uint64( res, &term );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "Xdr decode error" );
    return;
  }

  if( term > cl->term ) {
    raft2_log( LOG_LVL_TRACE, "Vote returned newer term - convert to follower" );
    raft2_convert_follower( cl, term, hostid );
  } else if( success ) {
    mp->flags |= RAFT2_MEMBER_VOTED;
    count = 1;
    for( i = 0; i < cl->nmember; i++ ) {
      if( cl->member[i].flags & RAFT2_MEMBER_VOTED ) count++;
    }

    raft2_log( LOG_LVL_TRACE, "Vote success count=%u", count );
    if( count >= raft2_cluster_quorum( cl ) ) {
      raft2_log( LOG_LVL_TRACE, "Sufficient votes received - convert to leader" );
      raft2_convert_leader( cl );
    }
  }
  
  mp->lastseen = time( NULL );
  raft2_cluster_set( cl );
  return;  
}

static void raft2_call_vote( struct raft2_cluster *cl, uint64_t hostid ) {
  struct hrauth_call hcall;
  struct xdr_s args;
  char argbuf[256];
  int sts;
  uint64_t lastterm, lastseq;
  
  raft2_command_seq( cl->clid, &lastterm, &lastseq );
  
  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  /* encode args */
  xdr_encode_uint64( &args, cl->clid );
  xdr_encode_uint64( &args, hostreg_localid() ); /* leaderid (must match sender id!) */
  xdr_encode_uint64( &args, cl->term );
  xdr_encode_uint64( &args, lastterm ); /* last command term */
  xdr_encode_uint64( &args, lastseq ); /* last command seq */

  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT2_RPC_PROG;
  hcall.vers = RAFT2_RPC_VERS;
  hcall.proc = 2; /* vote */
  hcall.donecb = raft2_vote_cb;
  hcall.cxt2 = cl->clid;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, &args, 1 );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "hrauth_call_async failed" );
    return;
  }

  
}


static void raft2_putcmd_cb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint64_t clid, hostid, term, seq;
  struct raft2_cluster *cl;
  struct raft2_member *mp;
  int i, success, sts;
  
  raft2_log( LOG_LVL_TRACE, "raft2_putcmd_cb %s", res ? "Success" : "Timeout" );
  if( !res ) return;
  
  clid = hcallp->cxt2;
  cl = cl_by_id( clid );
  if( !cl ) {
    raft2_log( LOG_LVL_TRACE, "Unknown cluster" );
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
    raft2_log( LOG_LVL_ERROR, "Unknown member" );
    return;
  }

  sts = xdr_decode_boolean( res, &success );
  if( !sts ) sts = xdr_decode_uint64( res, &term );
  if( !sts ) sts = xdr_decode_uint64( res, &seq );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "Xdr decode error" );
    return;
  }

  if( success ) {
    mp->storedseq = seq;
  }
  
  mp->lastseen = time( NULL );
  cl->commitseq = raft2_quorum_commitseq( cl );
  raft2_cluster_set( cl );

  /* apply any commands now acked by quorum */
  raft2_apply_commands( cl );

  /* send next command if any */
  raft2_call_putcmd( cl, hostid, 0 );
  
  return;  
}

static void raft2_call_putcmd( struct raft2_cluster *cl, uint64_t hostid, uint64_t cseq ) {
  struct hrauth_call hcall;
  struct xdr_s args[3];
  char argbuf[256];
  int sts, len, i;
  uint64_t cterm;

  if( cseq == 0 ) {
    for( i = 0; i < cl->nmember; i++ ) {
      if( cl->member[i].hostid == hostid ) {
	cseq = cl->member[i].storedseq + 1;
	break;
      }
    }
    if( cseq == 0 ) return;
  }
  
  len = raft2_command_by_seq( cl->clid, cseq, &cterm, glob.buf, sizeof(glob.buf), NULL );
  if( len < 0 ) return;
  
  xdr_init( &args[0], (uint8_t *)argbuf, sizeof(argbuf) );
  /* encode args */
  xdr_encode_uint64( &args[0], cl->clid );
  xdr_encode_uint64( &args[0], cl->term ); /* current term */
  xdr_encode_uint64( &args[0], cterm ); /* command term */
  xdr_encode_uint64( &args[0], cseq ); /* command seq */
  xdr_encode_uint32( &args[0], len );
  xdr_init( &args[1], (uint8_t *)glob.buf, len );
  if( len % 4 ) len += 4 - (len % 4);
  args[1].offset = len;
  
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = RAFT2_RPC_PROG;
  hcall.vers = RAFT2_RPC_VERS;
  hcall.proc = 3; /* putcmd */
  hcall.donecb = raft2_putcmd_cb;
  hcall.cxt2 = cl->clid;
  hcall.timeout = glob.prop.rpc_timeout;
  hcall.service = HRAUTH_SERVICE_PRIV;
  sts = hrauth_call_async( &hcall, args, 2 );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "hrauth_call_async failed" );
    return;
  }

}











static void raft2_convert_follower( struct raft2_cluster *cl, uint64_t term, uint64_t leaderid ) {
  int i;
  
  raft2_log( LOG_LVL_INFO, "Convert to follower term=%"PRIu64" leader=%"PRIx64"", term, leaderid );  
  cl->state = RAFT2_STATE_FOLLOWER;
  cl->term = term;
  cl->timeout = raft2_term_timeout();
  cl->leaderid = leaderid;
  for( i = 0; i < cl->nmember; i++ ) {
    cl->member[i].flags &= ~RAFT2_MEMBER_VOTED;
  }
  raft2_cluster_set( cl );
  raft2_set_iter_timeout();
}

static void raft2_convert_candidate( struct raft2_cluster *cl ) {
  int i;
  raft2_log( LOG_LVL_INFO, "Convert to candidate" );
  cl->state = RAFT2_STATE_CANDIDATE;
  cl->term++;
  cl->timeout = raft2_elec_timeout();
  cl->leaderid = 0;
  cl->voteid = hostreg_localid();  
  for( i = 0; i < cl->nmember; i++ ) {
    cl->member[i].flags &= ~RAFT2_MEMBER_VOTED;
    raft2_call_vote( cl, cl->member[i].hostid );
  }
  raft2_cluster_set( cl );
  raft2_set_iter_timeout();  
}

static void raft2_convert_leader( struct raft2_cluster *cl ) {
  int i;
  
  raft2_log( LOG_LVL_INFO, "Convert to leader" );

  cl->state = RAFT2_STATE_LEADER;
  cl->timeout = rpc_now() + glob.prop.term_low;
  cl->leaderid = hostreg_localid();
  for( i = 0; i < cl->nmember; i++ ) {
    cl->member[i].flags &= ~RAFT2_MEMBER_VOTED;
  }
  
  raft2_send_pings( cl );
  raft2_cluster_set( cl );
  raft2_set_iter_timeout();  
}






static int raft_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

/* 
 * send from leaders to keep their term alive
 */
static int raft_proc_ping( struct rpc_inc *inc ) {
  int handle, sts, success;
  uint64_t clid, hostid, term, commitseq, s, storedseq;
  struct raft2_cluster *clp;
  struct hrauth_context *hc;
  struct xdr_s res;
  char resbuf[16];
  
  /* we can guarantee this because we set a mandatory authenticator */
  hc = (struct hrauth_context *)inc->pcxt;
  success = 0;
  storedseq = 0;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &hostid ); /* leaderid (must match sender id!) */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &term ); /* current term */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &commitseq ); /* leaders commit seq */
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  raft2_log( LOG_LVL_TRACE, "raft2_proc_ping clid=%"PRIx64" leaderid=%"PRIx64" term=%"PRIu64" commitseq=%"PRIu64"",
	     clid, hostid, term, commitseq );
  
  if( hc->remoteid != hostid ) {
    /* sender is different to claimed leader */
    raft2_log( LOG_LVL_WARN, "Remoteid mismatch" );
  }
  
  clp = cl_by_id( clid );
  if( !clp ) goto done;

  if( term < clp->term ) {
    /* term too old, reject */
    raft2_log( LOG_LVL_WARN, "Term old - rejected" );
    term = clp->term;
    goto done;
  }

  if( term > clp->term ) {
    /* term increased, convert to follower */
    raft2_log( LOG_LVL_INFO, "Term incresed - Convert to follower %"PRIx64"", hostid );
    clp->voteid = 0;
    raft2_convert_follower( clp, term, hostid );
  }

  /* checks successful, we now accept the update and apply any changes required */
  clp->leaderid = hostid;
  clp->timeout = raft2_term_timeout();
  clp->state = RAFT2_STATE_FOLLOWER;
  raft2_cluster_set( clp );

  /* leader has incremented commitseq indicating quorum have received this entry */
  if( commitseq > clp->commitseq ) {
    raft2_command_seq( clp->clid, NULL, &s );
    clp->commitseq = commitseq < s ? commitseq : s;
    raft2_cluster_set( clp );
  }

  /* apply any commands commited but not yet applied */
  raft2_apply_commands( clp );

  raft2_command_seq( clid, NULL, &storedseq );
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
  int handle, sts, success;
  uint64_t clid, hostid, term, lastterm, lastseq, seq;
  struct raft2_cluster *clp;
  struct hrauth_context *hc;
  struct xdr_s res;
  char resbuf[16];

  /* we can guarantee this because we set a mandatory authenticator */
  hc = (struct hrauth_context *)inc->pcxt;
  success = 0;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &hostid ); /* leaderid (must match sender id!) */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &term ); /* current term */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &lastterm ); /* last command term */
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &lastseq ); /* last command seq */
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, &handle );

  raft2_log( LOG_LVL_TRACE, "raft2_proc_vote clid=%"PRIx64" leaderid=%"PRIx64" term=%"PRIu64" lastterm=%"PRIu64" lastseq=%"PRIu64"",
	     clid, hostid, term, lastterm, lastseq );
  
  if( hc->remoteid != hostid ) {
    /* sender is different to claimed leader */
    raft2_log( LOG_LVL_WARN, "Remoteid mismatch" );
  }
  
  clp = cl_by_id( clid );
  if( !clp ) goto done;

  if( term < clp->term ) {
    /* term too old, reject */
    raft2_log( LOG_LVL_WARN, "Old term - rejecting" );
    term = clp->term;
    goto done;
  }

  if( term > clp->term ) {
    /* term increased, convert to follower */
    raft2_log( LOG_LVL_INFO, "Term increased - convert to follower" );
    clp->state = RAFT2_STATE_FOLLOWER;
    clp->term = term;
    clp->voteid = 0;    
    raft2_cluster_set( clp );    
  }

  /* grant vote if not voted yet or voted for this host already AND the candidate is at least as up to date as us */
  raft2_command_seq( clp->clid, NULL, &seq );
  if( (clp->voteid == 0 || clp->voteid == hostid) && (lastseq >= seq) ) {
    raft2_log( LOG_LVL_DEBUG, "Granting vote" );
    clp->voteid = hostid;
    success = 1;
  }

  
 done:
  xdr_init( &res, (uint8_t *)resbuf, sizeof(resbuf) );
  xdr_encode_boolean( &res, success );
  xdr_encode_uint64( &res, term );
  return hrauth_reply( inc, &res, 1 );
}

/*
 * sent from leaders to distribute command buffers. 
 */
static int raft_proc_putcmd( struct rpc_inc *inc ) {
  int handle, sts, len;
  char *bufp;
  uint64_t term, cterm, cseq, entryid, clid, bterm;
  struct hrauth_context *hc;
  struct raft2_cluster *cl;
  struct xdr_s res;
  char resbuf[64];
  
  hc = (struct hrauth_context *)inc->pcxt;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &term );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &cterm );  
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &cseq );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &len );
  
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  raft2_log( LOG_LVL_TRACE, "raft_proc_putcmd clid=%"PRIx64" term=%"PRIu64" cterm=%"PRIu64" cseq=%"PRIu64" len=%u",
	     clid, term, cterm, cseq, len );
  
  /* lookup cluster */
  sts = -1;
  cl = cl_by_id( clid );
  if( !sts ) {
    raft2_log( LOG_LVL_ERROR, "Unknown cluster %"PRIx64"", clid );
    term = 0;
    goto done;
  }

  /* check term */
  if( term > cl->term ) {
    raft2_log( LOG_LVL_TRACE, "Term increase - convert to follower" );
    raft2_convert_follower( cl, term, hc->remoteid );
  }

  /* check sender is leader */
  if( cl->leaderid != hc->remoteid ) {
    raft2_log( LOG_LVL_ERROR, "Sender not leader" );
    goto done;
  }
  
  sts = raft2_command_by_seq( clid, cseq, &bterm, NULL, 0, &entryid );
  if( !sts && bterm != term ) {
    /* found this entry already */
    raft2_log( LOG_LVL_WARN, "Found conflicting existing command - deleting entries after %"PRIu64"", cseq );
    log_truncate( clog_by_id( clid ), entryid );
  }
  
  sts = raft2_command_put( clid, cterm, cseq, bufp, len );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "Failed to write command buffer" );
  }
 
 done:
  xdr_init( &res, (uint8_t *)resbuf, sizeof(resbuf) );
  xdr_encode_uint64( &res, sts ? 1 : 0 );
  xdr_encode_uint64( &res, term );
  xdr_encode_uint64( &res, sts ? 0 : cseq );
  return hrauth_reply( inc, &res, 1 );
}

/* 
 * Sent from nodes that come online to update their state 
 */
static int raft_proc_getcmd( struct rpc_inc *inc ) {
  int handle, sts;
  uint64_t term, seq, clid;
  struct hrauth_context *hc;
  struct xdr_s res[2];
  char resbuf[64];
  
  hc = (struct hrauth_context *)inc->pcxt;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &seq );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  raft2_log( LOG_LVL_TRACE, "raft_proc_getcmd clid=%"PRIx64" seq=%"PRIu64"",
	     clid, seq );

  sts = raft2_command_by_seq( clid, seq, &term, glob.buf, sizeof(glob.buf), NULL );

  xdr_init( &res[0], (uint8_t *)resbuf, sizeof(resbuf) );
  xdr_encode_boolean( &res[0], sts ? 0 : 1 );
  if( !sts ) {    
    xdr_encode_uint64( &res[0], term );
    xdr_encode_uint64( &res[0], seq );    
    xdr_encode_uint32( &res[0], sts );
    xdr_init( &res[1], (uint8_t *)glob.buf, sts );
    res[1].offset = sts;
  } else {
    xdr_init( &res[1], NULL, 0 );
  }
  
  return hrauth_reply( inc, res, 2 );
}

/* rpc interface to command api */
static int raft_proc_command( struct rpc_inc *inc ) {
  int handle, sts;
  char *bufp;
  int len;
  uint64_t clid;
  
  sts = xdr_decode_uint64( &inc->xdr, &clid );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &len );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  raft2_log( LOG_LVL_TRACE, "raft_proc_command clid=%"PRIx64" len=%u", clid, len );

  sts = raft2_cluster_command( clid, bufp, len );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, sts ? 0 : 1 );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}


static struct rpc_proc raft_procs[] = {
  { 0, raft_proc_null },
  { 1, raft_proc_ping },
  { 2, raft_proc_vote },  
  { 3, raft_proc_putcmd },
  { 4, raft_proc_getcmd },
  { 5, raft_proc_command },
  { 0, NULL }
};

static struct rpc_version raft_vers = {
  NULL, RAFT2_RPC_VERS, raft_procs
};

static uint32_t raft_prog_auths[] = { RPC_AUTH_HRAUTH, 0 };
static struct rpc_program raft_prog = {
    NULL, RAFT2_RPC_PROG, &raft_vers, raft_prog_auths
};


static void raft2_iter_cb( struct rpc_iterator *iter ) {
  uint64_t now;
  int i, j;
  
  //raft2_log( LOG_LVL_TRACE, "raft2 iterator" );
  raft2_prop( &glob.prop );
  glob.ncl = raft2_cluster_list( glob.cl, RAFT2_MAX_CLUSTER );

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
      case RAFT2_STATE_FOLLOWER:
	/* term timeout - convert to candidate */
	raft2_log( LOG_LVL_TRACE, "Term timeout - convert to candidate" );
	raft2_convert_candidate( &glob.cl[i] );
	break;
      case RAFT2_STATE_CANDIDATE:
	/* election timeout - start new election */
	raft2_log( LOG_LVL_TRACE, "Election timeout - new election" );
	raft2_convert_candidate( &glob.cl[i] );
	break;
      case RAFT2_STATE_LEADER:
	/* send pings to keep term alive */
	raft2_log( LOG_LVL_TRACE, "Send pings to keep term alive" );
	raft2_send_pings( &glob.cl[i] );
	glob.cl[i].timeout = rpc_now() + glob.prop.term_low;
	raft2_cluster_set( &glob.cl[i] );
	break;
      }
    }
  }

  /* set timeout to min cluster timeout */
  raft2_set_iter_timeout();
}


static struct rpc_iterator raft2_iter =
  {
   NULL,
   0,
   1000,
   raft2_iter_cb,
   NULL
  };

static void raft2_set_iter_timeout( void ) {
  int i;
  uint64_t to;
  
  /* set timeout to min cluster timeout */
  to = raft2_iter.timeout;
  for( i = 0; i < glob.ncl; i++ ) {
    if( glob.cl[i].timeout < to ) to = glob.cl[i].timeout;
  }
  raft2_iter.timeout = to;  
}


void raft2_register( void ) {
  int i;
  
  raft2_open();
  glob.ncl = raft2_cluster_list( glob.cl, RAFT2_MAX_CLUSTER );

  /* initiaze by immediately going into follower state */
  for( i = 0; i < glob.ncl; i++ ) {
    glob.cl[i].state = RAFT2_STATE_FOLLOWER;
    glob.cl[i].timeout = 0;
    raft2_cluster_set( &glob.cl[i] );
  }

  rpc_program_register( &raft_prog );
  rpc_iterator_register( &raft2_iter );
}

int raft2_app_register( struct raft2_app *app ) {
  struct raft2_app *ap;
  ap = glob.app;
  while( ap ) {
    if( ap == app ) return -1;
    ap = ap->next;
  }
  app->next = glob.app;
  glob.app = app;
  return 0;    
}

static struct raft2_app *raft2_app_by_appid( uint32_t appid ) {
  struct raft2_app *app;
  app = glob.app;
  while( app ) {
    if( app->appid == appid ) return app;
    app = app->next;
  }
  return NULL;
}
    
  

int raft2_cluster_command( uint64_t clid, char *buf, int len ) {
  struct raft2_cluster *cl;
  int sts, i;
  uint64_t seq;
  
  /* lookup cluster */
  cl = cl_by_id( clid );
  if( !cl ) return -1;

  /* reject if not leader */
  if( cl->state != RAFT2_STATE_LEADER ) return -1;

  /* save buffer locally */
  sts = raft2_command_seq( clid, NULL, &seq );
  if( sts ) return sts;
  
  sts = raft2_command_put( clid, cl->term, seq + 1, buf, len );
  if( sts ) {
    raft2_log( LOG_LVL_ERROR, "Failed to store command buffer" );
    return -1;
  }
  
  /* distribute buffer */
  for( i = 0; i < cl->nmember; i++ ) {
    raft2_call_putcmd( cl, cl->member[i].hostid, 0 );
  }

  return 0;
}

