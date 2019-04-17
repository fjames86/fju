
#include "raft.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <mmf.h>
#include <rpc.h>
#include <hostreg.h>
#include <inttypes.h>
#include <log.h>
#include <sys/stat.h>

#define RAFT_MAX_CLUSTER        16
#define RAFT_MAX_MEMBER         64   /* max members total */

struct raft_header {
    uint32_t magic;
#define RAFT_MAGIC 0x22AE9EF6
    uint32_t version;
    uint64_t seq;

    /* entry counts */
    uint32_t cluster_max;
    uint32_t cluster_count;
    uint32_t member_max;
    uint32_t member_count;

    /* header fields */
};


struct raft_file {
    struct raft_header header;
    struct raft_cluster cluster[RAFT_MAX_CLUSTER];
    struct raft_member member[RAFT_MAX_MEMBER];
};

static struct {
    struct mmf_s mmf;
    int ocount;
    struct raft_file *file;
} glob;

static void raft_lock( void ) {
    mmf_lock( &glob.mmf );
}
static void raft_unlock( void ) {
    mmf_unlock( &glob.mmf );
}

int raft_open( void ) {
    int sts;
    
    if( glob.ocount < 0 ) return -1;
    if( glob.ocount > 0 ) {
        glob.ocount++;
        return 0;
    }

    mkdir( mmf_default_path( "raft", NULL ), 0755 );
    sts = mmf_open( mmf_default_path( "raft", "raft.dat", NULL ), &glob.mmf );
    if( sts ) return sts;
    
    sts = mmf_remap( &glob.mmf, sizeof(*glob.file) );
    if( sts ) goto bad;
    glob.file = (struct raft_file *)glob.mmf.file;
    
    raft_lock();
    if( glob.file->header.magic != RAFT_MAGIC ) {
        glob.file->header.magic = RAFT_MAGIC;
        glob.file->header.version = RAFT_VERSION;
        glob.file->header.seq = 1;
        glob.file->header.cluster_max = RAFT_MAX_CLUSTER;
        glob.file->header.cluster_count = 0;
        glob.file->header.member_max = RAFT_MAX_MEMBER;
        glob.file->header.member_count = 0;
    } else if( glob.file->header.version != RAFT_VERSION ) {
        raft_unlock();
        goto bad;
    }
    raft_unlock();
    
    glob.ocount = 1;
    return 0;
 bad:
    mmf_close( &glob.mmf );
    return -1;
}

int raft_close( void ) {
    if( glob.ocount <= 0 ) return -1;
    glob.ocount--;
    if( glob.ocount > 0 ) return 0;
    mmf_close( &glob.mmf );
    return 0;
}

int raft_reset( void ) {
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    glob.file->header.magic = RAFT_MAGIC;
    glob.file->header.version = RAFT_VERSION;
    glob.file->header.seq = 1;
    glob.file->header.cluster_max = RAFT_MAX_CLUSTER;
    glob.file->header.cluster_count = 0;
    glob.file->header.member_max = RAFT_MAX_MEMBER;
    glob.file->header.member_count = 0;
    raft_unlock();
    return 0;
}

int raft_prop( struct raft_prop *prop ) {
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    prop->version = glob.file->header.version;
    prop->seq = glob.file->header.seq;
    prop->cluster_max = glob.file->header.cluster_max;
    prop->cluster_count = glob.file->header.cluster_count;
    prop->member_max = glob.file->header.member_max;
    prop->member_count = glob.file->header.member_count;
    raft_unlock();
    return 0;
}

/* ------------ cluster commands ----------- */

int raft_cluster_list( struct raft_cluster *clusterlist, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    for( i = 0; i < glob.file->header.cluster_count; i++ ) {
         if( i < n ) {
             clusterlist[i] = glob.file->cluster[i];
         }
    }
    sts = glob.file->header.cluster_count;
    raft_unlock();
    return sts;
}

int raft_cluster_by_id( uint64_t clid, struct raft_cluster *cluster ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.cluster_count; i++ ) {
        if( glob.file->cluster[i].clid == clid ) {
            if( cluster ) *cluster = glob.file->cluster[i];
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}

int raft_cluster_add( struct raft_cluster *cluster ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    if( glob.file->header.cluster_count < glob.file->header.cluster_max ) {
        if( !cluster->clid ) cluster->clid = glob.file->header.seq;
        i = glob.file->header.cluster_count;
        glob.file->cluster[i] = *cluster;
        glob.file->header.cluster_count++;
        glob.file->header.seq++;
        sts = 0;
    }
    raft_unlock();
    return sts;
}

int raft_cluster_rem( uint64_t clid ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = 0;
    /* look for any member of this cluster - if so, forbid removing */
    for( i = 0; i < glob.file->header.member_count; i++ ) {
      if( glob.file->member[i].clid == clid ) {
	sts = -1;
	break;
      }
    }
    if( sts == -1 ) goto done;
    
    sts = -1;
    for( i = 0; i < glob.file->header.cluster_count; i++ ) {
        if( glob.file->cluster[i].clid == clid ) {
            if( i != (glob.file->header.cluster_count - 1) ) glob.file->cluster[i] = glob.file->cluster[glob.file->header.cluster_count - 1];
            glob.file->header.cluster_count--;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    
 done:
    raft_unlock();
    return sts;
}

int raft_cluster_set( struct raft_cluster *cluster ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.cluster_count; i++ ) {
        if( glob.file->cluster[i].clid == cluster->clid ) {
            glob.file->cluster[i] = *cluster;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}

/* ------------ member commands ----------- */

int raft_member_list( struct raft_member *memberlist, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    for( i = 0; i < glob.file->header.member_count; i++ ) {
         if( i < n ) {
             memberlist[i] = glob.file->member[i];
         }
    }
    sts = glob.file->header.member_count;
    raft_unlock();
    return sts;
}

int raft_member_list_by_clid( uint64_t clid, struct raft_member *memberlist, int n ) {
    int sts, i, j;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    j = 0;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( glob.file->member[i].clid == clid ) {
	    if( j < n ) memberlist[j] = glob.file->member[i];
	    j++;
        }
    }
    sts = j;
    raft_unlock();
    return sts;
}

int raft_member_list_by_hostid( uint64_t hostid, struct raft_member *memberlist, int n ) {
    int sts, i, j;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    j = 0;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( glob.file->member[i].hostid == hostid ) {
	    if( j < n ) memberlist[j] = glob.file->member[i];
	    j++;
        }
    }
    sts = j;
    raft_unlock();
    return sts;
}

int raft_member_by_id( uint64_t memberid, struct raft_member *member ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( glob.file->member[i].memberid == memberid ) {
            if( member ) *member = glob.file->member[i];
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}

int raft_member_add( struct raft_member *member ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.cluster_count; i++ ) {
      if( glob.file->cluster[i].clid == member->clid ) {
	sts = 0;
	break;
      }
    }
    if( sts == -1 ) goto done;
    sts = -1;	  
    if( glob.file->header.member_count < glob.file->header.member_max ) {
        if( !member->memberid ) member->memberid = glob.file->header.seq;
        i = glob.file->header.member_count;
        glob.file->member[i] = *member;
        glob.file->header.member_count++;
        glob.file->header.seq++;
        sts = 0;
    }
 done:
    raft_unlock();
    return sts;
}

int raft_member_rem( uint64_t memberid ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( glob.file->member[i].memberid == memberid ) {
            if( i != (glob.file->header.member_count - 1) ) glob.file->member[i] = glob.file->member[glob.file->header.member_count - 1];
            glob.file->header.member_count--;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}

int raft_member_set( struct raft_member *member ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( glob.file->member[i].memberid == member->memberid ) {
            glob.file->member[i] = *member;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}

int raft_member_set_nextping( uint64_t memberid, uint64_t nextping ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( glob.file->member[i].memberid == memberid ) {
    	    glob.file->member[i].nextping = nextping;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}

uint64_t raft_member_by_hostid( uint64_t clid, uint64_t hostid ) {
    int sts, i;
    uint64_t ret = 0;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( (glob.file->member[i].clid == clid) && (glob.file->member[i].hostid == hostid) ) {
  	    ret = glob.file->member[i].memberid;
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return ret;
}

int raft_member_set_state( uint64_t memberid, uint32_t state ) {
  int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( glob.file->member[i].memberid == memberid ) {
	    glob.file->member[i].flags = (glob.file->member[i].flags & RAFT_MEMBER_STATEMASK) | (state & RAFT_MEMBER_STATEMASK);
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}

/* ------------------------------------------------------ */

/* RPC component */
struct raft_cl {
  struct raft_cl *next;

  uint64_t clid;
  //  struct raft_cluster cluster;  
  //  int nmember;
  //  struct raft_member member[RAFT_MAX_CLUSTER_MEMBER];  
  struct log_s log;
};

static struct {
  int ncluster;
  struct raft_cl cluster[RAFT_MAX_CLUSTER];  
} raftg;


/* we start with a very basic rpc interface that lists etc */

static int raft_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int raft_encode_cluster( struct xdr_s *xdr, struct raft_cluster *x ) {
  int sts;
  sts = xdr_encode_uint64( xdr, x->clid );
  if( sts ) return sts;
  sts = xdr_encode_uint64( xdr, x->currentterm );
  if( sts ) return sts;
  sts = xdr_encode_uint64( xdr, x->votedfor );
  if( sts ) return sts;
  sts = xdr_encode_uint64( xdr, x->leaderid );
  if( sts ) return sts;
  return 0;
}
static int raft_decode_cluster( struct xdr_s *xdr, struct raft_cluster *x ) {
  int sts;
  sts = xdr_decode_uint64( xdr, &x->clid );
  if( sts ) return sts;
  sts = xdr_decode_uint64( xdr, &x->currentterm );
  if( sts ) return sts;
  sts = xdr_decode_uint64( xdr, &x->votedfor );
  if( sts ) return sts;
  sts = xdr_decode_uint64( xdr, &x->leaderid );
  if( sts ) return sts;
  return 0;
}
static int raft_encode_member( struct xdr_s *xdr, struct raft_member *x ) {
  int sts;
  sts = xdr_encode_uint64( xdr, x->memberid );
  if( sts ) return sts;
  sts = xdr_encode_uint64( xdr, x->clid );
  if( sts ) return sts;
  sts = xdr_encode_uint64( xdr, x->hostid );
  if( sts ) return sts;
  sts = xdr_encode_uint64( xdr, x->lastseen );
  if( sts ) return sts;
  sts = xdr_encode_uint64( xdr, x->nextping );
  if( sts ) return sts;
  sts = xdr_encode_uint64( xdr, x->nextidx );
  if( sts ) return sts;
  sts = xdr_encode_uint64( xdr, x->matchidx );
  if( sts ) return sts;
  sts = xdr_encode_uint32( xdr, x->flags );
  if( sts ) return sts;
  return 0;  
}
static int raft_decode_member( struct xdr_s *xdr, struct raft_member *x ) {
  int sts;
  sts = xdr_decode_uint64( xdr, &x->memberid );
  if( sts ) return sts;
  sts = xdr_decode_uint64( xdr, &x->clid );
  if( sts ) return sts;
  sts = xdr_decode_uint64( xdr, &x->hostid );
  if( sts ) return sts;
  sts = xdr_decode_uint64( xdr, &x->lastseen );
  if( sts ) return sts;
  sts = xdr_decode_uint64( xdr, &x->nextping );
  if( sts ) return sts;
  sts = xdr_decode_uint64( xdr, &x->nextidx );
  if( sts ) return sts;
  sts = xdr_decode_uint64( xdr, &x->matchidx );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &x->flags );
  if( sts ) return sts;
  return 0;  
}

/*
 * Sent by leader to replicate log entries.
 */
static int raft_proc_append( struct rpc_inc *inc ) {
  int handle;
  /* arguments */
  uint64_t clid;
  uint64_t leaderterm;
  uint64_t leaderid;
  uint64_t prevlogidx;
  uint64_t prevlogterm;
  struct log_iov entries[32];
  int nentries;
  uint64_t commitidx; 

  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

/*
 * Sent by candidates to gather votes 
 */
static int raft_proc_vote( struct rpc_inc *inc ) {
  int handle;
  uint64_t term;
  uint64_t candidateid;
  uint64_t lastlogidx;
  uint64_t lastlogterm;


  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}




/*
 * List info: clusters, members etc
 */
static int raft_proc_list( struct rpc_inc *inc ) {
  int handle;
  int i, n;
  struct raft_cluster cluster[RAFT_MAX_CLUSTER];
  struct raft_member member[RAFT_MAX_MEMBER];
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  n = raft_cluster_list( cluster, RAFT_MAX_CLUSTER );
  xdr_encode_uint32( &inc->xdr, n );
  for( i = 0; i < n; i++ ) {
    raft_encode_cluster( &inc->xdr, &cluster[i] );
  }
  n = raft_member_list( member, RAFT_MAX_MEMBER );
  xdr_encode_uint32( &inc->xdr, n );
  for( i = 0; i < n; i++ ) {
    raft_encode_member( &inc->xdr, &member[i] );
  }
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct rpc_proc raft_procs[] = {
  { 0, raft_proc_null },
  { 1, raft_proc_append },
  { 2, raft_proc_vote },
  { 3, raft_proc_list },
  { 0, NULL }
};

static struct rpc_version raft_vers = {
  NULL, RAFT_RPC_VERSION, raft_procs
};

static struct rpc_program raft_prog = {
  NULL, RAFT_RPC_PROGRAM, &raft_vers
};


/* we also need an iterator to periodically contact other hosts */
static void raft_iter_cb( struct rpc_iterator *iter ) {
  int i, j, k, ncl, nm, sts;
  struct raft_cluster cluster[RAFT_MAX_CLUSTER];
  struct raft_member member[RAFT_MAX_CLUSTER_MEMBER];
  struct hostreg_host local, host;
  int found;
  uint64_t now, memberid;
  
  //  rpc_log( RPC_LOG_DEBUG, "raft iter" );

  /* we need to send some calls out and await the replies */
  hostreg_host_local( &local );
  
  /* list all clusters */
  ncl = raft_cluster_list( cluster, RAFT_MAX_CLUSTER );
  //  rpc_log( LOG_LVL_TRACE, "ncl = %d", ncl );
  
  /* start by loading all defined clusters, or unloading removed clusters */
  for( i = 0; i < ncl; i++ ) {
    rpc_log( LOG_LVL_DEBUG, "cluster[%d] = %"PRIx64"", i, cluster[i].clid );
    
    found = 0;
    for( j = 0; j < raftg.ncluster; j++ ) {
      if( raftg.cluster[j].clid == cluster[i].clid ) {
	found = 1;
	break;
      }
    }
    if( !found ) {
      char fname[64];
      struct log_opts opts;
      
      /* newly defined cluster - add to glob */
      rpc_log( LOG_LVL_DEBUG, "Adding new cluster %"PRIx64"", cluster[i].clid );
      if( raftg.ncluster == RAFT_MAX_CLUSTER ) continue;
      j = raftg.ncluster;
      
      raftg.cluster[j].clid = cluster[i].clid;
      memset( &opts, 0, sizeof(opts) );
      opts.mask = LOG_OPT_FLAGS|LOG_OPT_LBACOUNT;
      opts.flags = LOG_FLAG_FIXED|LOG_FLAG_GROW;
      sprintf( fname, "%"PRIx64".log", cluster[i].clid );
      sts = log_open( mmf_default_path( "raft", fname, NULL ), &opts, &raftg.cluster[j].log );
      if( sts ) {
	rpc_log( LOG_LVL_ERROR, "Failed to open log" );
	continue;
      }
      raftg.ncluster++;

      /* set initial state to follower */
      memberid = raft_member_by_hostid( cluster[i].clid, local.id );
      if( memberid ) {
	raft_member_set_state( memberid, RAFT_MEMBER_FOLLOWER );
	raft_member_set_nextping( memberid, 0 );
      }
      
    }
  }
  for( j = 0; j < raftg.ncluster; j++ ) {
    found = 0;
    for( i = 0; i < ncl; i++ ) {
      if( raftg.cluster[j].clid == cluster[i].clid ) {
	found = 1;
	break;
      }
    }
    if( !found ) {
      log_close( &raftg.cluster[j].log );
      if( j != raftg.ncluster - 1 ) raftg.cluster[j] = raftg.cluster[raftg.ncluster - 1];
      raftg.ncluster--;
    }
  }


  now = rpc_now();
  for( i = 0; i < ncl; i++ ) {
    /* for each cluster, list all members */
    nm = raft_member_list_by_clid( cluster[i].clid, member, RAFT_MAX_CLUSTER_MEMBER );
    for( j = 0; j < nm; j++ ) {
      /* for each member, if it is not local, send a ping and await reply */
      if( member[j].hostid == local.id ) continue;

      /* do nothing until next ping time expires */
      if( member[j].nextping > now ) continue;
      raft_member_set_nextping( member[j].memberid, now + 2000 );
      
      /* each host has many interfaces... */
      sts = hostreg_host_by_id( member[j].hostid, &host );
      if( sts ) continue;
      
      for( k = 0; k < host.naddr; k++ ) {
	/* TODO: send message on this address */
	rpc_log( RPC_LOG_DEBUG, "TODO: send message host=%"PRIx64" addr=%d.%d.%d.%d",
		 host.id,
		 (host.addr[k]) & 0xff,
		 (host.addr[k] >> 8) & 0xff,
		 (host.addr[k] >> 16) & 0xff,
		 (host.addr[k] >> 24) & 0xff );
		 
      }
      
    }
  }

  
}

static struct rpc_iterator raft_iter = {
    NULL,
    0,
    1000,
    raft_iter_cb,
    NULL
};
					

int raft_register( void ) {
  int sts;

  rpc_log( LOG_LVL_DEBUG, "raft_register" );
  
  sts = raft_open();
  if( sts ) rpc_log( LOG_LVL_ERROR, "raft_open failed" );
  
  rpc_program_register( &raft_prog );
  rpc_iterator_register( &raft_iter );
  return 0;
}




