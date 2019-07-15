
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "nls.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <mmf.h>
#include <sec.h>
#include <rpc.h>
#include <rpcd.h>
#include <sys/stat.h>
#include <hostreg.h>

#include "nls-private.h"

#ifdef WIN32
#define PRIu64 "llu"
#endif

struct nls_header {
    uint32_t magic;
#define NLS_MAGIC 0x22AE9EF6
    uint32_t version;
    uint64_t seq;

    /* entry counts */
    uint32_t share_max;
    uint32_t share_count;
    uint32_t remote_max;
    uint32_t remote_count;
    uint32_t notify_max;
    uint32_t notify_count;
  
    /* header fields */
    uint32_t poll_timeout;
    uint32_t rpc_timeout;
};


struct nls_file {
    struct nls_header header;
    struct nls_share share[NLS_MAX_SHARE];
    struct nls_remote remote[NLS_MAX_REMOTE];
    struct nls_notify notify[NLS_MAX_NOTIFY];
};

static struct {
    struct mmf_s mmf;
    int ocount;
    struct nls_file *file;
} glob;

static void nls_lock( void ) {
    mmf_lock( &glob.mmf );
}
static void nls_unlock( void ) {
    mmf_unlock( &glob.mmf );
}

int nls_open( void ) {
    int sts;
    
    if( glob.ocount < 0 ) return -1;
    if( glob.ocount > 0 ) {
        glob.ocount++;
        return 0;
    }

    mmf_ensure_dir( mmf_default_path( "nls", NULL ) );
    sts = mmf_open( mmf_default_path( "nls", "nls.dat", NULL ), &glob.mmf );
    if( sts ) return sts;
    
    sts = mmf_remap( &glob.mmf, sizeof(*glob.file) );
    if( sts ) goto bad;
    glob.file = (struct nls_file *)glob.mmf.file;
    
    nls_lock();
    if( glob.file->header.magic != NLS_MAGIC ) {
        glob.file->header.magic = NLS_MAGIC;
        glob.file->header.version = NLS_VERSION;
        glob.file->header.seq = 1;
        glob.file->header.share_max = NLS_MAX_SHARE;
        glob.file->header.share_count = 0;
	glob.file->header.remote_max = NLS_MAX_REMOTE;
	glob.file->header.remote_count = 0;
	glob.file->header.notify_max = NLS_MAX_NOTIFY;
	glob.file->header.notify_count = 0;
	glob.file->header.rpc_timeout = NLS_RPC_TIMEOUT;
	glob.file->header.poll_timeout = NLS_POLL_TIMEOUT;
    } else if( glob.file->header.version != NLS_VERSION ) {
        nls_unlock();
        goto bad;
    }
    nls_unlock();
    
    glob.ocount = 1;
    return 0;
 bad:
    mmf_close( &glob.mmf );
    return -1;
}

int nls_close( void ) {
    if( glob.ocount <= 0 ) return -1;
    glob.ocount--;
    if( glob.ocount > 0 ) return 0;
    mmf_close( &glob.mmf );
    return 0;
}

int nls_reset( void ) {
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    glob.file->header.magic = NLS_MAGIC;
    glob.file->header.version = NLS_VERSION;
    glob.file->header.seq = 1;
    glob.file->header.share_max = NLS_MAX_SHARE;
    glob.file->header.share_count = 0;
    glob.file->header.remote_max = NLS_MAX_REMOTE;
    glob.file->header.remote_count = 0;
    glob.file->header.notify_max = NLS_MAX_NOTIFY;
    glob.file->header.notify_count = 0;    
    nls_unlock();
    return 0;
}

int nls_prop( struct nls_prop *prop ) {
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    prop->version = glob.file->header.version;
    prop->seq = glob.file->header.seq;
    prop->share_max = glob.file->header.share_max;
    prop->share_count = glob.file->header.share_count;
    prop->remote_max = glob.file->header.remote_max;
    prop->remote_count = glob.file->header.remote_count;
    prop->notify_max = glob.file->header.notify_max;
    prop->notify_count = glob.file->header.notify_count;
    prop->rpc_timeout = glob.file->header.rpc_timeout;
    prop->poll_timeout = glob.file->header.poll_timeout;
    nls_unlock();
    return 0;
}

int nls_set_rpc_timeout( uint32_t timeout ) {
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    glob.file->header.rpc_timeout = timeout;
    nls_unlock();
    return 0;  
}
int nls_set_poll_timeout( uint32_t timeout ) {
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    glob.file->header.poll_timeout = timeout;
    nls_unlock();
    return 0;    
}

/* ------------ share commands ----------- */

int nls_share_list( struct nls_share *sharelist, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    for( i = 0; i < glob.file->header.share_count; i++ ) {
         if( i < n ) {
             sharelist[i] = glob.file->share[i];
         }
    }
    sts = glob.file->header.share_count;
    nls_unlock();
    return sts;
}

int nls_share_by_hshare( uint64_t hshare, struct nls_share *share ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.share_count; i++ ) {
        if( glob.file->share[i].hshare == hshare ) {
            if( share ) *share = glob.file->share[i];
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_share_add( struct nls_share *share ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    if( glob.file->header.share_count < glob.file->header.share_max ) {
	if( !share->hshare ) sec_rand( &share->hshare, sizeof(share->hshare) );
        i = glob.file->header.share_count;
        glob.file->share[i] = *share;
        glob.file->header.share_count++;
        glob.file->header.seq++;
        sts = 0;
    }
    nls_unlock();
    return sts;
}

int nls_share_rem( uint64_t hshare ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.share_count; i++ ) {
        if( glob.file->share[i].hshare == hshare ) {
            if( i != (glob.file->header.share_count - 1) ) glob.file->share[i] = glob.file->share[glob.file->header.share_count - 1];
            glob.file->header.share_count--;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_share_set( struct nls_share *share ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.share_count; i++ ) {
        if( glob.file->share[i].hshare == share->hshare ) {
            glob.file->share[i] = *share;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_share_open( struct nls_share *share, struct log_s *log ) {
  char name[256];
  sprintf( name, "%s.log", share->name );
  return log_open( mmf_default_path( "nls", name, NULL ), NULL, log );
}


/* ------------ remote commands ----------- */

int nls_remote_list( struct nls_remote *rlist, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    for( i = 0; i < glob.file->header.remote_count; i++ ) {
         if( i < n ) {
             rlist[i] = glob.file->remote[i];
         }
    }
    sts = glob.file->header.remote_count;
    nls_unlock();
    return sts;
}

int nls_remote_by_hshare( uint64_t hshare, struct nls_remote *remote ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.remote_count; i++ ) {
        if( glob.file->remote[i].hshare == hshare ) {
            if( remote ) *remote = glob.file->remote[i];
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_remote_add( struct nls_remote *remote ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    if( !remote->hshare ) return -1;
    
    nls_lock();
    sts = -1;
    if( glob.file->header.remote_count < glob.file->header.remote_max ) {
        i = glob.file->header.remote_count;
        glob.file->remote[i] = *remote;
        glob.file->header.remote_count++;
        glob.file->header.seq++;
        sts = 0;
    }
    nls_unlock();
    return sts;
}

int nls_remote_rem( uint64_t hshare ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.remote_count; i++ ) {
        if( glob.file->remote[i].hshare == hshare ) {
            if( i != (glob.file->header.remote_count - 1) ) glob.file->remote[i] = glob.file->remote[glob.file->header.remote_count - 1];
            glob.file->header.remote_count--;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_remote_set( struct nls_remote *remote ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.remote_count; i++ ) {
        if( glob.file->remote[i].hshare == remote->hshare ) {
            glob.file->remote[i] = *remote;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_remote_open( struct nls_remote *remote, struct log_s *log ) {
  char name[256], hostid[64];
  sprintf( hostid, "%"PRIu64"", remote->hostid );
  sprintf( name, "%s.log", remote->name );
  mmf_ensure_dir( mmf_default_path( "nls", hostid, NULL ) );
  return log_open( mmf_default_path( "nls", hostid, name, NULL ), NULL, log );
}



/* ------------ notify commands ----------- */

int nls_notify_list( struct nls_notify *rlist, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    for( i = 0; i < glob.file->header.notify_count; i++ ) {
         if( i < n ) {
             rlist[i] = glob.file->notify[i];
         }
    }
    sts = glob.file->header.notify_count;
    nls_unlock();
    return sts;
}

int nls_notify_by_hshare( uint64_t hostid, uint64_t hshare, struct nls_notify *notify ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.notify_count; i++ ) {
        if( glob.file->notify[i].hshare == hshare && glob.file->notify[i].hostid == hostid ) {
            if( notify ) *notify = glob.file->notify[i];
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}


int nls_notify_by_tag( uint64_t tag, struct nls_notify *notify ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.notify_count; i++ ) {
        if( glob.file->notify[i].tag == tag ) {
            if( notify ) *notify = glob.file->notify[i];
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_notify_add( struct nls_notify *entry ) {
  return nls_notify_set( entry );
}

int nls_notify_rem( uint64_t tag ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    nls_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.notify_count; i++ ) {
      if( glob.file->notify[i].tag == tag ) {
            if( i != (glob.file->header.notify_count - 1) ) glob.file->notify[i] = glob.file->notify[glob.file->header.notify_count - 1];
            glob.file->header.notify_count--;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    nls_unlock();
    return sts;
}

int nls_notify_set( struct nls_notify *notify ) {
    int sts, i, oldest;
    uint64_t age;
    
    if( glob.ocount <= 0 ) return -1;
    nls_lock();

    sts = -1;
    
    /* update existing */
    for( i = 0; i < glob.file->header.notify_count; i++ ) {
      if( glob.file->notify[i].hostid == notify->hostid &&
	  glob.file->notify[i].hshare == notify->hshare ) {
	glob.file->notify[i] = *notify;
	glob.file->header.seq++;
	sts = 0;
	goto done;
      }
    }

    /* add new */
    if( glob.file->header.notify_count == glob.file->header.notify_max ) {
      /* if out of space evict oldest */
      oldest = 0;
      age = 0;
      for( i = 0; i < glob.file->header.notify_count; i++ ) {
	if( oldest == 0 || glob.file->notify[i].timestamp < age ) {
	  age = glob.file->notify[i].timestamp;
	  oldest = i;
	}
      }
      if( oldest != glob.file->header.notify_count - 1 ) glob.file->notify[oldest] = glob.file->notify[glob.file->header.notify_count - 1];
      glob.file->header.notify_count--;
      glob.file->header.seq++;
    }

    if( glob.file->header.notify_count < glob.file->header.notify_max ) {
        i = glob.file->header.notify_count;
	notify->tag = glob.file->header.seq;
	glob.file->notify[i] = *notify;
	glob.file->header.notify_count++;
	glob.file->header.seq++;
	sts = 0;
    }
    
 done:
    nls_unlock();
    return sts;
}


