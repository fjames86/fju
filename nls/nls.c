
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
  
    /* header fields */
};


struct nls_file {
    struct nls_header header;
    struct nls_share share[NLS_MAX_SHARE];
    struct nls_remote remote[NLS_MAX_REMOTE];
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
        if( glob.file->remote[i].share.hshare == hshare ) {
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
    if( !remote->share.hshare ) return -1;
    
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
        if( glob.file->remote[i].share.hshare == hshare ) {
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
        if( glob.file->remote[i].share.hshare == remote->share.hshare ) {
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
  sprintf( name, "%s.log", remote->share.name );
  mmf_ensure_dir( mmf_default_path( "nls", hostid, NULL ) );
  return log_open( mmf_default_path( "nls", hostid, name, NULL ), NULL, log );
}



