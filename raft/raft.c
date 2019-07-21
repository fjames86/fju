
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <mmf.h>
#include <rpc.h>
#include <hostreg.h>
#include <inttypes.h>
#include <log.h>
#include <sec.h>

#include "raft.h"

#define RAFT_ELEC_LOW 1000
#define RAFT_ELEC_HIGH 1000
#define RAFT_TERM_LOW 1000
#define RAFT_TERM_HIGH 1000

#define RAFT_MAX_CLUSTER 32
#define RAFT_MAX_MEMBER 256

struct raft_header {
    uint32_t magic;
#define RAFT_MAGIC 0x22AE9EF7
    uint32_t version;
    uint64_t seq;

    /* entry counts */
    uint32_t cluster_max;
    uint32_t cluster_count;
    uint32_t member_max;
    uint32_t member_count;

    /* header fields */
    uint32_t elec_low;
    uint32_t elec_high;
    uint32_t term_low;
    uint32_t term_high;

    uint32_t spare[32];
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

    mmf_ensure_dir( mmf_default_path( "raft", NULL ) );
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
	glob.file->header.elec_low = RAFT_ELEC_LOW;
	glob.file->header.elec_high = RAFT_ELEC_HIGH;
	glob.file->header.term_low = RAFT_TERM_LOW;
	glob.file->header.term_high = RAFT_TERM_HIGH;	
    } else if( glob.file->header.version != RAFT_VERSION ) {
        raft_unlock();
        goto bad;
    }
    raft_unlock();

    hostreg_open();
    
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
    hostreg_close();
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
    prop->elec_low = glob.file->header.elec_low;
    prop->elec_high = glob.file->header.elec_high;
    prop->term_low = glob.file->header.term_low;
    prop->term_high = glob.file->header.term_high;
    raft_unlock();
    return 0;
}

int raft_set_timeouts( uint32_t elec_low, uint32_t elec_high, uint32_t term_low, uint32_t term_high ) {
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    if( elec_low ) glob.file->header.elec_low = elec_low;
    if( elec_high ) glob.file->header.elec_high = elec_high;
    if( term_low ) glob.file->header.term_low = term_low;
    if( term_high ) glob.file->header.term_high = term_high;
    raft_unlock();
    return 0;
}


int raft_cluster_list( struct raft_cluster *cluster, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    for( i = 0; i < glob.file->header.cluster_count; i++ ) {
         if( i < n ) {
             cluster[i] = glob.file->cluster[i];
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
        if( glob.file->cluster[i].id == clid ) {
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

    if( !cluster->id ) sec_rand( &cluster->id, sizeof(cluster->id) );
    cluster->seq = 0;
    cluster->voteid = 0;
    cluster->flags = RAFT_CLUSTER_OFFLINE;
    cluster->timeout = 0;
    
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    if( (glob.file->header.cluster_count >= glob.file->header.cluster_max) ||
	(glob.file->header.member_count >= glob.file->header.member_max) ) {
	sts = -1;
	goto done;
    }
    
    i = glob.file->header.cluster_count;
    glob.file->cluster[i] = *cluster;
    glob.file->header.cluster_count++;

    /* add local entry */
    i = glob.file->header.member_count;
    sec_rand( &glob.file->member[i].id, sizeof(uint64_t) );
    glob.file->member[i].clid = cluster->id;
    glob.file->member[i].hostid = hostreg_localid();
    glob.file->member[i].lastseen = 0;
    glob.file->member[i].flags = RAFT_STATE_FOLLOWER|RAFT_MEMBER_LOCAL;
    glob.file->header.member_count++;
    
    glob.file->header.seq++;
    sts = 0;

done:
    raft_unlock();
    return sts;
}

int raft_cluster_rem( uint64_t clid ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    
    raft_lock();
    sts = 0;
    /* look for any non-local member of this cluster - if so, forbid removing */
    for( i = 0; i < glob.file->header.member_count; i++ ) {
	if( (glob.file->member[i].clid == clid) &&
	    !(glob.file->member[i].flags & RAFT_MEMBER_LOCAL) ) {
	sts = -1;
	break;
      }
    }
    if( sts == -1 ) goto done;

    i = 0;
    while( i < glob.file->header.member_count ) {
	if( glob.file->member[i].clid == clid ) {
	    if( i != (glob.file->header.member_count - 1) ) glob.file->member[i] = glob.file->member[glob.file->header.member_count - 1];
	    glob.file->header.member_count--;
	    glob.file->header.seq++;
	} else {
	    i++;
	}
    }
    
    sts = -1;
    for( i = 0; i < glob.file->header.cluster_count; i++ ) {
        if( glob.file->cluster[i].id == clid ) {
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
        if( glob.file->cluster[i].id == cluster->id ) {
            glob.file->cluster[i] = *cluster;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}


int raft_cluster_online( uint64_t clid, int online ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.cluster_count; i++ ) {
        if( glob.file->cluster[i].id == clid ) {
	    sts = glob.file->cluster[i].flags & RAFT_CLUSTER_OFFLINE ? 0 : 1;

	    if( online ) glob.file->cluster[i].flags &= ~RAFT_CLUSTER_OFFLINE;
	    else glob.file->cluster[i].flags |= RAFT_CLUSTER_OFFLINE;

	    glob.file->header.seq++;
            break;
        }
    }
    raft_unlock();
    return sts;
}

int raft_member_list( uint64_t clid, struct raft_member *member, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = 0;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
	if( (clid == 0) || (glob.file->member[i].clid == clid) ) {
	    if( sts < n ) {
		member[sts] = glob.file->member[i];
	    }
	    sts++;
	}
    }
    raft_unlock();
    return sts;
}

int raft_member_by_id( uint64_t id, struct raft_member *member ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( glob.file->member[i].id == id ) {
            if( member ) *member = glob.file->member[i];
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}

int raft_member_local( uint64_t clid, struct raft_member *member ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( (glob.file->member[i].clid == clid) &&
	    (glob.file->member[i].flags & RAFT_MEMBER_LOCAL) ) {
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

    if( !member->id ) sec_rand( &member->id, sizeof(uint64_t) );
    member->lastseen = 0;
    member->flags &= ~RAFT_STATE_MASK;
    
    /* check cluster exists */
    sts = -1;
    for( i = 0; i < glob.file->header.cluster_count; i++ ) {
      if( glob.file->cluster[i].id == member->clid ) {
	sts = 0;
	break;
      }
    }
    if( sts == -1 ) goto done;

    sts = -1;	  
    if( glob.file->header.member_count < glob.file->header.member_max ) {
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

int raft_member_rem( uint64_t id ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    raft_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.member_count; i++ ) {
        if( glob.file->member[i].id == id ) {
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
        if( glob.file->member[i].id == member->id ) {
            glob.file->member[i] = *member;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    raft_unlock();
    return sts;
}

