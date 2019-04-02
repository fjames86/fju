
#include "hostreg.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <mmf.h>
#include <sec.h>

#define HOSTREG_MAX_HOST 32

struct hostreg_header {
    uint32_t magic;
#define HOSTREG_MAGIC 0xD091BB5C
    uint32_t version;
    uint64_t seq;

    /* entry counts */
    uint32_t host_max;
    uint32_t host_count;

    /* header fields */
    uint64_t localid;
    uint32_t publen;
    uint8_t pubkey[64];
    uint32_t privlen;
    uint8_t privkey[96];
};


struct hostreg_file {
    struct hostreg_header header;
    struct hostreg_host host[HOSTREG_MAX_HOST];
};

static struct {
    struct mmf_s mmf;
    int ocount;
    struct hostreg_file *file;
} glob;

static void hostreg_lock( void ) {
    mmf_lock( &glob.mmf );
}
static void hostreg_unlock( void ) {
    mmf_unlock( &glob.mmf );
}

int hostreg_open( void ) {
    int sts;
    struct sec_buf priv, pub;
    
    if( glob.ocount < 0 ) return -1;
    if( glob.ocount > 0 ) {
        glob.ocount++;
        return 0;
    }
    
    sts = mmf_open( "hostreg.dat", &glob.mmf );
    if( sts ) return sts;
    
    sts = mmf_remap( &glob.mmf, sizeof(*glob.file) );
    if( sts ) goto bad;
    glob.file = (struct hostreg_file *)glob.mmf.file;
    
    hostreg_lock();
    if( glob.file->header.magic != HOSTREG_MAGIC ) {
        glob.file->header.magic = HOSTREG_MAGIC;
        glob.file->header.version = HOSTREG_VERSION;
        glob.file->header.seq = 1;
        glob.file->header.host_max = HOSTREG_MAX_HOST;
        glob.file->header.host_count = 0;

//	sec_rand( &glob.file->header.localid, sizeof(uint64_t) );
	priv.buf = glob.file->header.privkey;
	priv.len = sizeof(glob.file->header.privkey);
	pub.buf = glob.file->header.pubkey;
	pub.len = sizeof(glob.file->header.pubkey);
//	ecdh_generate( &priv, &pub );
	glob.file->header.privlen = priv.len;
	glob.file->header.publen = pub.len;
    } else if( glob.file->header.version != HOSTREG_VERSION ) {
        hostreg_unlock();
        goto bad;
    }
    hostreg_unlock();
    
    glob.ocount = 1;
    return 0;
 bad:
    mmf_close( &glob.mmf );
    return -1;
}

int hostreg_close( void ) {
    if( glob.ocount <= 0 ) return -1;
    glob.ocount--;
    if( glob.ocount > 0 ) return 0;
    mmf_close( &glob.mmf );
    return 0;
}

int hostreg_reset( void ) {
    if( glob.ocount <= 0 ) return -1;
    hostreg_lock();
    glob.file->header.magic = HOSTREG_MAGIC;
    glob.file->header.version = HOSTREG_VERSION;
    glob.file->header.seq = 1;
    glob.file->header.host_max = HOSTREG_MAX_HOST;
    glob.file->header.host_count = 0;
    hostreg_unlock();
    return 0;
}

int hostreg_prop( struct hostreg_prop *prop ) {
    if( glob.ocount <= 0 ) return -1;
    hostreg_lock();
    prop->version = glob.file->header.version;
    prop->seq = glob.file->header.seq;
    prop->host_max = glob.file->header.host_max;
    prop->host_count = glob.file->header.host_count;
    prop->localid = glob.file->header.localid;
    prop->publen = glob.file->header.publen;
    memcpy( prop->pubkey, glob.file->header.pubkey, sizeof(glob.file->header.pubkey[0]) * 64 );
    prop->privlen = glob.file->header.privlen;
    memcpy( prop->privkey, glob.file->header.privkey, sizeof(glob.file->header.privkey[0]) * 96 );
    hostreg_unlock();
    return 0;
}

/* ------------ host commands ----------- */

int hostreg_host_list( struct hostreg_host *hostlist, int n ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    hostreg_lock();
    for( i = 0; i < glob.file->header.host_count; i++ ) {
         if( i < n ) {
             hostlist[i] = glob.file->host[i];
         }
    }
    sts = glob.file->header.host_count;
    hostreg_unlock();
    return sts;
}

int hostreg_host_by_id( uint64_t id, struct hostreg_host *host ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    hostreg_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.host_count; i++ ) {
        if( glob.file->host[i].id == id ) {
            if( host ) *host = glob.file->host[i];
            sts = 0;
            break;
        }
    }
    hostreg_unlock();
    return sts;
}

int hostreg_host_by_name( char *name, struct hostreg_host *host ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    hostreg_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.host_count; i++ ) {
        if( strcmp( glob.file->host[i].name, name ) == 0 ) {
            if( host ) *host = glob.file->host[i];
            sts = 0;
            break;
        }
    }
    hostreg_unlock();
    return sts;
}

int hostreg_host_put( struct hostreg_host *host ) {
    int sts, i, idx;
    
    if( !host->id ) return -1;
    if( glob.ocount <= 0 ) return -1;
    hostreg_lock();
    
    sts = -1;
    idx = -1;
    for( i = 0; i < glob.file->header.host_count; i++ ) {
	if( glob.file->host[i].id == host->id ) {
	    idx = i;
	    break;
	}
    }
    if( idx == -1 ) {    
	if( glob.file->header.host_count < glob.file->header.host_max ) {
	    idx = glob.file->header.host_count;
	    glob.file->header.host_count++;
	}
    }

    if( idx != -1 ) {
	glob.file->host[idx] = *host;
        glob.file->header.seq++;
        sts = 0;
    }
    
    hostreg_unlock();
    return sts;
}

int hostreg_host_rem( uint64_t id ) {
    int sts, i;
    if( glob.ocount <= 0 ) return -1;
    hostreg_lock();
    sts = -1;
    for( i = 0; i < glob.file->header.host_count; i++ ) {
        if( glob.file->host[i].id == id ) {
            if( i != (glob.file->header.host_count - 1) ) glob.file->host[i] = glob.file->host[glob.file->header.host_count - 1];
            glob.file->header.host_count--;
            glob.file->header.seq++;
            sts = 0;
            break;
        }
    }
    hostreg_unlock();
    return sts;
}

