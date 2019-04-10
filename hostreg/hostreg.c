
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS

#include <WinSock2.h>
#include <Windows.h>
#include <iphlpapi.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>

#ifndef WIN32
#include <sys/types.h>
#include <ifaddrs.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#endif


#include "hostreg.h"
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
    uint8_t pubkey[HOSTREG_MAX_PUBKEY];
    uint32_t privlen;
    uint8_t privkey[HOSTREG_MAX_PRIVKEY];
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

    sts = mmf_open( mmf_default_path( "hostreg.dat", NULL ), &glob.mmf );
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

	sec_rand( &glob.file->header.localid, sizeof(uint64_t) );
	priv.buf = (char *)glob.file->header.privkey;
	priv.len = sizeof(glob.file->header.privkey);
	pub.buf = (char *)glob.file->header.pubkey;
	pub.len = sizeof(glob.file->header.pubkey);
	ecdh_generate( &priv, &pub );
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

int hostreg_reset( int full ) {
    struct sec_buf priv, pub;
  
    if( glob.ocount <= 0 ) return -1;
    hostreg_lock();
    glob.file->header.magic = HOSTREG_MAGIC;
    glob.file->header.version = HOSTREG_VERSION;
    glob.file->header.seq = 1;
    glob.file->header.host_max = HOSTREG_MAX_HOST;
    glob.file->header.host_count = 0;
    if( full ) {
        sec_rand( &glob.file->header.localid, sizeof(uint64_t) );
	priv.buf = (char *)glob.file->header.privkey;
	priv.len = sizeof(glob.file->header.privkey);
	pub.buf = (char *)glob.file->header.pubkey;
	pub.len = sizeof(glob.file->header.pubkey);
	ecdh_generate( &priv, &pub );
	glob.file->header.privlen = priv.len;
	glob.file->header.publen = pub.len;
    }
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
    memcpy( prop->pubkey, glob.file->header.pubkey, glob.file->header.publen );
    prop->privlen = glob.file->header.privlen;
    memcpy( prop->privkey, glob.file->header.privkey, glob.file->header.privlen );
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

int hostreg_host_local( struct hostreg_host *host ) {
  struct hostreg_prop prop;
  struct sockaddr_in *sinp;
     
  hostreg_prop( &prop );
  memset( host, 0, sizeof(*host) );
     
  host->id = prop.localid;
  gethostname( host->name, sizeof(host->name) );
  memcpy( host->pubkey, prop.pubkey, prop.publen );
  host->publen = prop.publen;
  
#ifdef WIN32
  {
    char *buf = malloc( 32 * 1024 );
    IP_ADAPTER_ADDRESSES *ipa = buf;		
    IP_ADAPTER_UNICAST_ADDRESS *ipu;
    DWORD plen;
	struct sockaddr_in *sinp;

    plen = 32 * 1024;
    GetAdaptersAddresses( 0, 0, NULL, ipa, &plen );
    while( ipa ) {
	  if(ipa->OperStatus == IfOperStatusUp && ipa->IfType != IF_TYPE_SOFTWARE_LOOPBACK) {
        ipu = ipa->FirstUnicastAddress;	  
        while( ipu ) {		 
          if( (ipu->Address.lpSockaddr->sa_family == AF_INET) && (host->naddr < HOSTREG_MAX_ADDR) ) {
  	        sinp = (struct sockaddr_in *)ipu->Address.lpSockaddr;
            host->addr[host->naddr] = sinp->sin_addr.s_addr;
		    host->naddr++;
		  }
	      ipu = ipu->Next;
	    }
	  }
      ipa = ipa->Next;
    }
    free( buf );
  }
#else
  {
    struct ifaddrs *ifl, *ifa;
    getifaddrs( &ifl );
    ifa = ifl;
    while( ifa ) {
      sinp = (struct sockaddr_in *)ifa->ifa_addr;
      if( sinp && sinp->sin_family == AF_INET && ifa->ifa_flags & IFF_UP && !(ifa->ifa_flags & IFF_LOOPBACK) ) {
	if( host->naddr < HOSTREG_MAX_ADDR ) {
	  host->addr[host->naddr] = sinp->sin_addr.s_addr;
	  host->naddr++;
	}
      }
      ifa = ifa->ifa_next;
    }
    freeifaddrs( ifl );
  }
#endif

  return 0;
}

int hostreg_host_common( uint64_t hostid, char *common, int *size ) {
  int sts;
  struct hostreg_prop prop;
  struct hostreg_host host;
  struct sec_buf buf[3];
  
  sts = hostreg_host_by_id( hostid, &host );
  if( sts ) return sts;

  hostreg_prop( &prop );

  sec_buf_init( &buf[0], (char *)prop.privkey, prop.privlen );
  sec_buf_init( &buf[1], (char *)host.pubkey, prop.publen );
  memset( common, 0, *size );
  sec_buf_init( &buf[2], common, *size );
  ecdh_common( &buf[0], &buf[1], &buf[2] );
  *size = buf[2].len;
  
  return 0;
}
