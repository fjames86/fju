/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
*/
 
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


#ifdef WIN32
#ifndef MSYS
#define strcasecmp _stricmp
#endif
#endif

#include <fju/hostreg.h>
#include <fju/mmf.h>
#include <fju/sec.h>
#include <fju/freg.h>

#define HOSTREG_ROOTPATH "/fju/hostreg"

int hostreg_open( void ) {
    int sts;
    struct sec_buf priv, pub;
    struct freg_entry entry;
    uint8_t pubkey[HOSTREG_MAX_PUBKEY];
    uint8_t privkey[HOSTREG_MAX_PRIVKEY];
    uint64_t parentid;
			
    freg_open( NULL, NULL );

    sts = freg_subkey( NULL, 0, HOSTREG_ROOTPATH "/local", FREG_CREATE, &parentid );
    if( sts ) return sts;

    sts = freg_entry_by_name( NULL, parentid, "hostid", &entry, NULL );
    if( sts ) {
      uint64_t localid;
      sec_rand( &localid, sizeof(uint64_t) );
      freg_put( NULL, parentid, "hostid", FREG_TYPE_UINT64, (char *)&localid, sizeof(localid), NULL );
    }

    sts = freg_entry_by_name( NULL, parentid, "privkey", &entry, NULL );
    if( !sts ) sts = freg_entry_by_name( NULL, parentid, "pubkey", &entry, NULL );
    if( sts ) {
	priv.buf = (char *)privkey;
	priv.len = sizeof(privkey);
	pub.buf = (char *)pubkey;
	pub.len = sizeof(pubkey);
	ecdh_generate( &priv, &pub );

	freg_put( NULL, parentid, "privkey", FREG_TYPE_OPAQUE, (char *)priv.buf, priv.len, NULL );
	freg_put( NULL, parentid, "pubkey", FREG_TYPE_OPAQUE, (char *)pub.buf, pub.len, NULL );
    }

    return 0;
}

int hostreg_close( void ) {
    freg_close( NULL );
    return 0;
}

int hostreg_reset( int full ) {
    struct sec_buf priv, pub;
    uint64_t parentid, id;
    uint64_t localid;
    uint8_t pubkey[HOSTREG_MAX_PUBKEY];
    uint8_t privkey[HOSTREG_MAX_PRIVKEY];
    int sts;

    sts = freg_subkey( NULL, 0, HOSTREG_ROOTPATH, 0, &parentid );
    if( !sts ) {
      sts = freg_subkey( NULL, parentid, "hosts", 0, &id );
      if( !sts ) freg_rem( NULL, parentid, id );
    }
   
    sts = freg_subkey( NULL, 0, HOSTREG_ROOTPATH "/local", FREG_CREATE, &parentid );
    if( sts ) return sts;

    sec_rand( &localid, sizeof(uint64_t) );
    freg_put( NULL, parentid, "hostid", FREG_TYPE_UINT64, (char *)&localid, sizeof(localid), NULL );

    priv.buf = (char *)privkey;
    priv.len = sizeof(privkey);
    pub.buf = (char *)pubkey;
    pub.len = sizeof(pubkey);
    ecdh_generate( &priv, &pub );    
    freg_put( NULL, parentid, "privkey", FREG_TYPE_OPAQUE, (char *)priv.buf, priv.len, NULL );
    freg_put( NULL, parentid, "pubkey", FREG_TYPE_OPAQUE, (char *)pub.buf, pub.len, NULL );

    return 0;
}

int hostreg_prop( struct hostreg_prop *prop ) {
  int sts;
  uint64_t parentid;
  
  sts = freg_subkey( NULL, 0, HOSTREG_ROOTPATH "/local", 0, &parentid );
  if( sts ) return sts;

  sts = freg_get_by_name( NULL, parentid, "hostid", FREG_TYPE_UINT64, (char *)&prop->localid, sizeof(prop->localid), NULL );
  if( !sts ) sts = freg_get_by_name( NULL, parentid, "pubkey", FREG_TYPE_OPAQUE, (char *)prop->pubkey, sizeof(prop->pubkey), (int *)&prop->publen );
  if( !sts ) sts = freg_get_by_name( NULL, parentid, "privkey", FREG_TYPE_OPAQUE, (char *)prop->privkey, sizeof(prop->privkey), (int *)&prop->privlen );
  if( sts ) return sts;
  
  return 0;
}

uint64_t hostreg_localid( void ) {
  int sts;
  uint64_t localid;
  sts = freg_get_by_name( NULL, 0, HOSTREG_ROOTPATH "local/hostid", FREG_TYPE_UINT64, (char *)&localid, sizeof(localid), NULL );
  if( sts ) return sts;
  return localid;
}

/* ------------ host commands ----------- */

static int get_host( uint64_t hkey, struct hostreg_host *host ) {
  int sts;
  sts = freg_get_by_name( NULL, hkey, "id", FREG_TYPE_UINT64, (char *)&host->id, sizeof(host->id), NULL );
  if( !sts ) sts = freg_get_by_name( NULL, hkey, "name", FREG_TYPE_STRING, (char *)host->name, sizeof(host->name), NULL );
  if( !sts ) sts = freg_get_by_name( NULL, hkey, "pubkey", FREG_TYPE_OPAQUE, (char *)host->pubkey, sizeof(host->pubkey), (int *)&host->publen );
  if( !sts ) sts = freg_get_by_name( NULL, hkey, "addr", FREG_TYPE_OPAQUE, (char *)host->addr, sizeof(host->addr), (int *)&host->naddr );
  host->naddr /= sizeof(uint32_t);
  return sts;    
}

static int put_host( struct hostreg_host *host ) {
  int sts;
  uint64_t parentid;
  char name[FREG_MAX_NAME];
  
  sts = freg_subkey( NULL, 0, HOSTREG_ROOTPATH "/hosts", FREG_CREATE, &parentid );
  if( sts ) return sts;

  sprintf( name, "%"PRIx64"", host->id );
  sts = freg_subkey( NULL, parentid, name, FREG_CREATE, &parentid );
  if( sts ) return sts;
  
  sts = freg_put( NULL, parentid, "id", FREG_TYPE_UINT64, (char *)&host->id, sizeof(host->id), NULL );
  sts = freg_put( NULL, parentid, "name", FREG_TYPE_STRING, (char *)host->name, sizeof(host->name), NULL );
  sts = freg_put( NULL, parentid, "pubkey", FREG_TYPE_OPAQUE, (char *)host->pubkey, host->publen, NULL );
  sts = freg_put( NULL, parentid, "addr", FREG_TYPE_OPAQUE, (char *)host->addr, sizeof(host->addr[0]) * host->naddr, NULL );
  
  return sts;    
}

int hostreg_host_list( struct hostreg_host *hostlist, int n ) {
  int sts, i, idx;
  uint64_t parentid, id;
  struct hostreg_host host;
  struct freg_entry entry;
  
  sts = freg_subkey( NULL, 0, "/fju/hostreg/hosts", 0, &parentid );
  if( sts ) return sts;

  id = 0;
  idx = 0;
  i = 0;
  do {
    sts = freg_next( NULL, parentid, id, &entry );    
    if( sts ) break;
    if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
      sts = get_host( entry.id, &host );
      if( !sts ) {
	i++;
	if( idx < n ) {
	  hostlist[idx] = host;
	  idx++;
	}
      }
    }
    id = entry.id;
  } while( 1 );

  return i;
}

int hostreg_host_by_id( uint64_t id, struct hostreg_host *host ) {
  int sts;
  uint64_t parentid, tid;
  struct hostreg_host thost;
  struct freg_entry entry;
  
  sts = freg_subkey( NULL, 0, HOSTREG_ROOTPATH "/hosts", 0, &parentid );
  if( sts ) return sts;

  tid = 0;
  do {
    sts = freg_next( NULL, parentid, tid, &entry );    
    if( sts ) break;
    if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
      sts = get_host( entry.id, &thost );
      if( !sts ) {
	if( thost.id == id ) {
	  if( host ) *host = thost;
	  return 0;
	}
      }
    }
    tid = entry.id;
  } while( 1 );

  return -1;
}

int hostreg_host_by_name( char *name, struct hostreg_host *host ) {
  int sts;
  uint64_t parentid, tid, id;
  struct hostreg_host thost;
  char *term;
  struct freg_entry entry;
			    
  tid = strtoull( name, &term, 16 );
  if( *term ) tid = 0;
  
  sts = freg_subkey( NULL, 0, HOSTREG_ROOTPATH "/hosts", 0, &parentid );
  if( sts ) return sts;

  id = 0;
  do {
    sts = freg_next( NULL, parentid, id, &entry );    
    if( sts ) break;
    if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
      sts = get_host( entry.id, &thost );
      if( !sts ) {
	if( (strcasecmp( thost.name, name ) == 0) || (thost.id == tid) ) {
	  if( host ) *host = thost;
	  return 0;
	}
      }
    }
    id = entry.id;
  } while( 1 );

  return -1;
}

uint64_t hostreg_hostid_by_name( char *name ) {
  int sts;
  struct hostreg_host host;
  sts = hostreg_host_by_name( name, &host );
  if( sts ) return 0;
  return host.id;
}

char *hostreg_name_by_hostid( uint64_t hostid, char *str ) {
  int sts;
  struct hostreg_host host;
  sts = hostreg_host_by_id( hostid, &host );
  if( sts ) return "";
  strncpy( str, host.name, sizeof(host.name) - 1 );
  return str;
}


int hostreg_host_put( struct hostreg_host *host ) {
  return put_host( host );
}

int hostreg_host_rem( uint64_t id ) {
  int sts;
  char name[FREG_MAX_NAME];
  uint64_t parentid;
  
  sts = freg_subkey( NULL, 0, HOSTREG_ROOTPATH "/hosts", 0, &parentid );
  if( sts ) return sts;

  sprintf( name, "%"PRIx64"", id );
  sts = freg_subkey( NULL, parentid, name, 0, &id );
  if( sts ) return sts;

  sts = freg_rem( NULL, parentid, id );
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
  sts = ecdh_common( &buf[0], &buf[1], &buf[2] );  
  *size = buf[2].len;
  
  return 0;
}

