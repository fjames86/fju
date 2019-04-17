
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
#include <mmf.h>
#include <sec.h>
#include "hrauth.h"
#include <rpc.h>

#ifndef WIN32
#include <sys/types.h>
#include <ifaddrs.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include "hostreg.h"

static void print_host( struct hostreg_host *host );

static void usage( char *fmt, ... ) {
    printf( "Usage:    prop\n"
	    "          reset [full]\n" 
            "          add [id=ID] [name=NAME] [pubkey=PUBKEY] [addr=ADDR] ]\n"
            "          set ID [name=NAME] [pubkey=PUBKEY] [addr=ADDR] ]\n"
            "          rem ID\n"
	    "          common ID\n"
	    "          call [addr=ADDR] [port=PORT]\n" 
    );

    if( fmt ) {
        va_list args;
        printf( "Error: " );
        va_start( args, fmt );
        vprintf( fmt, args );
        va_end( args );
        printf( "\n" );
    }
    exit( 0 );
}

static void argval_split( char *instr, char *argname, char **argval ) {
    char *p;

    p = strchr( instr, '=' );
    if( p ) *argval = p + 1;
    else *argval = NULL;

    p = instr;
    while( *p != '0' && *p != '=' ) {
        *argname = *p;
        p++;
        argname++;
    }
    *argname = '\0';
}

static void cmd_list( void );
static void cmd_prop( void );
static void hex2bn( char *hex, struct sec_buf *buf );
static void bn2hex( char *bn, char *hex, int len );
static int mynet_pton( char *str, uint8_t *inaddr );
static char *mynet_ntop( uint32_t inaddr, char *str );
static void cmd_common( uint64_t id );
static void cmd_call( uint32_t addr, int port );

int main( int argc, char **argv ) {
    int sts, i;

#ifdef WIN32
	{
		WSADATA wsadata;
		WSAStartup( MAKEWORD( 2, 2 ), &wsadata );
	}
#endif

    sts = hostreg_open();
    if( sts ) usage( "Failed to open" );

    i = 1;
    if( i >= argc ) {
        cmd_list();
    } else if( strcmp( argv[i], "list" ) == 0 ) {
        cmd_list();
    } else if( strcmp( argv[i], "prop" ) == 0 ) {
        cmd_prop();
    } else if( strcmp( argv[i], "reset" ) == 0 ) {
        int full = 0;
	i++;
	while( i < argc ) {
	  if( strcmp( argv[i], "full" ) == 0 ) {
	    full = 1;
	  } else usage( NULL );
	  i++;
	}
	hostreg_reset( full );
    } else if( strcmp( argv[i], "add" ) == 0 ) {
	struct hostreg_host entry;
	char argname[64], *argval;
	memset( &entry, 0, sizeof(entry) );
	i++;
	while( i < argc ) {
	    argval_split( argv[i], argname, &argval );
	    if( strcmp( argname, "ID" ) == 0 ) {
		if( argval ) entry.id = strtoull( argval, NULL, 16 );
	    } else if( strcmp( argname, "name" ) == 0 ) {
		if( argval ) strncpy( entry.name, argval, sizeof(entry.name) );
	    } else if( strcmp( argname, "pubkey" ) == 0 ) {
		struct sec_buf buf;
		
		if( !argval ) usage( NULL );
		
		buf.buf = (char *)entry.pubkey;
		buf.len = sizeof(entry.pubkey);
		hex2bn( argval, &buf );
		entry.publen = buf.len;
	    } else if( strcmp( argname, "addr" ) == 0 ) {
		if( !argval ) usage( NULL );
		if( entry.naddr < 8 ) {
		    mynet_pton( argval, (uint8_t *)&entry.addr[entry.naddr] );
		    entry.naddr++;
		}
	    } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
	    i++;
	}
	sts = hostreg_host_put( &entry );
	if( sts ) usage( "Failed to add host" );
	print_host( &entry );
    } else if( strcmp( argv[i], "rem" ) == 0 ) {
	uint64_t id;
	i++;
	if( i >= argc ) usage( NULL );
	id = strtoull( argv[i], NULL, 16 );
	sts = hostreg_host_rem( id );
	if( sts ) usage( "Failed to rem host" );
    } else if( strcmp( argv[i], "set" ) == 0 ) {
	struct hostreg_host entry;
	char argname[64], *argval;
	uint64_t id;
	int setaddr = 0;
	memset( &entry, 0, sizeof(entry) );
	i++;
	if( i >= argc ) usage( NULL );
	id = strtoull( argv[i], NULL, 16 );
	sts = hostreg_host_by_id( id, &entry );
	if( sts ) usage( "Failed to lookup" );
	i++;
	while( i < argc ) {
	    argval_split( argv[i], argname, &argval );
	    if( strcmp( argname, "name" ) == 0 ) {
		if( argval ) strncpy( entry.name, argval, sizeof(entry.name) );
	    } else if( strcmp( argname, "pubkey" ) == 0 ) {
		struct sec_buf buf;		     
		if( !argval ) usage( NULL );
		buf.buf = (char *)entry.pubkey;
		buf.len = sizeof(entry.pubkey);
		hex2bn( argval, &buf );
		entry.publen = buf.len;
	    } else if( strcmp( argname, "addr" ) == 0 ) {
		if( !argval ) usage( NULL );
		if( !setaddr ) {
		  entry.naddr = 0;
		  setaddr = 1;
		}
		if( entry.naddr < 8 ) {
		    mynet_pton( argval, (uint8_t *)&entry.addr[entry.naddr] );
		    entry.naddr++;
		}
	    } else { printf( "Unknown field name %s\n", argname ); usage( NULL ); }
	    i++;
	}
	sts = hostreg_host_put( &entry );
	if( sts ) usage( "Failed to set host" );
    } else if( strcmp( argv[i], "common" ) == 0 ) {
	uint64_t id;
	i++;
	if( i >= argc ) usage( NULL );
	id = strtoull( argv[i], NULL, 16 );
	cmd_common( id );
    } else if( strcmp( argv[i], "call" ) == 0 ) {
        uint32_t addr;
        int port;
	char argname[64], *argval;

	addr = htonl( INADDR_LOOPBACK );
	port = 8000;
	
	i++;
	while( i < argc ) {
	  argval_split( argv[i], argname, &argval );
	  if( strcmp( argname, "addr" ) == 0 ) {
	    mynet_pton( argval, (uint8_t *)&addr );
	  } else if( strcmp( argname, "port" ) == 0 ) {
	    port = strtoul( argval, NULL, 10 );
	  } else usage( NULL );
	  i++;
	}

	cmd_call( addr, port );
    } else usage( NULL );

    hostreg_close();
    return 0;
}

static void print_host( struct hostreg_host *host ) {
    char hex[256];
    int j;
    
    memset( hex, 0, sizeof(hex) );
    bn2hex( (char *)host->pubkey, hex, host->publen );
    printf( "ID=%"PRIx64" name=%s pubkey=%s ",
	    host->id, host->name, hex );

    for( j = 0; j < host->naddr; j++ ) {
	mynet_ntop( host->addr[j], hex );
	printf( "addr=%s ", hex );
    }
    printf( "\n" );
    
}

static void cmd_list( void ) {
    int sts, i, n, m, j;
    struct hostreg_host *lst;

    n = hostreg_host_list( NULL, 0 );
    lst = (struct hostreg_host *)malloc( sizeof(*lst) * n );
    m = hostreg_host_list( lst, n );
    if( m < n ) n = m;
    for( i = 0; i < n; i++ ) {
	print_host( &lst[i] );
    }
    free( lst );
    printf( "\n" );
}

static void cmd_prop( void ) {
     struct hostreg_prop prop;
     char hex[256];
     struct ifaddrs *ifl, *ifa;
     struct sockaddr_in *sinp;
	 struct hostreg_host host;

     hostreg_prop( &prop );
     printf( "seq=%"PRIu64"\n", prop.seq );
     printf( "host=%d/%d\n", prop.host_count, prop.host_max );
     bn2hex( (char *)prop.privkey, hex, prop.privlen );
     printf( "privkey=%s\n", hex );

	 hostreg_host_local( &host );
	 print_host( &host );

	 printf( "\n" );
}

static void hex2bn( char *hex, struct sec_buf *buf ) {
  int i, j;
  unsigned char x;
  for( i = 0; i < buf->len; i++ ) {
    if( hex[2*i] == '\0' ) break;

    x = 0;
    j = 2 * i;
    if( hex[j] != '\0' ) {
      if( hex[j] >= '0' && hex[j] <= '9' ) x = hex[j] - '0';
      else if( hex[j] >= 'a' && hex[j] <= 'f' ) x = 10 + (hex[j] - 'a');
      else if( hex[j] >= 'A' && hex[j] <= 'F' ) x = 10 + (hex[j] - 'A');
      else usage( "Unable to parse \"%s\"", hex );
    }
    x = x << 4;
    if( hex[2*i + 1] != '\0' ) {
      j = (2*i) + 1;
      if( hex[j] >= '0' && hex[j] <= '9' ) x |= hex[j] - '0';
      else if( hex[j] >= 'a' && hex[j] <= 'f' ) x |= 10 + (hex[j] - 'a');
      else if( hex[j] >= 'A' && hex[j] <= 'F' ) x |= 10 + (hex[j] - 'A');
      else usage( "Unable to parse \"%s\"", hex );
    }
    buf->buf[i] = x;
    if( hex[(2*i)+1] == '\0' ) break;
  }
  buf->len = i;
}

static void bn2hex( char *bn, char *hex, int len ) {
  int i;
  unsigned int x;
  memset( hex, 0, 2*len );
  for( i = 0; i < len; i++ ) {
    x = (unsigned int)((unsigned char)bn[i]);
    sprintf( hex + 2*i, "%02x", x );
  }
  hex[len*2] = '\0';
}

static char *mynet_ntop( uint32_t inaddr, char *str ) {
  uint8_t *inp = (uint8_t *)&inaddr;
    sprintf( str, "%d.%d.%d.%d", inp[0], inp[1], inp[2], inp[3] );
    return str;	     
}

static int mynet_pton( char *str, uint8_t *inaddr ) {
    char *p;
    char tmp[4];
    int i, j;
    
    p = str;
    memset( inaddr, 0, 4 );
    
    for( j = 0; j < 4; j++ ) {
	memset( tmp, 0, sizeof(tmp) );
	i = 0;
	while( 1 ) {
	    if( *p == '\0' ) break;
	    if( *p == '.' ) {
		p++;
		break;
	    }

	    if( i <= 3 ) {
	      tmp[i] = *p;
	    }
	    i++;
	    p++;
	}
	inaddr[j] = strtoul( tmp, NULL, 10 );
    }

    return 0;
}

static void cmd_common( uint64_t id ) {
  uint8_t common[32];
  int sts, size;
  char hex[256];
  
  size = sizeof(common);
  sts = hostreg_host_common( id, (char *)common, &size );
  if( sts ) usage( "Unknown host \"%"PRIx64"\"", id );

  bn2hex( (char *)common, hex, size );
  printf( "%s\n", hex );  
}

static int hostreg_decode_host( struct xdr_s *xdr, struct hostreg_host *x ) {
  int sts, i;
  memset( x, 0, sizeof(*x) );
  sts = xdr_decode_uint64( xdr, &x->id );
  if( sts ) return sts;
  sts = xdr_decode_string( xdr, x->name, sizeof(x->name) );
  if( sts ) return sts;
  x->publen = sizeof(x->pubkey);
  sts = xdr_decode_opaque( xdr, x->pubkey, (int *)&x->publen );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &x->naddr );
  if( sts ) return sts;
  if( x->naddr > HOSTREG_MAX_ADDR ) return -1;
  for( i = 0; i < x->naddr; i++ ) {
    sts = xdr_decode_uint32( xdr, &x->addr[i] );
    if( sts ) return sts;
  }
  return 0;
}

static void cmd_call( uint32_t addr, int port ) {
  struct rpc_inc inc;
  struct sockaddr_in *sinp;
  int handle;
  int sts;
  struct hostreg_host host;
  uint8_t *rpc_buf = malloc( 32*1024 );

  memset( &inc, 0, sizeof(inc) );
  xdr_init( &inc.xdr, rpc_buf, 32*1024 );
  rpc_init_call( &inc, HRAUTH_PROGRAM, HRAUTH_VERSION, 1, &handle );
  rpc_complete_call( &inc, handle );

  sinp = (struct sockaddr_in *)&inc.raddr;
  sinp->sin_family = AF_INET;
  sinp->sin_port = htons( port );
  sinp->sin_addr.s_addr = addr;
  inc.raddr_len = sizeof(*sinp);
  
  sts = rpc_call_udp( &inc );  
  if( sts ) {
    char addrstr[64];
    mynet_ntop( addr, addrstr );
    usage( "Failed to call addr=%s port=%d", addrstr, port );
  }

  sts = rpc_process_reply( &inc );
  if( sts ) usage( "Failed to receive reply" );
  
  /* decode results */
  memset( &host, 0, sizeof(host) );
  sts = hostreg_decode_host( &inc.xdr, &host );
  if( sts ) usage( "Failed to decode host" );
  print_host( &host );

  free( rpc_buf );
}
