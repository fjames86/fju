/*
 * MIT License
 * 
 * Copyright (c) 2018 Frank James
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

/*
 * This file defines a very simple RPC client to talk to the server defined in rpcd.c 
 * All it does is call the rpcbind_dump procedure, which lists available programs. 
 * It also uses the custom authentication flavour RPC_AUTH_SHAUTH. 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

#include "rpc.h"
#include "shauth.h"
#include <hrauth.h>
#include <hostreg.h>

static void usage( char *fmt, ... ) {
  va_list args;

  printf( "Usage: client [-p port] [-s SHAUTH-SECRET | -h HRAUTH-HOSTID]\n" 
	  "\n" );


  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }

  exit( 0 );
}


static uint8_t rpc_buf[32*1024];

static int rpcbind_decode_mapping( struct xdr_s *xdr, struct rpcbind_mapping *m ) {
  int sts;
  sts = xdr_decode_uint32( xdr, &m->prog );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &m->vers );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &m->prot );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &m->port );
  if( sts ) return sts;
  return 0;
}

static int rpcbind_decode_mapping_list( struct xdr_s *xdr, struct rpcbind_mapping *mlist, int n ) {
  int sts, b;
  int i;

  i = 0;
  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) return sts;
  while( b ) {
    if( i < n ) {
      sts = rpcbind_decode_mapping( xdr, &mlist[i] );
      if( sts ) return sts;
    }
    i++;

    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) return sts;
  }

  return i;
}

static void parse_secret( char *str, uint8_t *key ) {
  char *p, *terminator;
  int i;
  char tmp[4];
  
  p = str;
  for( i = 0; i < 32; i++ ) {
    memset( tmp, 0, 4 );
    if( *p ) {
      tmp[0] = *p;
      p++;
    }
    if( *p ) {
      tmp[1] = *p;
      p++;
    }
    
    key[i] = (uint8_t)strtoul( tmp, &terminator, 16 );
    if( *terminator ) break;
    
    if( !*p ) break;
  }
}

int main( int argc, char **argv ) {
  struct rpc_inc inc;
  struct sockaddr_in *sinp;
  int i, sts, handle;
  struct rpcbind_mapping mlist[16];
  struct shauth_context sa;
  struct hrauth_context hrauth;
  uint8_t sa_key[32] = { 0 };
  int port = 8000;
  
#ifdef WIN32
  {
      WSADATA wsadata;
      WSAStartup( MAKEWORD(2,2), &wsadata );
  }
#endif

  memset( &inc, 0, sizeof(inc) );

  hostreg_open();
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      port = strtoul( argv[i], NULL, 10 );
    } else if( strcmp( argv[i], "-s" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );

      parse_secret( argv[1], sa_key );
      shauth_init( &sa, sa_key );
      sa.service = SHAUTH_SERVICE_PRIV;
      inc.pvr = shauth_provider();
      inc.pcxt = &sa;
    } else if( strcmp( argv[i], "-h" ) == 0 ) {
      uint64_t remoteid;
      
      i++;
      if( i >= argc ) usage( NULL );

      remoteid = strtoull( argv[i], NULL, 16 );
      sts = hrauth_init( &hrauth, remoteid );
      if( sts ) usage( "Unknown remote host %"PRIx64"", remoteid );
      inc.pvr = hrauth_provider();
      inc.pcxt = &hrauth;
    } else usage( NULL );
    
    i++;
  }
  
  xdr_init( &inc.xdr, rpc_buf, sizeof(rpc_buf) );
  rpc_init_call( &inc, 100000, 2, 4, &handle );
  rpc_complete_call( &inc, handle );

  sinp = (struct sockaddr_in *)&inc.raddr;
  sinp->sin_family = AF_INET;
  sinp->sin_port = htons( port );
  sinp->sin_addr.s_addr = htonl( INADDR_LOOPBACK );
  inc.raddr_len = sizeof(*sinp);
  
  sts = rpc_call_udp( &inc );
  if( sts ) {
    printf( "Failed to call\n" );
    exit( 1 );
  }

  sts = rpc_process_reply( &inc );
  if( sts ) {
    printf( "Failed to receive reply\n" );
    exit( 1 );
  }
  
  /* decode results */
  sts = rpcbind_decode_mapping_list( &inc.xdr, mlist, 16 );
  for( i = 0; i < sts; i++ ) {
    printf( "%-8d %-8d %-8d %-8d\n", mlist[i].prog, mlist[i].vers, mlist[i].prot, mlist[i].port );
  }
  
  return 0;
}

