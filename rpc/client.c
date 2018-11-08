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

#include "rpc.h"

#ifdef USE_SHAUTH
#include "shauth.h"
#endif

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

int main( int argc, char **argv ) {
  struct rpc_inc inc;
  struct sockaddr_in *sinp;
  int i, sts, handle;
  struct rpcbind_mapping mlist[16];
#ifdef USE_SHAUTH
  struct shauth_context sa;
  uint8_t sa_key[32] = { 0 };
#endif

  
  memset( &inc, 0, sizeof(inc) );
#ifdef USE_SHAUTH
  shauth_init( &sa, sa_key );
  sa.service = SHAUTH_SERVICE_PRIV;
  inc.pvr = shauth_provider();
  inc.pcxt = &sa;
#endif
  
  xdr_init( &inc.xdr, rpc_buf, sizeof(rpc_buf) );
  rpc_init_call( &inc, 100000, 2, 4, &handle );
  rpc_complete_call( &inc, handle );


  sinp = (struct sockaddr_in *)&inc.raddr;
  sinp->sin_family = AF_INET;
  sinp->sin_port = htons( 8000 );
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

