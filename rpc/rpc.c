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
 * This file defines a very simple ONC/RPC ("SunRPC") implementation. 
 * It can be extended with custom authentication providers (struct rpc_provider) 
 * which themselves may optionally modify argument/result buffers e.g. to implement 
 * encryption or checksumming.
 */

#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#endif

#include "rpc.h"

#include <errno.h>
#include <stdarg.h>
#include <time.h>

#ifndef WIN32
#include <poll.h>
#endif

void xdr_init( struct xdr_s *xdr, uint8_t *buf, int size ) {
  xdr->buf = buf;
  xdr->buf_size = size;
  xdr->offset = 0;
  xdr->count = size;
}

void xdr_reset( struct xdr_s *xdr ) {
  xdr->offset = 0;
  xdr->count = xdr->buf_size;
}

int xdr_encode_uint32( struct xdr_s *xdr, uint32_t x ) {
  if( (xdr->offset + 4) > xdr->count ) return -1;
  xdr->buf[xdr->offset++] = (x >> 24) & 0xff;
  xdr->buf[xdr->offset++] = (x >> 16) & 0xff;
  xdr->buf[xdr->offset++] = (x >> 8) & 0xff;
  xdr->buf[xdr->offset++] = x & 0xff;
  return 0;
}

int xdr_encode_uint64( struct xdr_s *xdr, uint64_t x ) {
  int sts;
  sts = xdr_encode_uint32( xdr, x >> 32 );
  if( sts ) return sts;
  sts = xdr_encode_uint32( xdr, x & 0xffffffff );
  if( sts ) return sts;
  return 0;
}

int xdr_decode_uint32( struct xdr_s *xdr, uint32_t *x ) {
  if( (xdr->offset + 4) > xdr->count ) return -1;
  *x  = (xdr->buf[xdr->offset++] << 24);
  *x |= (xdr->buf[xdr->offset++] << 16);
  *x |= (xdr->buf[xdr->offset++] << 8);
  *x |= (xdr->buf[xdr->offset++]);
  return 0;
}

int xdr_decode_uint64( struct xdr_s *xdr, uint64_t *x ) {
  int sts;
  uint32_t l, h;
  sts = xdr_decode_uint32( xdr, &h );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &l );
  if( sts ) return sts;
  *x = ((uint64_t)h << 32) | (uint64_t)l;
  return 0;
}
int xdr_encode_int32( struct xdr_s *xdr, int32_t x ) {
  return xdr_encode_uint32( xdr, (uint32_t)x );
}

int xdr_encode_int64( struct xdr_s *xdr, int64_t x ) {
  return xdr_encode_uint64( xdr, (uint64_t)x );
}

int xdr_decode_int32( struct xdr_s *xdr, int32_t *x ) {
  return xdr_decode_uint32( xdr, (uint32_t *)x );
}

int xdr_decode_int64( struct xdr_s *xdr, int64_t *x ) {
  return xdr_decode_uint64( xdr, (uint64_t *)x );
}

int xdr_encode_boolean( struct xdr_s *xdr, int x ) {
  return xdr_encode_uint32( xdr, x ? 1 : 0 );
}

int xdr_decode_boolean( struct xdr_s *xdr, int *x ) {
  uint32_t u;
  int sts;
  sts = xdr_decode_uint32( xdr, &u );
  if( sts ) return sts;
  *x = u ? 1 : 0;
  return 0;
}

int xdr_encode_string( struct xdr_s *xdr, char *str ) {
  int sts;
  uint32_t len, xlen;
  len = (uint32_t)strlen( str );
  sts = xdr_encode_uint32( xdr, len );
  if( sts ) return sts;
  xlen = len;
  if( xlen % 4 ) xlen += 4 - (len % 4);
  if( (xdr->offset + xlen) > (uint32_t)xdr->count ) return -1;
  memcpy( xdr->buf + xdr->offset, str, len );
  if( len % 4 ) memset( xdr->buf + xdr->offset + len, 0, 4 - (len % 4) );
  xdr->offset += xlen;
  return 0;
}

int xdr_decode_string( struct xdr_s *xdr, char *str, int n ) {
  uint32_t len, xlen, tocopy;
  int sts;

  sts = xdr_decode_uint32( xdr, &len );
  if( sts ) return sts;
  xlen = len;
  if( xlen % 4 ) xlen += 4 - (xlen % 4);
  if( (xdr->offset + xlen) > (uint32_t)xdr->count ) return -1;
  tocopy = len;
  if( tocopy > (uint32_t)(n - 1) ) tocopy = (uint32_t)(n - 1);
  memcpy( str, xdr->buf + xdr->offset, tocopy );
  str[tocopy] = '\0';
  xdr->offset += xlen;
  return 0;
}

int xdr_encode_fixed( struct xdr_s *xdr, uint8_t *buf, int n ) {
  if( (xdr->offset + n) > xdr->count ) return -1;
  memcpy( xdr->buf + xdr->offset, buf, n );
  xdr->offset += n;
  return 0;
}

int xdr_decode_fixed( struct xdr_s *xdr, uint8_t *buf, int n ) {
  if( (xdr->offset + n) > xdr->count ) return -1;
  memcpy( buf, xdr->buf + xdr->offset, n );
  xdr->offset += n;
  return 0;
}

int xdr_encode_opaque( struct xdr_s *xdr, uint8_t *buf, int n ) {
  uint32_t xlen;
  int sts;
  sts = xdr_encode_uint32( xdr, n );
  if( sts ) return sts;
  xlen = n;
  if( xlen % 4 ) xlen += 4 - (xlen % 4);
  if( (xdr->offset + xlen) > (uint32_t)xdr->count ) return -1;
  memcpy( xdr->buf + xdr->offset, buf, n );
  xdr->offset += xlen;
  return 0;
}

int xdr_decode_opaque( struct xdr_s *xdr, uint8_t *buf, int *n ) {
  uint32_t len, xlen;
  int sts;

  sts = xdr_decode_uint32( xdr, &len );
  if( sts ) return sts;
  xlen = len;
  if( xlen % 4 ) xlen += 4 - (xlen % 4);
  memcpy( buf, xdr->buf + xdr->offset, (int)len > *n ? *n : (int)len );
  xdr->offset += xlen;
  *n = (int)len;
  return 0;
}

int xdr_decode_opaque_ref( struct xdr_s *xdr, uint8_t **buf, int *n ) {
  uint32_t len, xlen;
  int sts;

  sts = xdr_decode_uint32( xdr, &len );
  if( sts ) return sts;
  xlen = len;
  if( xlen % 4 ) xlen += 4 - (xlen % 4);
  *buf = xdr->buf + xdr->offset;
  *n = len;
  xdr->offset += xlen;
  return 0;
}

int rpc_encode_opaque_auth( struct xdr_s *xdr, struct rpc_opaque_auth *x ) {
  int sts;
  sts = xdr_encode_uint32( xdr, x->flavour );
  if( sts ) return sts;
  sts = xdr_encode_opaque( xdr, x->data, x->len );
  if( sts ) return sts;
  return 0;
}
int rpc_decode_opaque_auth( struct xdr_s *xdr, struct rpc_opaque_auth *x ) {
  int sts;
  sts = xdr_decode_uint32( xdr, (uint32_t *)&x->flavour );
  if( sts ) return sts;
  x->len = sizeof(x->data);
  sts = xdr_decode_opaque( xdr, x->data, (int *)&x->len );
  if( sts ) return sts;
  return 0;
}
int rpc_encode_mismatch( struct xdr_s *xdr, struct rpc_mismatch *x ) {
  int sts;
  sts = xdr_encode_uint32( xdr, x->low );
  if( sts ) return sts;
  sts = xdr_encode_uint32( xdr, x->high );
  if( sts ) return sts;
  return 0;
}
int rpc_decode_mismatch( struct xdr_s *xdr, struct rpc_mismatch *x ) {
  int sts;
  sts = xdr_decode_uint32( xdr, &x->low );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &x->high );
  if( sts ) return sts;
  return 0;
}
int rpc_encode_msg( struct xdr_s *xdr, struct rpc_msg *x ) {
  int sts;
  sts = xdr_encode_uint32( xdr, x->xid );
  if( sts ) return sts;
  sts = xdr_encode_int32( xdr, (int32_t)x->tag );
  if( sts ) return sts;
  switch( x->tag ) {
  case RPC_CALL:
    sts = xdr_encode_uint32( xdr, x->u.call.rpcvers );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, x->u.call.prog );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, x->u.call.vers );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, x->u.call.proc );
    if( sts ) return sts;
    sts = rpc_encode_opaque_auth( xdr, &x->u.call.auth );
    if( sts ) return sts;
    sts = rpc_encode_opaque_auth( xdr, &x->u.call.verf );
    if( sts ) return sts;
    break;
  case RPC_REPLY:
    sts = xdr_encode_int32( xdr, x->u.reply.tag );
    if( sts ) return sts;
    switch( x->u.reply.tag ) {
    case RPC_MSG_ACCEPT:
      sts = rpc_encode_opaque_auth( xdr, &x->u.reply.u.accept.verf );
      if( sts ) return sts;
      sts = xdr_encode_int32( xdr, x->u.reply.u.accept.tag );
      if( sts ) return sts;
      switch( x->u.reply.u.accept.tag ) {
      case RPC_ACCEPT_PROG_MISMATCH:
	sts = rpc_encode_mismatch( xdr, &x->u.reply.u.accept.u.mismatch );
	if( sts ) return sts;
	break;
      default:
	break;
      }
      break;
    case RPC_MSG_REJECT:
      sts = xdr_encode_int32( xdr, x->u.reply.u.reject.tag );
      if( sts ) return sts;
      switch( x->u.reply.u.reject.tag ) {
      case RPC_REJECT_RPCMISMATCH:
	sts = rpc_encode_mismatch( xdr, &x->u.reply.u.reject.u.mismatch );
	if( sts ) return sts;
	break;
      case RPC_REJECT_AUTH_ERROR:
	sts = xdr_encode_int32( xdr, x->u.reply.u.reject.u.auth_error );
	break;
      }
      break;
    default:
      break;
    }
    break;
  default:
    return -1;
  }
  return 0;
}

int rpc_decode_msg( struct xdr_s *xdr, struct rpc_msg *x ) {
  int sts;
  sts = xdr_decode_uint32( xdr, &x->xid );
  if( sts ) return sts;
  sts = xdr_decode_int32( xdr, (int32_t *)&x->tag );
  if( sts ) return sts;
  switch( x->tag ) {
  case RPC_CALL:
    sts = xdr_decode_uint32( xdr, &x->u.call.rpcvers );
    if( sts ) return sts;
    sts = xdr_decode_uint32( xdr, &x->u.call.prog );
    if( sts ) return sts;
    sts = xdr_decode_uint32( xdr, &x->u.call.vers );
    if( sts ) return sts;
    sts = xdr_decode_uint32( xdr, &x->u.call.proc );
    if( sts ) return sts;
    sts = rpc_decode_opaque_auth( xdr, &x->u.call.auth );
    if( sts ) return sts;
    sts = rpc_decode_opaque_auth( xdr, &x->u.call.verf );
    if( sts ) return sts;
    break;
  case RPC_REPLY:
    sts = xdr_decode_int32( xdr, (int32_t *)&x->u.reply.tag );
    if( sts ) return sts;
    switch( x->u.reply.tag ) {
    case RPC_MSG_ACCEPT:
      sts = rpc_decode_opaque_auth( xdr, &x->u.reply.u.accept.verf );
      if( sts ) return sts;
      sts = xdr_decode_int32( xdr, (int32_t *)&x->u.reply.u.accept.tag );
      if( sts ) return sts;
      switch( x->u.reply.u.accept.tag ) {
      case RPC_ACCEPT_PROG_MISMATCH:
	sts = rpc_decode_mismatch( xdr, &x->u.reply.u.accept.u.mismatch );
	if( sts ) return sts;
	break;
      default:
	break;
      }
      break;
    case RPC_MSG_REJECT:
      sts = xdr_decode_int32( xdr, (int32_t *)&x->u.reply.u.reject.tag );
      if( sts ) return sts;
      switch( x->u.reply.u.reject.tag ) {
      case RPC_REJECT_RPCMISMATCH:
	sts = rpc_decode_mismatch( xdr, &x->u.reply.u.reject.u.mismatch );
	if( sts ) return sts;
	break;
      case RPC_REJECT_AUTH_ERROR:
	sts = xdr_decode_int32( xdr, (int32_t *)&x->u.reply.u.reject.u.auth_error );
	break;
      default:
	break;
      }
      break;
    }
    break;
  default:
    return -1;
  }
  return 0;
}



static struct rpc_program *plist;
void rpc_program_register( struct rpc_program *p ) {
  p->next = plist;
  plist = p;
}
void rpc_program_unregister( struct rpc_program *p ) {
  struct rpc_program *pg;
  struct rpc_program *prev = NULL;
  pg = plist;
  while( pg ) {
    if( pg == p ) {
      if( prev ) prev->next = p->next;
      else plist = p->next;
      return;
    }
    prev = pg;
    pg = pg->next;
  }
}

struct rpc_program *rpc_program_list( void ) {
  return plist;
}


int rpc_program_find( uint32_t prog, uint32_t vers, uint32_t proc,
                      struct rpc_program **p,
                      struct rpc_version **v,
                      struct rpc_proc **pc ) {
  int i;
  struct rpc_program *pg;
  struct rpc_version *vs;

  *p = NULL;
  *v = NULL;
  *pc = NULL;

  pg = plist;
  while( pg ) {
    if( pg->prog == prog ) {
      *p = pg;

      vs = pg->vers;
      while( vs ) {
	if( vs->vers == vers ) {
	  *v = vs;

	  i = 0;
	  while( vs->procs[i].fn ) {
	    if( vs->procs[i].proc == proc ) {
	      *pc = &vs->procs[i];
	      return 0;
	    }
	    i++;
	  }
	  return -1;
	}
	vs = vs->next;
      }
      return -1;
    }
    pg = pg->next;
  }
  return -1;
}


static struct rpc_provider *pvrlist;

void rpc_provider_register( struct rpc_provider *p ) {
  p->next = pvrlist;
  pvrlist = p;
}

void rpc_provider_unregister( struct rpc_provider *p ) {
  struct rpc_provider *pvr, *prev;
  pvr = pvrlist;
  prev = NULL;
  while( pvr ) {
    if( pvr == p ) {
      if( prev ) prev->next = pvr->next;
      else pvrlist = pvr->next;
      return;
    }
    prev = pvr;
    pvr = pvr->next;
  }

}

struct rpc_provider *rpc_provider_by_flavour( int32_t flavour ) {
  struct rpc_provider *p;
  p = pvrlist;
  while( p ) {
    if( p->taste && p->taste( p, flavour ) ) return p;
    if( p->flavour == flavour ) return p;
    p = p->next;
  }
  return NULL;
}



int rpc_init_call( struct rpc_inc *inc, int32_t prog, uint32_t vers, uint32_t proc, int *handle ) {
  static uint32_t xid;

  int sts;

  memset( &inc->msg, 0, sizeof(inc->msg) );
  inc->msg.xid = xid++;
  inc->msg.tag = RPC_CALL;
  inc->msg.u.call.rpcvers = RPC_VERS;
  inc->msg.u.call.prog = prog;
  inc->msg.u.call.vers = vers;
  inc->msg.u.call.proc = proc;
  if( inc->pvr ) {
    sts = inc->pvr->cauth( inc->pvr, &inc->msg, inc->pcxt );
    if( sts ) return sts;
  }

  xdr_reset( &inc->xdr );    
  sts = rpc_encode_msg( &inc->xdr, &inc->msg );

  if( handle ) *handle = inc->xdr.offset;

  return sts;    
}

int rpc_complete_call( struct rpc_inc *inc, int handle ) {
  int sts;

  if( inc->pvr && inc->pvr->cmargs ) {
    sts = inc->pvr->cmargs( inc->pvr, &inc->xdr, handle, inc->xdr.offset, inc->pcxt );
    if( sts ) return sts;
  }

  return 0;
}


int rpc_init_accept_reply( struct rpc_inc *inc, uint32_t xid, int accept_stat, struct rpc_mismatch *mm, int *handle ) {
  int sts;

  memset( &inc->msg, 0, sizeof(inc->msg) );
  inc->msg.xid = xid;
  inc->msg.tag = RPC_REPLY;
  inc->msg.u.reply.tag = RPC_MSG_ACCEPT;
  inc->msg.u.reply.u.accept.tag = accept_stat;
  if( mm ) inc->msg.u.reply.u.accept.u.mismatch = *mm;
  if( inc->pvr ) inc->msg.u.reply.u.accept.verf = inc->pvr->rverf;

  xdr_reset( &inc->xdr );
  sts = rpc_encode_msg( &inc->xdr, &inc->msg );
  if( sts ) return sts;

  if( handle ) *handle = inc->xdr.offset;

  return 0;
}

int rpc_complete_accept_reply( struct rpc_inc *inc, int handle ) {
  int sts;

  if( inc->pvr && inc->pvr->smres ) {
    sts = inc->pvr->smres( inc->pvr, &inc->xdr, handle, inc->xdr.offset, inc->pcxt );
    if( sts ) return sts;
  }
    
  return 0;
}

int rpc_init_reject_reply( struct rpc_inc *inc, uint32_t xid, int32_t auth_stat ) {
  int sts;

  memset( &inc->msg, 0, sizeof(inc->msg) );
  inc->msg.xid = xid;
  inc->msg.tag = RPC_REPLY;
  inc->msg.u.reply.tag = RPC_MSG_REJECT;
  inc->msg.u.reply.u.reject.tag = RPC_REJECT_AUTH_ERROR;
  inc->msg.u.reply.u.reject.u.auth_error = auth_stat;

  xdr_reset( &inc->xdr );
  sts = rpc_encode_msg( &inc->xdr, &inc->msg );
  return sts;
}

int rpc_process_incoming( struct rpc_inc *inc ) {
  int sts;
  struct rpc_program *p;
  struct rpc_version *v;
  struct rpc_proc *pc;
  int start, handle;

  sts = rpc_decode_msg( &inc->xdr, &inc->msg );
  if( sts ) return sts;
    
  switch( inc->msg.tag ) {
  case RPC_CALL:
    rpc_log( RPC_LOG_INFO, "CALL %d:%d:%d", inc->msg.u.call.prog, inc->msg.u.call.vers, inc->msg.u.call.proc );

    /* lookup function */
    sts = rpc_program_find( inc->msg.u.call.prog, inc->msg.u.call.vers, inc->msg.u.call.proc,
			    &p, &v, &pc );
    if( sts ) {
      if( !p ) {
	rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_PROG_UNAVAIL, NULL, &handle );
	return 0;
      }

      if( !v ) {
	struct rpc_mismatch mm;
	mm.low = 0;
	mm.high = 0;
	v = p->vers;
	while( v ) {
	  if( (mm.low == 0) || (v->vers < mm.low) ) mm.low = v->vers;
	  if( (mm.high == 0) || (v->vers > mm.high) ) mm.high = v->vers;
	  v = v->next;
	}
	rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_PROG_MISMATCH, &mm, &handle );
	return 0;
      }

      if( !pc ) {
	rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_PROC_UNAVAIL, NULL, &handle );
	return 0;
      }
      return -1;
    }

    /* authenticate */
    inc->pvr = rpc_provider_by_flavour( inc->msg.u.call.auth.flavour );
    if( inc->pvr ) {
      rpc_log( RPC_LOG_INFO, "AUTH %d", inc->msg.u.call.auth.flavour );
    
      sts = inc->pvr->sauth( inc->pvr, &inc->msg, &inc->pcxt );
      if( sts ) {
	rpc_init_reject_reply( inc, inc->msg.xid, sts < 0 ? RPC_AUTH_ERROR_TOOWEAK : sts );
	return 0;
      }
    }

    /* allow provider to modify arguments */
    start = inc->xdr.offset;
    if( inc->pvr && inc->pvr->smargs ) {
      sts = inc->pvr->smargs( inc->pvr, &inc->xdr, start, inc->xdr.count, inc->pcxt );
      if( sts ) {
	rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
	return 0;
      }
    }

    /* invoke handler function */
    sts = pc->fn( inc );
    if( sts < 0 ) {
      rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SYSTEM_ERROR, NULL, &handle );
      return 0;
    } 

    return sts;
    break;
  case RPC_REPLY:
    /* check for a waiter or drop */
    rpc_waiter_invoke( inc->msg.xid, inc );
    break;
  }
    
  return -1;
}


int rpc_process_reply( struct rpc_inc *inc ) {
  int sts;
  
  sts = rpc_decode_msg( &inc->xdr, &inc->msg );
  if( sts ) return sts;

  if( inc->msg.tag != RPC_REPLY ) return -1;
  if( inc->msg.u.reply.tag != RPC_MSG_ACCEPT ) return -1;
  if( inc->msg.u.reply.u.accept.tag != RPC_ACCEPT_SUCCESS ) return -1;
  
  if( inc->pvr ) {
    sts = inc->pvr->cverf( inc->pvr, &inc->msg, inc->pcxt );
    if( sts ) return sts;

    if( inc->pvr->cmres ) {
      sts = inc->pvr->cmres( inc->pvr, &inc->xdr, inc->xdr.offset, inc->xdr.count, inc->pcxt );
      if( sts ) return sts;
    }
  }
  
  return 0;
}




static int rpcbind_encode_mapping( struct xdr_s *xdr, struct rpcbind_mapping *m ) {
  int sts;
  sts = xdr_encode_uint32( xdr, m->prog );
  if( sts ) return sts;
  sts = xdr_encode_uint32( xdr, m->vers );
  if( sts ) return sts;
  sts = xdr_encode_uint32( xdr, m->prot );
  if( sts ) return sts;
  sts = xdr_encode_uint32( xdr, m->port );
  if( sts ) return sts;
  return 0;
}

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

static int rpcbind_encode_mapping_list( struct xdr_s *xdr, struct rpcbind_mapping *mlist, int n ) {
  int sts;
  int i;

  for( i = 0; i < n; i++ ) {
    sts = xdr_encode_boolean( xdr, 1 );
    if( sts ) return sts;
    sts = rpcbind_encode_mapping( xdr, &mlist[i] );
    if( sts ) return sts;
  }
  sts = xdr_encode_boolean( xdr, 0 );
  if( sts ) return sts;

  return 0;
}


/* ----------------------rpcbind version2 ----------------------- */

static int bind_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static int bind_set( struct rpc_inc *inc ) {
  rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  return 0;
}

static int bind_unset( struct rpc_inc *inc ) {
  rpc_init_reject_reply( inc, inc->msg.xid, RPC_AUTH_ERROR_TOOWEAK );
  return 0;
}
static int bind_getport( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_uint32( &inc->xdr, ntohs( ((struct sockaddr_in *)&inc->laddr)->sin_port ) );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct {
  int prot;
  int port;
} bind_laddrs[16];

void rpcbind_set_laddrs( int *prot_port, int n ) {
  int i;

  if( n > 15 ) n = 15;
  if( n < 0 ) n = 0;
  for( i = 0; i < n; i++ ) {
    bind_laddrs[i].prot = (prot_port[i] >> 16) & 0xffff;
    bind_laddrs[i].port = (prot_port[i]) & 0xffff;
  }
  bind_laddrs[n].prot = 0;
  bind_laddrs[n].port = 0;
}

static int bind_list( struct rpc_inc *inc ) {
  struct rpcbind_mapping mlist[64];
  int i, j;
  struct rpc_program *p;
  struct rpc_version *v;
  struct sockaddr_in laddr;
  int lport, handle;

  memset( &laddr, 0, sizeof(laddr) );
  if( inc->laddr_len == sizeof(laddr) ) memcpy( &laddr, &inc->laddr, sizeof(laddr) );
  lport = ntohs( laddr.sin_port );

  i = 0;
  p = rpc_program_list();
  while( p ) {
    v = p->vers;
    while( v ) {
      /* walk list of laddrs */
      for( j = 0; j < 16; j++ ) {
	if( bind_laddrs[j].port == 0 ) break;

	if( i < 64 ) {
	  mlist[i].prog = p->prog;
	  mlist[i].vers = v->vers;
	  mlist[i].prot = bind_laddrs[j].prot;
	  mlist[i].port = bind_laddrs[j].port;
	  i++;
	}
      }

      v = v->next;
    }
    p = p->next;
  }

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpcbind_encode_mapping_list( &inc->xdr, mlist, i > 64 ? 64 : i );
  rpc_complete_accept_reply( inc, handle );
    
  return 0;
}


static struct rpc_proc bind_procs[] = {
  { 0, bind_null },
  { 1, bind_set },
  { 2, bind_unset },
  { 3, bind_getport },
  { 4, bind_list },
  { 0, NULL }
};

static struct rpc_version bind_vers = {
  NULL, 2, bind_procs
};

static struct rpc_program bind_prog = {
  NULL, 100000, &bind_vers
};


int rpcbind_register( void ) {
  rpc_program_register( &bind_prog );
  return 0;
}

/* ----------------------------------------------------------------- */

int rpc_call_udp( struct rpc_inc *inc ) {
#ifdef WIN32
  SOCKET fd;
#else
  int fd;
  struct pollfd pfd[1];
#endif
  int sts;
  struct sockaddr_in sin;
  
  fd = socket( AF_INET, SOCK_DGRAM, 0 );
  if( fd < 0 ) return -1;
    
  memset( &sin, 0, sizeof(sin) );
  sin.sin_family = AF_INET;
  sts = bind( fd, (struct sockaddr *)&sin, sizeof(sin) );
  if( sts < 0 ) goto done;

  sts = sendto( fd, inc->xdr.buf, inc->xdr.offset, 0,
		(struct sockaddr *)&inc->raddr, inc->raddr_len );
  if( sts < 0 ) goto done;

#ifdef WIN32
  {
    HANDLE evt;
    WSANETWORKEVENTS events;
    
    evt = WSACreateEvent();
    WSAEventSelect( fd, evt, FD_READ );
    WSAWaitForMultipleEvents( 1, &evt, TRUE, 5000, FALSE );
    WSAEnumNetworkEvents( fd, evt, &events );
    WSACloseEvent( evt );
  }
#else
  pfd[0].fd = fd;
  pfd[0].events = POLLIN;
  pfd[0].revents = 0;
  sts = poll( pfd, 1, 5000 );
  if( sts != 1 ) {
    sts = -1;
    goto done;
  }
  if( !(pfd[0].revents & POLLIN) ) {
    sts = -1;
    goto done;
  }
#endif
  
  sts = recvfrom( fd, inc->xdr.buf, inc->xdr.buf_size, 0,
		  (struct sockaddr *)&inc->raddr, &inc->raddr_len );
  if( sts < 0 ) goto done;
    
  inc->xdr.offset = 0;
  inc->xdr.count = sts;
    
  sts = 0;
 done:
#ifdef WIN32
  closesocket( fd );
#else
  close( fd );
#endif
    
  return sts;
}

int rpc_call_tcp( struct rpc_inc *inc ) {
#ifdef WIN32
  SOCKET fd;
#else
  int fd;
#endif
  int sts;
  struct sockaddr_in sin;
  int offset;
  uint8_t lbuf[4];
  struct xdr_s tmpx;
  uint32_t len;
  
  fd = socket( AF_INET, SOCK_STREAM, 0 );
  if( fd < 0 ) return -1;
    
  memset( &sin, 0, sizeof(sin) );
  sin.sin_family = AF_INET;
  sts = bind( fd, (struct sockaddr *)&sin, sizeof(sin) );
  if( sts < 0 ) goto done;
  sts = connect( fd, (struct sockaddr *)&inc->raddr, sizeof(struct sockaddr_in) );
  if( sts < 0 ) goto done;

  xdr_init( &tmpx, lbuf, 4 );
  xdr_encode_uint32( &tmpx, inc->xdr.offset | 0x80000000 );
  sts = send( fd, lbuf, 4, 0 );
    
  offset = 0;
  while( offset < inc->xdr.offset ) {
    sts = send( fd, inc->xdr.buf, inc->xdr.offset, 0 );
    if( sts < 0 ) goto done;
    offset += sts;
  }
    
  sts = recv( fd, lbuf, 4, 0 );
  if( sts < 0 ) goto done;
    
  xdr_init( &tmpx, lbuf, 4 );
  xdr_decode_uint32( &tmpx, &len );
  if( !(len & 0x80000000) ) {
    sts = -1;
    goto done;
  }
    
  len &= ~0x80000000;
    
  if( (int)len > inc->xdr.buf_size ) {
    sts = -1;
    goto done;
  }
    
  xdr_reset( &inc->xdr );

  offset = 0;
  while( offset < (int)len ) {
    sts = recv( fd, inc->xdr.buf + offset, len - offset, 0 );
    if( sts < 0 ) {
#ifdef WIN32
      sts = GetLastError();
      if( sts == WSAEWOULDBLOCK ) continue;
      else goto done;
#else
      if( errno == EAGAIN ) continue;
      else if( errno == EINTR ) continue;
      else goto done;
#endif       
    }
    offset += sts;
  }
  inc->xdr.offset = 0;
  inc->xdr.count = len;
    
  sts = 0;
 done:
#ifdef WIN32
  closesocket( fd );
#else
  close( fd );
#endif
    
  return sts;
}


int rpcbind_call_dump( struct sockaddr_in *addr, struct rpcbind_mapping *mlist, int n ) {
  struct rpc_inc inc;
  uint8_t *buf;
  int sts, handle;
       
  buf = malloc( 64 * 1024 );

  memset( &inc, 0, sizeof(inc) );
  xdr_init( &inc.xdr, buf, 64 * 1024 );

  rpc_init_call( &inc, 100000, 2, 4, &handle );
  memcpy( &inc.raddr, addr, sizeof(struct sockaddr_in) );
  inc.raddr_len = sizeof(struct sockaddr_in);
    
  sts = rpc_call_tcp( &inc );
  if( sts ) goto done;
  
  sts = rpc_process_reply( &inc );
  if( sts ) goto done;
  
  /* decode result from xdr */
  sts = rpcbind_decode_mapping_list( &inc.xdr, mlist, n );
  if( sts ) goto done;
  
 done:
  free( buf );
    
  return sts;
}

int rpcbind_call_set( struct sockaddr_in *addr, struct rpcbind_mapping *m ) {
  struct rpc_inc inc;
  uint8_t *buf;
  int sts, handle;
       
  buf = malloc( 64 * 1024 );

  memset( &inc, 0, sizeof(inc) );
  xdr_init( &inc.xdr, buf, 64 * 1024 );

  rpc_init_call( &inc, 100000, 2, 1, &handle );
  rpcbind_encode_mapping( &inc.xdr, m );

  memcpy( &inc.raddr, addr, sizeof(struct sockaddr_in) );
  inc.raddr_len = sizeof(struct sockaddr_in);

  sts = rpc_call_udp( &inc );
  //  sts = rpc_call_tcp( &inc );

  free( buf );
    
  return sts;
}

uint64_t rpc_now( void ) {
#ifdef WIN32
  return GetTickCount64();
#else
  struct timespec tm;
  clock_gettime( CLOCK_MONOTONIC, &tm );
  return (uint64_t)((tm.tv_sec * 1000ULL) + (tm.tv_nsec / (1000ULL * 1000ULL)));
#endif
}


static struct rpc_iterator *itlist;

void rpc_iterator_register( struct rpc_iterator *it ) {
  it->next = itlist;
  itlist = it;
}

void rpc_iterator_unregister( struct rpc_iterator *it ) {
  struct rpc_iterator *i, *prev;

  prev = NULL;
  i = itlist;
  while( i ) {
    if( i == it ) {
      if( prev ) prev->next = i->next;
      else itlist = i->next;
      break;
    }

    prev = i;
    i = i->next;
  }
  
}

void rpc_iterator_service( void ) {
  struct rpc_iterator *it;
  uint64_t now;

  now = rpc_now();
  it = itlist;
  while( it ) {
    if( (it->timeout == 0) || (now >= it->timeout) ) {
      it->timeout = now + it->period;
      it->cb( it );
    }
    it = it->next;
  }

}

int rpc_iterator_timeout( void ) {
  struct rpc_iterator *it;
  uint64_t now;
  int timeout;


  timeout = -1;
  it = itlist;
  now = rpc_now();
  while( it ) {
    if( (it->timeout == 0) || (now >= it->timeout) ) {
      timeout = 0;
      break;
    } else if( (int)(now - it->timeout) > timeout ) {
      timeout = (int)(now - it->timeout);
    }

    it = it->next;
  }

  return timeout;
}

static void rpc_default_log( int lvl, char *fmt, va_list args ) {
  char timestr[64];
  struct tm *tm;
  time_t now;
  
  now = time( NULL );
  tm = localtime( &now );
  strftime( timestr, sizeof(timestr), "%Y-%m-%d %H:%M:%S", tm );
  printf( "%s %d ", timestr, lvl );
  vprintf( fmt, args );  
  printf( "\n" );  
}

static struct rpc_logger *loggers;
void rpc_add_logger( struct rpc_logger *logger ) {
  logger->next = loggers;
  if( !logger->cb ) logger->cb = rpc_default_log;
  loggers = logger;
}

void rpc_log( int lvl, char *fmt, ... ) {
  va_list args;
  struct rpc_logger *lg;
  
  lg = loggers;
  while( lg ) {
    va_start( args, fmt );
    lg->cb( lvl, fmt, args );
    va_end( args );
    lg = lg->next;
  }
}

/* ------------------------------ */

static struct rpc_waiter *waiter_list;
int rpc_await_reply( struct rpc_waiter *waiter ) {
  struct rpc_waiter *w;

  /* check this waiter is not already registered */
  w = waiter_list;
  while( w ) {
    if( w == waiter ) return -1;
    w = w->next;
  }

  /* push onto list */
  waiter->next = waiter_list;
  waiter_list = waiter;

  return 0;
}

int rpc_waiter_invoke( uint32_t xid, struct rpc_inc *inc ) {
  struct rpc_waiter *w, *prev;
  w = waiter_list;
  prev = NULL;
  while( w ) {
    if( w->xid == xid ) {
      /* pop from list */
      if( prev ) prev->next = w->next;
      else waiter_list = w->next;

      /* invoke callback and return */
      w->cb( w, inc );
      return 0;
    }
    prev = w;
    w = w->next;
  }
  
  return -1;
}

void rpc_waiter_service( void ) {
  struct rpc_waiter *w, *next, *prev;
  struct rpc_waiter *timedout;
  uint64_t now;

  now = rpc_now();
  w = waiter_list;
  prev = NULL;
  timedout = NULL;
  
  while( w ) {
    next = w->next;
    if( w->timeout <= now ) {
      if( prev ) prev->next = next;
      else waiter_list = next;

      /* defer timeout callbacks to later */
      w->next = timedout;
      timedout = w;
      
      /* move onto next */
      w = next;
    } else {
      prev = w;
      w = next;
    }
  }
  
  /* 
   * invoke all timed out waiters now. This is because 
   * the callback may call rpc_await_reply and thereby modify 
   * the waiter list. doing it this way makes all this work without 
   * any odd bugs.
   */
  w = timedout;
  while( w ) {
    next = w->next;
    w->cb( w, NULL );
    w = next;
  }

}


  
int rpc_errno( void ) {
#ifdef WIN32
    return (int)WSAGetLastError();
#else
    return errno;
#endif
}

char *rpc_strerror( int sts ) {
    static char buf[256];
#ifdef WIN32
    FormatMessageA( FORMAT_MESSAGE_FROM_SYSTEM, NULL, sts, 0, buf, 256, NULL );
#else
    strcpy( buf, strerror( sts ) );
#endif
    return buf;
}
