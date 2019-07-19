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

#ifndef RPC_H
#define RPC_H

#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <WinSock2.h>
#include <Windows.h>
#include <ws2def.h>
#else
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdint.h>

#define RPC_LOG_INFO  1
#define RPC_LOG_DEBUG 1
#define RPC_LOG_ERROR 2
#define RPC_LOG_WARN  2


struct xdr_s {
  uint8_t *buf;
  int buf_size;
    
  int count;
  int offset;    
};
void xdr_init( struct xdr_s *xdr, uint8_t *buf, int size );
void xdr_reset( struct xdr_s *xdr );

int xdr_encode_uint32( struct xdr_s *xdr, uint32_t x );
int xdr_encode_uint64( struct xdr_s *xdr, uint64_t x );
int xdr_decode_uint32( struct xdr_s *xdr, uint32_t *x );
int xdr_decode_uint64( struct xdr_s *xdr, uint64_t *x );
int xdr_encode_int32( struct xdr_s *xdr, int32_t x );
int xdr_encode_int64( struct xdr_s *xdr, int64_t x );
int xdr_decode_int32( struct xdr_s *xdr, int32_t *x );
int xdr_decode_int64( struct xdr_s *xdr, int64_t *x );
int xdr_encode_boolean( struct xdr_s *xdr, int x );
int xdr_decode_boolean( struct xdr_s *xdr, int *x );
int xdr_encode_string( struct xdr_s *xdr, char *str );
int xdr_decode_string( struct xdr_s *xdr, char *str, int n );
int xdr_encode_fixed( struct xdr_s *xdr, uint8_t *buf, int n );
int xdr_decode_fixed( struct xdr_s *xdr, uint8_t *buf, int n );
int xdr_encode_opaque( struct xdr_s *xdr, uint8_t *buf, int n );
int xdr_decode_opaque( struct xdr_s *xdr, uint8_t *buf, int *n );
int xdr_decode_opaque_ref( struct xdr_s *xdr, uint8_t **buf, int *n );

#define RPC_MAX_OPAQUE_AUTH      400

typedef enum {
  RPC_AUTH_NULL            = 0,
  RPC_AUTH_UNIX            = 1,
  RPC_AUTH_SHORT           = 2,
  RPC_AUTH_DES             = 3,
  RPC_AUTH_KERB4           = 4,
  RPC_AUTH_RSA             = 5,
  RPC_AUTH_GSS             = 6,
  RPC_AUTH_SPNEGO          = 390000,
  RPC_AUTH_KRB5            = 390003,
  RPC_AUTH_KRB5_INTEGRITY  = 390004,
  RPC_AUTH_KRB5_PRIVACY    = 390005,
} rpc_auth_t;
struct rpc_opaque_auth {
  rpc_auth_t flavour;
  uint32_t len;
  uint8_t data[RPC_MAX_OPAQUE_AUTH];
};

struct rpc_mismatch {
  uint32_t low;
  uint32_t high;
};

typedef enum {
  RPC_CALL = 0,
  RPC_REPLY = 1,
} rpc_msg_t;
typedef enum {
  RPC_MSG_ACCEPT = 0,
  RPC_MSG_REJECT = 1,
} rpc_msg_accept_t;
typedef enum {
  RPC_ACCEPT_SUCCESS       = 0,
  RPC_ACCEPT_PROG_UNAVAIL  = 1,
  RPC_ACCEPT_PROG_MISMATCH = 2,
  RPC_ACCEPT_PROC_UNAVAIL  = 3,
  RPC_ACCEPT_GARBAGE_ARGS  = 4,
  RPC_ACCEPT_SYSTEM_ERROR  = 5,
} rpc_accept_t;
typedef enum { 
  RPC_REJECT_RPCMISMATCH    = 0,
  RPC_REJECT_AUTH_ERROR     = 1,
} rpc_reject_t;
typedef enum {
  RPC_AUTH_ERROR_BADCRED      = 1,
  RPC_AUTH_ERROR_REJECTED     = 2,
  RPC_AUTH_ERROR_BADVERF      = 3,
  RPC_AUTH_ERROR_REJECTEDVERF = 4,
  RPC_AUTH_ERROR_TOOWEAK      = 5,
} rpc_auth_error_t;

struct rpc_msg {
  uint32_t xid;
  rpc_msg_t tag;
  union {
    struct {
      uint32_t rpcvers;
#define RPC_VERS 2
      uint32_t prog;
      uint32_t vers;
      uint32_t proc;
      struct rpc_opaque_auth auth;
      struct rpc_opaque_auth verf;
    } call;
    struct {
      rpc_msg_accept_t tag;
      union {
	struct {
	  struct rpc_opaque_auth verf;
	  rpc_accept_t tag;
	  union {
	    struct rpc_mismatch mismatch;
	  } u;
	} accept;
	struct {
	  rpc_reject_t tag;
	  union {
	    struct rpc_mismatch mismatch;
	    rpc_auth_error_t auth_error;
	  } u;
	} reject;
      } u;
    } reply;
  } u;
};
int rpc_encode_msg( struct xdr_s *xdr, struct rpc_msg *x );
int rpc_decode_msg( struct xdr_s *xdr, struct rpc_msg *x );


struct rpc_provider;
struct rpc_inc {
  struct xdr_s xdr;              /* rxtx buffer */
  struct rpc_msg msg;            /* rpc msg header as decoded or to reply with */
  struct rpc_provider *pvr;      /* authentication provider */
  void *pcxt;                    /* authentication context */
    
  /* remote address */
#ifdef WIN32
  SOCKADDR_STORAGE raddr;
#else
  struct sockaddr_storage raddr;
#endif
  unsigned int raddr_len;
    
  /* local address */
#ifdef WIN32
  SOCKADDR_STORAGE laddr;
#else
  struct sockaddr_storage laddr;
#endif
  unsigned int laddr_len;
};

/* rpc procedure version and program definitions */
typedef int (*rpc_proc_t)( struct rpc_inc *inc );
struct rpc_proc {
  uint32_t proc;
  rpc_proc_t fn;
};
struct rpc_version {
  struct rpc_version *next;
  uint32_t vers;
  struct rpc_proc *procs;
};
struct rpc_program {
  struct rpc_program *next;
  uint32_t prog;
  struct rpc_version *vers;
};
void rpc_program_register( struct rpc_program *p );
void rpc_program_unregister( struct rpc_program *p );
int rpc_program_find( uint32_t prog, uint32_t vers, uint32_t proc,
                      struct rpc_program **p,
                      struct rpc_version **v,
                      struct rpc_proc **pc );
struct rpc_program *rpc_program_list( void );


/* authentication provider definition */
typedef int (*rpc_cauth_t)( struct rpc_provider *pvr, struct rpc_msg *msg, void *pcxt );
typedef int (*rpc_cverf_t)( struct rpc_provider *pvr, struct rpc_msg *msg, void *pcxt );
typedef int (*rpc_taste_t)( struct rpc_provider *pvr, int32_t flavour );
typedef int (*rpc_sauth_t)( struct rpc_provider *pvr, struct rpc_msg *msg, void **pcxt );
typedef int (*rpc_modify_t)( struct rpc_provider *pvr, struct xdr_s *xdr, int start, int end, void *pcxt );
struct rpc_provider {
  struct rpc_provider *next;
  int32_t flavour;
    
  rpc_cauth_t cauth;
  rpc_cverf_t cverf;
  rpc_modify_t cmargs;
  rpc_modify_t cmres;
    
  rpc_taste_t taste;
  rpc_sauth_t sauth;
  rpc_modify_t smargs;
  rpc_modify_t smres;
    
  struct rpc_opaque_auth rverf;
};
void rpc_provider_register( struct rpc_provider *p );
void rpc_provider_unregister( struct rpc_provider *p );
struct rpc_provider *rpc_provider_by_flavour( int32_t flavour );


int rpc_init_call( struct rpc_inc *inc, int32_t prog, uint32_t vers, uint32_t proc, int *handle );
int rpc_complete_call( struct rpc_inc *inc, int handle );
int rpc_init_accept_reply( struct rpc_inc *inc, uint32_t xid, int accept_stat, struct rpc_mismatch *mm, int *handle );
int rpc_complete_accept_reply( struct rpc_inc *inc, int handle );
int rpc_init_reject_reply( struct rpc_inc *inc, uint32_t xid, int32_t auth_stat );
int rpc_process_reply( struct rpc_inc *inc );
int rpc_process_incoming( struct rpc_inc *inc );

struct rpcbind_mapping {
  uint32_t prog;
  uint32_t vers;
  uint32_t prot;
  uint32_t port;
};
int rpcbind_register( void );
int rpcbind_call_dump( struct sockaddr_in *addr, struct rpcbind_mapping *mlist, int n );
int rpcbind_call_set( struct sockaddr_in *addr, struct rpcbind_mapping *m );
void rpcbind_set_laddrs( int *prot_port, int n );

int rpc_call_tcp( struct rpc_inc *inc );

int rpc_call_udp( struct rpc_inc *inc );

struct rpc_call_opts {
	uint32_t mask;
#define RPC_CALL_OPT_TIMEOUT 0x0001 
#define RPC_CALL_OPT_FD      0x0002 
	int timeout;
#ifdef WIN32
	UINT_PTR fd;
#else
	int fd;
#endif
};
int rpc_call_udp2( struct rpc_inc *inc, struct rpc_call_opts *opts );

struct rpc_iterator;
typedef void (*rpc_iterator_t)( struct rpc_iterator *it );
struct rpc_iterator {
  struct rpc_iterator *next;
  uint64_t timeout;
  int period;
  rpc_iterator_t cb;
  void *cxt;
};
void rpc_iterator_register( struct rpc_iterator *it );
void rpc_iterator_unregister( struct rpc_iterator *it );
int rpc_iterator_timeout( void );
void rpc_iterator_service( void );

uint64_t rpc_now( void );
void rpc_log( int lvl, char *fmt, ... );

struct rpc_logger {
  struct rpc_logger *next;
  void (*cb)( int lvl, char *fmt, va_list args );
};
void rpc_add_logger( struct rpc_logger *logger );

struct rpc_waiter;
typedef void (*rpc_waiter_t)( struct rpc_waiter *w, struct rpc_inc *inc );
struct rpc_waiter {
  struct rpc_waiter *next;         /* list pointer */
  
  uint32_t xid;                    /* xid of message exchange*/
  uint64_t timeout;                /* rpc_now()+milliseconds to wait */
 
  rpc_waiter_t cb;                 /* reply or timeout callback */
  void *cxt;                       /* private data */
  
  struct rpc_provider *pvr;        /* authentication provider to use */
  void *pcxt;                      /* authentication data */
};

/* 
 * caller allocates waiter and ensures it lives until the callback is invoked. 
 * The callback will be invoked either when the reply is recieved or the timeout expires.
 */
int rpc_await_reply( struct rpc_waiter *waiter );
int rpc_waiter_invoke( uint32_t xid, struct rpc_inc *inc );
void rpc_waiter_service( void );

int rpc_errno( void );
char *rpc_strerror( int sts );

#endif

