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

#ifndef HRAUTH_H
#define HRAUTH_H

#include <rpc.h>
#include <rpcd.h>

#define RPC_AUTH_HRAUTH 0x27E1DAF0
#define HRAUTH_RPC_PROG  0x27E1FAF0
#define HRAUTH_RPC_VERS  1

struct hrauth_context {
  uint64_t remoteid;                /* remote host id */  
  uint8_t key[32];                  /* common secret */
  uint8_t session_key[32];          /* ephemeral session key */
  int service;                      /* service level */
#define HRAUTH_SERVICE_NONE   0     /* just authentication */
#define HRAUTH_SERVICE_INTEG  1     /* arg/result checksumming */
#define HRAUTH_SERVICE_PRIV   2     /* checksumming and encryption */
  int window;                       /* acceptable clock skew */
  uint32_t cipher;                  /* cipher suite. we want to be able to extend in future */
#define HRAUTH_CIPHER_MASK    0x0000ffff    /* mask off the low 16 bits to get the cipher to use */
#define HRAUTH_CIPHER_AES128  0x00000001    /* AES-128 */
#define HRAUTH_DIGEST_MASK    0xffff0000    /* mask off the high 16 bits to get the digest to use for hmacs */
#define HRAUTH_DIGEST_SHA1    0x00010000    /* SHA1 */
  uint32_t nickname;                /* authentication nickname */
  uint32_t timestamp;               /* timestamp we sent */
};

  /* encrypted and sent in the auth structure */
struct hrauth_cred {
  uint32_t nonce;
  uint8_t session_key[32];
  uint32_t service;
  uint32_t window;
  uint32_t cipher;
};

struct hrauth_auth {
  int tag;
#define HRAUTH_NICKNAME 0
#define HRAUTH_FULL     1
  union {
    uint32_t nickname;
    struct {
      uint64_t id;
      uint32_t cipher;
      uint8_t ecred[64];   /* encrypted cred structure */
    } full;
  } u;
};

/* verifier structure - encrypted with session key */
struct hrauth_verf {
  uint32_t nonce;      /* random number */
  uint32_t timestamp;  /* current time */
  uint32_t tverf;      /* timestamp - 1 */
  uint32_t nickname;   /* context nickname */
};

int hrauth_init( struct hrauth_context *cxt, uint64_t remoteid );
struct rpc_provider *hrauth_provider( void );
int hrauth_priv_header( struct hrauth_context *sa, struct xdr_s *xdr, int start, int end );
void hrauth_register( void );

struct hrauth_call {
  uint64_t hostid;
  uint32_t prog;
  uint32_t vers;
  uint32_t proc;
  void (*donecb)( struct xdr_s *res, void *cxt );
  void *cxt;
  int timeout;
  int service;
};

struct hrauth_call_opts {
  uint32_t mask;
#define HRAUTH_CALL_OPT_FD           0x0001
#define HRAUTH_CALL_OPT_TMPBUF       0x0002
#define HRAUTH_CALL_OPT_PORT         0x0004
#define HRAUTH_CALL_OPT_ADDRMASK     0x0008
#ifdef WIN32
  SOCKET fd;
#else
  int fd;
#endif
  struct xdr_s tmpbuf;
  int port;
  uint32_t addrmask;
};
int hrauth_call_udp_async( struct hrauth_call *hcall, struct xdr_s *args, struct hrauth_call_opts *opts );

//int hrauth_call_tcp_async( struct hrauth_call *hcall, struct xdr_s *args );

int hrauth_call_udp_proxy( struct rpc_inc *inc, uint64_t hostid, struct xdr_s *args );

/* synchronous rpc call */
int hrauth_call_udp( struct hrauth_call *hcall, struct xdr_s *args, struct xdr_s *res, struct hrauth_call_opts *opts );
int hrauth_call_tcp( struct hrauth_call *hcall, struct xdr_s *args, struct xdr_s *res, struct hrauth_call_opts *opts );

#endif

