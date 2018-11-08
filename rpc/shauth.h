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

#ifndef SHAUTH_H
#define SHAUTH_H

#include "rpc.h"

#define RPC_AUTH_SHAUTH 100334

struct shauth_context {
  uint8_t key[32];                  /* shared secret */
  uint8_t session_key[32];          /* session key */
  int service;                      /* service level */
#define SHAUTH_SERVICE_NONE   0     /* just authentication */
#define SHAUTH_SERVICE_INTEG  1     /* arg/result checksumming */
#define SHAUTH_SERVICE_PRIV   2     /* checksumming and encryption */
  int window;                       /* acceptable clock skew */
  uint32_t cipher;                  /* cipher suite. we want to be able to extend in future */
#define SHAUTH_CIPHER_MASK    0x0000ffff    /* mask off the low 16 bits to get the cipher to use */
#define SHAUTH_CIPHER_AES128  0x00000001    /* AES-128 */
#define SHAUTH_DIGEST_MASK    0xffff0000    /* mask off the high 16 bits to get the digest to use for hmacs */
#define SHAUTH_DIGEST_SHA1    0x00010000    /* SHA1 */
  uint32_t nickname;                /* authentication nickname */
  uint32_t timestamp;               /* timestamp we sent */
};

  /* encrypted and sent in the auth structure */
struct shauth_cred {
  uint32_t nonce;
  uint8_t session_key[32];
  uint32_t service;
  uint32_t window;
  uint32_t cipher;
};

struct shauth_auth {
  int tag;
#define SHAUTH_NICKNAME 0
#define SHAUTH_FULL     1
  union {
    uint32_t nickname;
    struct {
      uint32_t cipher;
      uint8_t ecred[64];   /* encrypted cred structure */
    } full;
  } u;
};

/* verifier structure - encrypted with session key */
struct shauth_verf {
  uint32_t nonce;      /* random number */
  uint32_t timestamp;  /* current time */
  uint32_t tverf;      /* timestamp - 1 */
  uint32_t nickname;   /* context nickname */
};

void shauth_init( struct shauth_context *cxt, uint8_t *key );
struct rpc_provider *shauth_provider( void );
void shauth_set_shared( uint8_t *key );
int shauth_priv_header( struct shauth_context *sa, struct xdr_s *xdr, int start, int end );
void shauth_register( uint8_t *key );

#endif

