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
 
#ifndef SEC_H
#define SEC_H

#include <stdint.h>

#define SEC_MAX_KEYBUF 128 
struct sec_buf {
  int len;
  char *buf;
};
void sec_buf_init( struct sec_buf *sbuf, char *buf, int len );

#define SEC_ECDH_MAX_PRIVKEY 32
#define SEC_ECDH_MAX_PUBKEY  64 
#define SEC_ECDH_MAX_COMMON  20
int ecdh_generate( struct sec_buf *local_priv, struct sec_buf *local_public );
int ecdh_common( struct sec_buf *local_priv, struct sec_buf *remote_public, struct sec_buf *common );

#define SEC_SHA1_MAX_HASH 20
void sha1( uint8_t *hash, struct sec_buf *iov, int n );
void sha1_hmac( uint8_t *hash, uint8_t *key, struct sec_buf *iov, int n );

#define SEC_AES_MAX_KEY 16
void aes_encrypt( uint8_t *key, uint8_t *buf, int n );
void aes_decrypt( uint8_t *key, uint8_t *buf, int n );

void sec_rand( void *buf, int n );

#endif
