
#ifndef SEC_H
#define SEC_H

#include <stdint.h>

#define SEC_MAX_KEYBUF 128 
struct sec_buf {
  int len;
  char *buf;
};

#define SEC_ECDH_MAX_PRIVKEY 96
#define SEC_ECDH_MAX_PUBKEY  64 
#define SEC_ECDH_MAX_COMMON  20
int ecdh_generate( struct sec_buf *local_priv, struct sec_buf *local_public );
int ecdh_common( struct sec_buf *local_priv, struct sec_buf *remote_public, struct sec_buf *common );

#define SEC_SHA1_MAX_HASH 20
void sha1( uint8_t *hash, struct sec_buf *iov, int n );
void sha1_hmac( uint8_t *hash, uint8_t *key, char *buf, int len );

#define SEC_AES_MAX_KEY 16
void aes_encrypt( uint8_t *key, uint8_t *buf, int n );
void aes_decrypt( uint8_t *key, uint8_t *buf, int n );

void sec_rand( void *buf, int n );

#endif
