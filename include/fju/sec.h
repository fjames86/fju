 
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
uint32_t sec_rand_uint32( void );

char *sec_timestr( uint64_t now, char *str );

struct sec_shamir_share {
  uint32_t flags;
#define SEC_SHAMIR_XVAL  0x0001 
  uint8_t xval;
  uint8_t spare[3];
  uint8_t *sharebuf;
};
int sec_shamir_split( uint8_t *secret, int secretlen, struct sec_shamir_share *share, int nshares, int k );
int sec_shamir_join( uint8_t *secret, int secretlen, struct sec_shamir_share *share, int nshares );

/* Set crc=0xffffffff on first use, pass result in to continue */
uint32_t sec_crc32( uint32_t crc, char *buf, int len );

int fju_readstdin( char *buf, int size );
int fju_writestdout( char *buf, int size );

#if 0
void base32_encode( char *plain, int len, char *coded );
int base32_decode( char *coded, char *plain );
#endif

/* str must be a least 4*((buflen / 3) + (buflen % 3 ? 1 : 0)) + 1 bytes long */
int base64_encode( char *buf, int buflen, char *str );
int base64_decode( char *buf, int buflen, char *str );

#define SEC_MAX_SIG 72 
int sec_sign( struct sec_buf *privkey, struct sec_buf *pubkey, struct sec_buf *dataiov, int niov, struct sec_buf *sig );
int sec_verify( struct sec_buf *pubkey, struct sec_buf *dataiov, int niov, struct sec_buf *sig );

#endif
