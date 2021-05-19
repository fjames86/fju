
/*
 * ef - Encrypted File utility
 * Written by Frank James <frank.a.james@gmail.com>
 *
 * Copyright 2018 Frank James 
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy 
 * of this software and associated documentation files (the "Software"), 
 * to deal in the Software without restriction, including without limitation 
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, 
 * and/or sell copies of the Software, and to permit persons to whom the 
 * Software is furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included 
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#define _XOPEN_SOURCE 500

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <openssl/evp.h>
#include <openssl/conf.h>
#include <openssl/err.h>
#include <openssl/sha.h>
#include <openssl/opensslv.h>
#include <sys/mman.h>


#include "ef.h"
#include "fastpbkdf2.h"

#define EF_KEYLEN 16

#if ((OPENSSL_VERSION_NUMBER & 0x0ff00000) >= 0x00100000)
#define EF_OPENSSL_1_1
#endif


int ef_genkey( uint8_t *key, int keylen ) {
  int fd;
  int n;
  
  fd = open( "/dev/urandom", O_RDONLY, 0600 );
  if( fd < 0 ) return -1;

  n = read( fd, key, keylen );

  close( fd );

  return n;
}


static void init_crypto( void ) {
  static int initialized = 0;
  if( !initialized ) {
    ERR_load_crypto_strings();
    OpenSSL_add_all_algorithms();

#ifdef EF_OPENSSL_1_1
    OPENSSL_init_crypto( OPENSSL_INIT_ADD_ALL_CIPHERS|
			 OPENSSL_INIT_ADD_ALL_DIGESTS |
			 OPENSSL_INIT_LOAD_CRYPTO_STRINGS,
			 NULL );
#else
    OPENSSL_config( NULL );
#endif
    initialized = 1;
  }
}

int ef_pbkdf2( char *password, char *salt, uint8_t *key, int keylen ) {
  init_crypto();
  fastpbkdf2_hmac_sha1( (uint8_t *)password, strlen( password ),
			(uint8_t *)(salt ? salt : ""),
			salt ? strlen( salt ) : 0,
			1,
			key, keylen );
  return 0;
}

static void ef_sha1_hmac( uint8_t *key, uint8_t *input, int n, uint8_t *hmac ) {
  uint8_t opad[16], ipad[16];
  int i;
  SHA_CTX cxt;
  uint8_t rhash[32], final[32];

  init_crypto();
  memset( final, 0, sizeof(final) );

  for( i = 0; i < 16; i++ ) {
    opad[i] = 0x5c ^ key[i];
    ipad[i] = 0x36 ^ key[i];
  }

  SHA1_Init( &cxt );
  SHA1_Update( &cxt, ipad, 16 );
  SHA1_Update( &cxt, input, n );
  SHA1_Final( rhash, &cxt );

  SHA1_Init( &cxt );
  SHA1_Update( &cxt, opad, 16 );
  SHA1_Update( &cxt, rhash, 16 );
  SHA1_Final( final, &cxt );

  memcpy( hmac, final, EF_KEYLEN );
}

/*
 * Encrypt filekey using password key 
 */
static void ef_encrypt_filekey( uint8_t *passkey, uint8_t *filekey, uint8_t *iv, uint8_t *efilekey ) {
  EVP_CIPHER_CTX *cxt;
  int len;

  cxt = EVP_CIPHER_CTX_new();
  
  EVP_CIPHER_CTX_init( cxt );
  EVP_EncryptInit_ex( cxt, EVP_aes_128_cbc(), NULL, passkey, iv );
  EVP_CIPHER_CTX_set_padding( cxt, 0 );
  EVP_EncryptUpdate( cxt, efilekey, &len, filekey, EF_KEYLEN );
  EVP_CIPHER_CTX_cleanup( cxt );

  EVP_CIPHER_CTX_free( cxt );
}

static void ef_decrypt_filekey( uint8_t *passkey, uint8_t *efilekey, uint8_t *iv, uint8_t *filekey ) {
  EVP_CIPHER_CTX *cxt;
  int len;

  cxt = EVP_CIPHER_CTX_new();
  
  EVP_CIPHER_CTX_init( cxt );
  EVP_DecryptInit_ex( cxt, EVP_aes_128_cbc(), NULL, passkey, iv );
  EVP_CIPHER_CTX_set_padding( cxt, 0 );
  EVP_DecryptUpdate( cxt, filekey, &len, efilekey, EF_KEYLEN );
  EVP_CIPHER_CTX_cleanup( cxt );

  EVP_CIPHER_CTX_free( cxt );
}

struct ef_header {
  uint32_t magic;
#define EF_MAGIC 0xef91c3ef
  uint32_t version;
  uint8_t efilekey[EF_KEYLEN];
  uint8_t fkiv[EF_KEYLEN];
  uint8_t iv[EF_KEYLEN];
  uint8_t hmac[EF_KEYLEN];
};

/* encrypt given file */
int ef_encrypt( char *password, char *salt, char *infile, char *outfile ) {
  uint8_t passkey[EF_KEYLEN], filekey[EF_KEYLEN], efilekey[EF_KEYLEN];
  uint8_t fkiv[EF_KEYLEN], iv[EF_KEYLEN];
  int ifd = 0, ofd = 0;
  uint8_t *ifile = NULL, *ofile = NULL;
  EVP_CIPHER_CTX *cxt;
  int isize, osize;
  struct ef_header *header;
 int len, plen;
  int sts = 0;

  cxt = EVP_CIPHER_CTX_new();
  
  /* derive password key */
  ef_pbkdf2( password, salt, passkey, EF_KEYLEN );
  
  /* generate random file key and iv */
  ef_genkey( filekey, EF_KEYLEN );
  ef_genkey( iv, EF_KEYLEN );
  ef_genkey( fkiv, EF_KEYLEN );
  
  /* mmap infile and outfile */
  ifd = open( infile, O_RDWR, 0600 );
  if( ifd < 0 ) {
    ifd = 0;
    sts = EF_EIO;
    goto bad;
  }
  
  ofd = open( outfile, O_RDWR|O_CREAT|O_TRUNC, 0600 );
  if( ofd < 0 ) {
    sts = EF_EIO;
    ofd = 0;
    goto bad;
  }
  
  /* stretch ofile */
  isize = lseek( ifd, (off_t)0, SEEK_END );
  osize = isize;
  if( osize % EF_KEYLEN ) osize += EF_KEYLEN - (osize % EF_KEYLEN);
  osize += 16 + 512;
  
  pwrite( ofd, "", 1, osize - 1 );

  ifile = mmap( NULL, isize, PROT_READ|PROT_WRITE, MAP_SHARED, ifd, 0 );
  if( ifile == MAP_FAILED ) {
    sts = EF_EIO;
    ifile = NULL;
    goto bad;
  }
  
  ofile = mmap( NULL, osize, PROT_READ|PROT_WRITE, MAP_SHARED, ofd, 0 );
  if( ofile == MAP_FAILED ) {
    sts = EF_EIO;
    ofile = NULL;
    goto bad;
  }
  
  /* write header to ofile */
  memset( ofile, 0, 512 );
  header = (struct ef_header *)ofile;
  header->magic = EF_MAGIC;
  header->version = EF_VERSION;
  memcpy( header->fkiv, fkiv, EF_KEYLEN );
  memcpy( header->iv, iv, EF_KEYLEN );
  ef_encrypt_filekey( passkey, filekey, fkiv, header->efilekey );

  /* encrypt file contents */
  EVP_CIPHER_CTX_init( cxt );
  sts = EVP_EncryptInit_ex( cxt, EVP_aes_128_cbc(), NULL, filekey, iv );
  if( !sts ) {
    sts = EF_EIO;
    goto bad;
  }
  
  sts = EVP_EncryptUpdate( cxt, ofile + 512, &len, ifile, isize );
  if( !sts ) {
    sts = EF_EIO;
    goto bad;
  }
  
  sts = EVP_EncryptFinal_ex( cxt, ofile + 512 + len, &plen );
  if( !sts ) {
    sts = EF_EIO;
    goto bad;
  }

  EVP_CIPHER_CTX_cleanup( cxt );
  
  /* hmac the encrypted contents */
  ef_sha1_hmac( filekey, ofile + 512, osize - 512, header->hmac );

  EVP_CIPHER_CTX_free( cxt );
  munmap( ifile, isize );
  munmap( ofile, osize );
  close( ifd );
  close( ofd );
  
  return 0;

 bad:
  EVP_CIPHER_CTX_free( cxt );
  if( ifile ) munmap( ifile, isize );
  if( ofile ) munmap( ofile, osize );
  if( ifd ) close( ifd );
  if( ofd ) close( ofd );
  return sts;
}

int ef_decrypt( char *password, char *salt, char *infile, char *outfile ) {
  int ifd = 0, ofd = 0;
  void *ifile = NULL, *ofile = NULL;
  uint64_t isize = 0, osize = 0;
  uint8_t filekey[EF_KEYLEN], hmac[EF_KEYLEN], passkey[EF_KEYLEN];
  uint64_t fsize;
  struct ef_header *header;
  int len, plen;
  EVP_CIPHER_CTX *cxt;
  int sts;
  uint8_t blk[16];

  cxt = EVP_CIPHER_CTX_new();
  sts = 0;
  
  /* derive password key */
  ef_pbkdf2( password, salt, passkey, EF_KEYLEN );

  /* open files */
  ifd = open( infile, O_RDWR, 0600 );
  if( ifd < 0 ) {
    ifd = 0;
    sts = EF_EIO;
    goto bad;
  }
  
  isize = lseek( ifd, (off_t)0, SEEK_END );
  if( isize < 512 ) {
    sts = EF_EIO;
    goto bad;
  }

  ifile = mmap( NULL, isize, PROT_READ|PROT_WRITE, MAP_SHARED, ifd, 0 );
  if( ifile == MAP_FAILED ) {
    ifile = NULL;
    sts = EF_EIO;
    goto bad;
  }

  /* check header */
  header = (struct ef_header *)ifile;
  if( header->magic != EF_MAGIC ) {
    sts = EF_EBADMAGIC;
    goto bad;
  }

  /* 
   * we only support our version - in future if we ever make changes
   * to the file/header format we may need more sophisticated logic here 
   */
  if( header->version != EF_VERSION ) {
    sts = EF_EVERSION;
    goto bad;
  }

  ofd = open( outfile, O_RDWR|O_CREAT|O_TRUNC, 0600 );
  if( ofd < 0 ) {
    ofd = 0;
    sts = EF_EIO;
    goto bad;
  }
  osize = isize - 512;
  pwrite( ofd, "", 1, osize - 1 );

  ofile = mmap( NULL, osize, PROT_READ|PROT_WRITE, MAP_SHARED, ofd, 0 );
  if( ofile == MAP_FAILED ) {
    ofile = NULL;
    sts = EF_EIO;
    goto bad;
  }

  /* decrypt filekey */
  ef_decrypt_filekey( passkey, header->efilekey, header->fkiv, filekey );

  /* compare hmac */
  ef_sha1_hmac( filekey, ifile + 512, isize - 512, hmac );
  if( memcmp( hmac, header->hmac, EF_KEYLEN ) != 0 ) {
    sts = EF_ECHKSUM;
    goto bad;
  }

  /* all good - decryt file */
  EVP_CIPHER_CTX_init( cxt );
  sts = EVP_DecryptInit_ex( cxt, EVP_aes_128_cbc(), NULL, filekey, header->iv );
  if( !sts ) {
    sts = EF_EIO;
    goto bad;
  }

  fsize = 0;
  sts = EVP_DecryptUpdate( cxt, ofile, &len, ifile + 512, isize - 512 );
  if( !sts ) {
    sts = EF_EIO;
    goto bad;
  }
  fsize += len;
  
  sts = EVP_DecryptFinal_ex( cxt, blk, &plen );
  if( !sts ) {
    sts = EF_EIO;
    goto bad;
  }
  fsize += plen;  
  memcpy( ofile + len, blk, plen );
  EVP_CIPHER_CTX_cleanup( cxt );

  munmap( ifile, isize );
  munmap( ofile, osize );
  close( ifd );

  ftruncate( ofd, fsize );
  close( ofd );
  EVP_CIPHER_CTX_free( cxt );
  
  return 0;

 bad:
  EVP_CIPHER_CTX_free( cxt );
  
  if( ifile ) munmap( ifile, isize );
  if( ofile ) munmap( ofile, osize );
  
  if( ifd ) close( ifd );
  if( ofd ) close( ofd );

  return sts;
}


