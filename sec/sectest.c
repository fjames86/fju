
#include <WinSock2.h>
#include <Windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <bcrypt.h>
#include <fju/sec.h>

#define SEC_ECDH_KEYLEN 32

static void bn2hex( char *bn, char *hex, int len ) {
  int i;
  unsigned int x;
  strcpy( hex, "" );
  for( i = 0; i < len; i++ ) {
    x = (unsigned int)((unsigned char)bn[i]);
    sprintf( hex + 2*i, "%02x", x );
  }
  hex[len*2] = '\0';
}


static int ecdh_common2( struct sec_buf *local_priv, struct sec_buf *remote_public, struct sec_buf *common ) {
  BCRYPT_ALG_HANDLE handle;
  BCRYPT_KEY_HANDLE hkey, hrkey;
  BCRYPT_SECRET_HANDLE skey;
  int outlen;
  NTSTATUS sts;
  BCRYPT_ECCKEY_BLOB *eccp;
  char pout[3*SEC_ECDH_KEYLEN + sizeof(BCRYPT_ECCKEY_BLOB)];
  int nbytes;
  ULONG (*pfn)( void *alg, void *hkey, void *blobtype, void *phkey, void *input, ULONG cinput, ULONG dwflags );
  void *hdl;
  wchar_t *wstr;
  char cstr[256];
  
  hdl = LoadLibraryA( "BCrypt.dll" );
  pfn = GetProcAddress( hdl, "BCryptImportKeyPair" );
  
  if( local_priv->len != (SEC_ECDH_KEYLEN) ) return -1;
  if( remote_public->len != (SEC_ECDH_KEYLEN*2) ) return -1;

  
  sts = BCryptOpenAlgorithmProvider( &handle, BCRYPT_ECDH_P256_ALGORITHM, NULL, 0 );
  if( sts ) {
      printf( "BCryptOpenAlgorithmProvider failed %u (%x)\n", sts, sts );
      return -1;
  }

  outlen = sizeof(*eccp) + 3*SEC_ECDH_KEYLEN;
  eccp = (BCRYPT_ECCKEY_BLOB *)pout;
  eccp->dwMagic = BCRYPT_ECDH_PRIVATE_P256_MAGIC;
  eccp->cbKey = SEC_ECDH_KEYLEN;
  memcpy( pout + sizeof( *eccp ), remote_public->buf, 2 * SEC_ECDH_KEYLEN );
  memcpy( pout + sizeof(*eccp) + 2*SEC_ECDH_KEYLEN, local_priv->buf, SEC_ECDH_KEYLEN );
  wcstombs( cstr, BCRYPT_ECCPRIVATE_BLOB, 256 );
  printf( "BCRYPT_ECCPRIVATE_BLOB cstr = %s\n", cstr );
  wstr = L"ECCPRIVATEBLOB"; //  BCRYPT_ECCPRIVATE_BLOB;
  wprintf( L"BCRYPT_ECCPRIVATE_BLOB wstr = %s\n", wstr );
  sts = pfn( handle, NULL, wstr, &hkey, pout, outlen, 0 );
  if( sts ) {
      printf( "BCryptImportKeyPair priv failed %u (%x)\n", sts, sts );
      return -1;
  }
  
  outlen = sizeof(*eccp) + 2*SEC_ECDH_KEYLEN;
  eccp = (BCRYPT_ECCKEY_BLOB *)pout;
  eccp->dwMagic = BCRYPT_ECDH_PUBLIC_P256_MAGIC;
  eccp->cbKey = SEC_ECDH_KEYLEN;
  memcpy( pout + sizeof(*eccp), remote_public->buf, SEC_ECDH_MAX_PUBKEY );
  sts = pfn( handle, NULL, BCRYPT_ECCPUBLIC_BLOB, &hrkey, pout, outlen, 0 );
  if( sts ) {
      printf( "BCryptImportKeyPair pub failed %u (%x)\n", sts, sts );
      return -1;
  }
  
  /* derive common key */
  sts = BCryptSecretAgreement( hkey, hrkey, &skey, 0 );
  if( sts ) {
      printf( "BCryptSecretAgreement failed %u (%x)\n", sts, sts );
      return -1;
  }
  
  sts = BCryptDeriveKey( skey, BCRYPT_KDF_HASH, NULL, common->buf, SEC_ECDH_MAX_COMMON, &nbytes, 0 );
  if( sts ) {
      printf( "BCryptDeriveKey failed %u (%x)\n", sts, sts );
      return -1;
  }
  
  common->len = SEC_ECDH_MAX_COMMON;
  
  BCryptDestroyKey( hkey );
  BCryptDestroyKey( hrkey );
  BCryptDestroySecret( skey );
  BCryptCloseAlgorithmProvider( handle, 0 );
   
  return 0;
}


int main( int argc, char **argv ) {
  struct sec_buf priv1, pub1;
  struct sec_buf priv2, pub2;
  struct sec_buf com1, com2;
  char buf_priv1[SEC_ECDH_MAX_PRIVKEY];
  char buf_priv2[SEC_ECDH_MAX_PRIVKEY];
  char buf_pub1[SEC_ECDH_MAX_PUBKEY];
  char buf_pub2[SEC_ECDH_MAX_PUBKEY];
  char buf_com1[SEC_ECDH_MAX_COMMON];
  char buf_com2[SEC_ECDH_MAX_COMMON];
  char hex[256];
  
  priv1.buf = buf_priv1;
  priv2.buf = buf_priv2;
  pub1.buf = buf_pub1;
  pub2.buf = buf_pub2;
  com1.buf = buf_com1;
  com2.buf = buf_com2;

  ecdh_generate( &priv1, &pub1 );
  ecdh_generate( &priv2, &pub2 );
  ecdh_common2( &priv1, &pub2, &com1 );
  ecdh_common2( &priv2, &pub1, &com2 );
  if( memcmp( com1.buf, com2.buf, SEC_ECDH_MAX_COMMON ) != 0 ) {
    printf( "FAILED ecdh_common not equal\n" );
    bn2hex( priv1.buf, hex, priv1.len );
    printf( "PRIV1: %s\n", hex );
    bn2hex( pub1.buf, hex, pub1.len );
    printf( "PUB1:  %s\n", hex );
    bn2hex( priv2.buf, hex, priv2.len );
    printf( "PRIV2: %s\n", hex );
    bn2hex( pub2.buf, hex, pub2.len );
    printf( "PUB2:  %s\n", hex );
    bn2hex( com1.buf, hex, com1.len );
    printf( "COM1:  %s\n", hex );
    bn2hex( com2.buf, hex, com2.len );
    printf( "COM2:  %s\n", hex );

  }

  sec_rand( buf_priv1, sizeof(buf_priv1) );
  sha1( (uint8_t *)buf_priv1, &priv1, 1 );
  sha1_hmac( (uint8_t *)buf_priv2, (uint8_t *)buf_com1, &priv1, 1 );
  
  


  return 0;
}

