
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <fju/sec.h>

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
  ecdh_common( &priv1, &pub2, &com1 );
  ecdh_common( &priv2, &pub1, &com2 );
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

