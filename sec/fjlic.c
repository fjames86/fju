
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include <fju/sec.h>
#include <fju/lic.h>

static void usage( char *fmt, ... ) {
  printf( "Usage: fjlic hostid pubkey [-v version] [-D days] [-k privkey]\n" 
	  );
  if( fmt ) {
    va_list args;
    printf( "Error: " );
    va_start( args, fmt );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
  
  exit( 1 );
}

static uint8_t privkeybuf[] =
  {
   0xee, 0xf0, 0x99, 0x48, 0xef, 0x76, 0x72, 0xb9, 0xbd, 0xe1, 0xaa,
    0x09, 0xdf, 0x53, 0x54, 0xac, 0x07, 0xee, 0xe5, 0xa5, 0x60, 0x68,
    0x64, 0x8d, 0x43, 0xa0, 0x42, 0x84, 0x0c, 0x6f, 0x58, 0x5a
  };

static void hex2bn( char *hex, struct sec_buf *buf ) {
  int i, j;
  unsigned char x;
  for( i = 0; i < buf->len; i++ ) {
    if( hex[2*i] == '\0' ) break;

    x = 0;
    j = 2 * i;
    if( hex[j] != '\0' ) {
      if( hex[j] >= '0' && hex[j] <= '9' ) x = hex[j] - '0';
      else if( hex[j] >= 'a' && hex[j] <= 'f' ) x = 10 + (hex[j] - 'a');
      else if( hex[j] >= 'A' && hex[j] <= 'F' ) x = 10 + (hex[j] - 'A');
      else usage( "Unable to parse \"%s\"", hex );
    }
    x = x << 4;
    if( hex[2*i + 1] != '\0' ) {
      j = (2*i) + 1;
      if( hex[j] >= '0' && hex[j] <= '9' ) x |= hex[j] - '0';
      else if( hex[j] >= 'a' && hex[j] <= 'f' ) x |= 10 + (hex[j] - 'a');
      else if( hex[j] >= 'A' && hex[j] <= 'F' ) x |= 10 + (hex[j] - 'A');
      else usage( "Unable to parse \"%s\"", hex );
    }
    buf->buf[i] = x;
    if( hex[(2*i)+1] == '\0' ) break;
  }
  buf->len = i;
}

int main( int argc, char **argv ) {
  int sts, i, version, days;
  struct lic_s lic;
  struct sec_buf privkey, iov[1], sig;
  uint64_t hostid;
  char *term;
  char str[256];
  uint8_t *p;
  char hostpubkey[SEC_ECDH_MAX_PUBKEY];
  char commonbuf[SEC_ECDH_MAX_COMMON];
  struct sec_buf hostpubkeybuf, seckeybuf, common;
  
  if( argc < 3 ) usage( NULL );
  hostid = strtoull( argv[1], &term, 16 );
  if( *term ) usage( "Failed to parse hostid" );

  hostpubkeybuf.buf = hostpubkey;
  hostpubkeybuf.len = SEC_ECDH_MAX_PUBKEY;
  hex2bn( argv[2], &hostpubkeybuf );
  seckeybuf.buf = (char *)privkeybuf;
  seckeybuf.len = sizeof(privkeybuf);
  common.buf = (char *)commonbuf;
  common.len = sizeof(commonbuf);
  ecdh_common( &seckeybuf, &hostpubkeybuf, &common );
  
  version = 1;
  days = 365;
  i = 3;
  while( i < argc ) {
    if( strcmp( argv[i], "-v" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      version = strtoul( argv[i], NULL, 0 );
    } else if( strcmp( argv[i], "-D" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      days = strtoul( argv[i], NULL, 0 );
    } else if( strcmp( argv[i], "-k" ) == 0 ) {
      struct sec_buf xx;
      i++;
      if( i >= argc ) usage( NULL );
      xx.buf = (char *)privkeybuf;
      xx.len = sizeof(privkeybuf);
      hex2bn( argv[i], &xx );
    } else usage( NULL );
    i++;
  }
  
  memset( &lic, 0, sizeof(lic) );
  lic.hostid = hostid;
  lic.expire = time( NULL ) + (60*60*24*days);
  lic.version = version;

  privkey.buf = (char *)privkeybuf;
  privkey.len = sizeof(privkeybuf);
  iov[0].buf = (char *)&lic;
  iov[0].len = 32;
  sig.buf = lic.verf;
  sig.len = sizeof(lic.verf);
  sts = sec_sign( &privkey, iov, 1, &sig );
  if( sts ) usage( "Failed to sign" );
  lic.nverf = sig.len;
  
  aes_encrypt( (uint8_t *)common.buf, (uint8_t *)&lic, sizeof(lic) );
  
  p = (uint8_t *)&lic;
  strcpy( str, "" );
  for( i = 0; i < sizeof(lic); i++ ) {
    sprintf( str + strlen( str ), "%02x", (uint32_t)p[i] );
  }
  
  printf( "put /fju/lic opaque %s\n", str );
  
  return 0;
}
