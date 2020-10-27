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

/*
 * This file defines a small program showing how to use the ECDH functions 
 * to generate keys and derive the common secret 
 */

#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS 
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <fju/sec.h>

static void usage( char *fmt, ... ) {
  if( fmt ) {
    va_list args;
    printf( "Error: " );
    va_start( args, fmt );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
    exit( 1 );
  }

  printf( "ecdh [-p public -s secret] [-S signdata] [-V verifysig] [-d | -e]\n"
	  "\n"
	  "     -p public       Public key\n"
	  "     -s secret       Secret key\n"
	  "     -S signdata     Generate signature from this string, requires secret key\n"
	  "     -V verifysig    Verify signature, requires public key and signdata\n"
	  "     -e              Encrypt from stdin\n"
	  "     -d              Decrypt from stdin\n" 
	  );
  
  exit( 0 );
}


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
  int i;
  char secret_buf[SEC_ECDH_MAX_PRIVKEY];
  char pub_buf[SEC_ECDH_MAX_PUBKEY];
  char common_buf[SEC_ECDH_MAX_COMMON];
  struct sec_buf secret, public, common;
  int sp, pp;
  char hex[256];
  char *signstr = NULL, *verstr = NULL;
  int encflag = 0; // 0=don't 1=encrypt 2=decrypt 
  
  memset( secret_buf, 0, sizeof(secret_buf) );
  memset( common_buf, 0, sizeof(common_buf) );
  memset( pub_buf, 0, sizeof(pub_buf) );
  sec_buf_init( &secret, secret_buf, sizeof(secret_buf) );  
  sec_buf_init( &public, pub_buf, sizeof(pub_buf) );
  sec_buf_init( &common, common_buf, sizeof(common_buf) );
  
  sp = 0;
  pp = 0;

  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-s" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      hex2bn( argv[i], &secret );
      sp = 1;
    } else if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      hex2bn( argv[i], &public );
      pp = 1;
    } else if( strcmp( argv[i], "-S" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      signstr = argv[i];
    } else if( strcmp( argv[i], "-V" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      verstr = argv[i];
    } else if( strcmp( argv[i], "-e" ) == 0 ) {
      encflag = 1;
    } else if( strcmp( argv[i], "-d" ) == 0 ) {
      encflag = 2;
    } else usage( NULL );
    i++;
  }

  if( pp && sp ) {
    ecdh_common( &secret, &public, &common );
    bn2hex( common.buf, hex, common.len );
    if( !encflag ) printf( "COMMON %s\n", hex );    
  } else if( signstr == NULL && verstr == NULL ) {
    ecdh_generate( &secret, &public );
    bn2hex( secret.buf, hex, secret.len );
    printf( "SECRET %s\n", hex );
    bn2hex( public.buf, hex, public.len );
    printf( "PUBLIC %s\n", hex );
    ecdh_common( &secret, &public, &common );
  }

  if( signstr ) {
    struct sec_buf dataiov[1], sig;
    char sigbuf[SEC_MAX_SIG];
    int sts;

    dataiov[0].buf = signstr;
    dataiov[0].len = strlen( signstr );
    
    if( sp ) {      
      sig.buf = sigbuf;
      sig.len = SEC_MAX_SIG;
      do {
	sts = sec_sign( &secret, dataiov, 1, &sig );
	if( sts ) usage( "Sign failed" );
      } while( sig.len != 70 );
      
      memset( hex, 0, sizeof(hex) );
      base64_encode( sig.buf, sig.len, hex );
      //bn2hex( sig.buf, hex, sig.len );
      printf( "SIG %s\n", hex );
    }
    
    if( pp && verstr ) {
      sts = base64_decode( sigbuf, sizeof(sigbuf), verstr );
      if( sts < 0 ) usage( "Bad verf string" );
      sig.len = sts;
      sig.buf = sigbuf;
      
      sts = sec_verify( &public, dataiov, 1, &sig );
      printf( "Verify %s\n", sts ? "failed" : "success" );
    }
  }

  if( encflag ) {
    char *encbuf = malloc( 32*1024 );
    int n;
    
    memset( encbuf, 0, 32*1024 );
    
    n = fju_readstdin( encbuf, 32*1024 );
    if( n % 16 ) n += 16 - (n % 16);

    if( encflag == 1 ) {
      aes_encrypt( (uint8_t *)common.buf, (uint8_t *)encbuf, n );
    } else {
      aes_decrypt( (uint8_t *)common.buf, (uint8_t *)encbuf, n );
    }
    
    fju_writestdout( encbuf, n );
  }

  return 0;
}

