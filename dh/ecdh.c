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

  printf( "ecdh [-p public -s secret]\n" );
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
    } else usage( NULL );
    i++;
  }

  if( pp && sp ) {
    ecdh_common( &secret, &public, &common );
    bn2hex( common.buf, hex, common.len );
    printf( "COMMON %s\n", hex );
  } else {
    ecdh_generate( &secret, &public );
    bn2hex( secret.buf, hex, secret.len );
    printf( "SECRET %s\n", hex );
    bn2hex( public.buf, hex, public.len );
    printf( "PUBLIC %s\n", hex );
  }

  return 0;
}

