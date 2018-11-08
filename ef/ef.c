

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
 * 
 */


/*
 * This file defines a CLI tool for encrypting files using a password 
 * derived key. The file is prepended with a header containing a randomly 
 * generated single-use key, encrypted with the password dervied key. The
 * file contents is then encrypted using this temporary key. The file contents
 * is also checksummed with an hmac to test for file data integrity. 
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>

#include "ef.h"

static void usage( char *fmt, ... ) {
  va_list args;

  printf( "Usage: ef [-p password] [-s salt]\n"
	  "          [-e | -d] -i infile -o outfile\n"
	  "\n"
	  "Where: \n"
	  "   -p password            Password string\n"
	  "   -s salt                Salt string\n"
	  "   -e                     Encrypt\n"
	  "   -d                     Decrypt\n"
	  "   -i infile              Input file name\n"
	  "   -o outfile             Output file name\n" 
	  "\n" );
  if( fmt ) {
    printf( "Error: " );
    va_start( args, fmt );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
  exit( 1 );
}
	     

int main( int argc, char **argv ) {
  int i;
  char *pw, *salt;
  char *infile, *outfile;
  int encrypt, decrypt;
  int sts;
  uint8_t key[16];
  
  pw = NULL;
  salt = NULL;
  infile = NULL;
  outfile = NULL;
  encrypt = 0;
  decrypt = 0;
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      pw = argv[i];
    } else if( strcmp( argv[i], "-s" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      salt = argv[i];
    } else if( strcmp( argv[i], "-e" ) == 0 ) {
      encrypt = 1;
    } else if( strcmp( argv[i], "-d" ) == 0 ) {
      decrypt = 1;
    } else if( strcmp( argv[i], "-i" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      infile = argv[i];
    } else if( strcmp( argv[i], "-o" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      outfile = argv[i];
    } else if( strcmp( argv[i], "-v" ) == 0 ) {
      printf( "ef version %d.%d.%d build time " __DATE__ " " __TIME__ "\n",
	      (EF_VERSION >> 24) & 0xff, (EF_VERSION >> 16) & 0xff, (EF_VERSION >> 8) & 0xff );
      exit( 0 );
    } else usage( NULL );
    
    i++;
  }

  if( !pw ) {
    /* generate random key */
    ef_genkey( key, 16 );
    for( i = 0; i < 16; i++ ) {
      printf( "%02x", key[i] );
    }
    printf( "\n" );
  } else if( !encrypt && !decrypt ) {
    /* pbkdf2 password */
    ef_pbkdf2( pw, salt, key, 16 );
    for( i = 0; i < 16; i++ ) {
      printf( "%02x", key[i] );
    }
    printf( "\n" );
  } else if( !infile || !outfile ) {
    usage( "Must specify input and output files" );
  } else if( encrypt ) {
    sts = ef_encrypt( pw, salt, infile, outfile );
    if( sts ) usage( "Failed to encrypt file: %s",
		     sts == EF_EIO ? "Generic failure" :
		     sts == EF_ECHKSUM ? "Checksum mismatch" :
		     sts == EF_EBADMAGIC ? "Not an ef file" :
		     sts == EF_EVERSION ? "Bad ef version" :
		     sts == EF_ESIZE ? "Bad file size" : 
		     "Other" );
  } else if( decrypt ) {
    sts = ef_decrypt( pw, salt, infile, outfile );
    if( sts ) usage( "Failed to decrypt file: %s",
		     sts == EF_EIO ? "Generic failure" :
		     sts == EF_ECHKSUM ? "Checksum mismatch" :
		     sts == EF_EBADMAGIC ? "Not an ef file" :
		     sts == EF_EVERSION ? "Bad ef version" :
		     sts == EF_ESIZE ? "Bad file size" : 
		     "Other" );
  } else usage( NULL );

  
  return 0;
}
