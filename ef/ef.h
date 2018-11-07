
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

#ifndef EF_H
#define EF_H

#include <stdint.h>

#define EF_VERSION 0x01000000   /* version 1.0.0.0 */

int ef_encrypt( char *password, char *salt, char *infile, char *outfile );
int ef_decrypt( char *password, char *salt, char *infile, char *outfile );
int ef_pbkdf2( char *password, char *salt, uint8_t *key, int keylen );
int ef_genkey( uint8_t *key, int keylen );

#define EF_EIO         1
#define EF_ECHKSUM     2
#define EF_EBADMAGIC   3
#define EF_EVERSION    4
#define EF_ESIZE       5


#endif


