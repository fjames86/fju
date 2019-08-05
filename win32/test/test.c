/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
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
 
#define _CRT_SECURE_NO_WARNINGS 

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <fju/mmf.h>

struct _file {
  uint32_t magic;
  uint32_t n;
  char str[64];
};

int main( int argc, char **argv ) {
  struct mmf_s mmf;
  int sts;
  struct _file *f;

  sts = mmf_open( "test.dat", &mmf );
  mmf_remap( &mmf, 1024 );
  f = (struct _file *)mmf.file;

  mmf_lock( &mmf );
  f->magic = 0x12344321;
  printf( "n=%d str=\"%s\"\n", f->n, f->str );
  f->n++;
  sprintf( f->str, "hello %d!", f->n );
  mmf_unlock( &mmf );

  mmf_close( &mmf );

  return 0;
}




