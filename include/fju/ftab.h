
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

#ifndef FTAB_H
#define FTAB_H

#include <fju/mmf.h>

#define FTAB_MAX_COOKIE 16

struct ftab_s {
  struct mmf_s mmf;
};

struct ftab_prop {
  uint32_t version;
#define FTAB_VERSION 1
  uint64_t seq;
  uint32_t max;
  uint32_t count;
  uint32_t lbasize;
  char cookie[FTAB_MAX_COOKIE];
};

struct ftab_opts {
    uint32_t mask;
#define FTAB_OPT_LBASIZE      0x0001
#define FTAB_OPT_LBACOUNT     0x0002
#define FTAB_OPT_COOKIE       0x0004 
    uint32_t lbasize;
    uint32_t lbacount;
    char cookie[FTAB_MAX_COOKIE];
};

int ftab_open( char *path, struct ftab_opts *opts, struct ftab_s *ftab );
int ftab_close( struct ftab_s *ftab );
int ftab_prop( struct ftab_s *ftab, struct ftab_prop *prop );
int ftab_reset( struct ftab_s *ftab );
int ftab_set_cookie( struct ftab_s *ftab, char *cookie );
int ftab_sync( struct ftab_s *ftab, int sync );

int ftab_alloc( struct ftab_s *ftab, uint64_t *id );
int ftab_free( struct ftab_s *ftab, uint64_t id );
int ftab_read( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset );
int ftab_write( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset );


#endif

