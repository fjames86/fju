
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

#ifndef FDTAB_H
#define FDTAB_H

#include <fju/ftab.h>

struct fdtab_s {
  struct ftab_s ftab;
  uint32_t lbasize;  
};

int fdtab_open( char *path, struct ftab_opts *opts, struct fdtab_s *fdt );
int fdtab_close( struct fdtab_s *fdt );
int fdtab_alloc( struct fdtab_s *fdt, uint32_t size, uint64_t *id );
int fdtab_free( struct fdtab_s *fdt, uint64_t id );
int fdtab_realloc( struct fdtab_s *fdt, uint64_t id, uint32_t newsize );
int fdtab_size( struct fdtab_s *fdt, uint64_t id );
int fdtab_read( struct fdtab_s *fdt, uint64_t id, char *buf, int n, uint32_t offset );
int fdtab_write( struct fdtab_s *fdt, uint64_t id, char *buf, int n, uint32_t offset );
int fdtab_truncate( struct fdtab_s *fdt, uint64_t id, uint32_t size );

#endif

