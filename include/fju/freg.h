
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

#ifndef FREG_H
#define FREG_H

#include <stdint.h>

#define FREG_MAX_NAME 64

struct freg_entry {
  char name[FREG_MAX_NAME];
  int len;
  uint32_t flags;
#define FREG_TYPE_MASK   0x000f 
#define FREG_TYPE_OPAQUE 0x0000    /* data contains opaque octets */
#define FREG_TYPE_UINT32 0x0001    /* data contains uint32 */
#define FREG_TYPE_UINT64 0x0002    /* data contains uint64 */
#define FREG_TYPE_STRING 0x0003    /* data contains string */
#define FREG_TYPE_KEY    0x0004    /* data contains id of child node */
};

int freg_open( void );
int freg_close( void );

int freg_list( uint64_t parentid, struct freg_entry *entry, int n );
int freg_get( uint64_t parentid, char *name, uint32_t *flags, char *buf, int len, int *lenp );
int freg_put( uint64_t parentid, char *name, uint32_t flags, char *buf, int len );
int freg_rem( uint64_t parentid, char *name );

#define FREG_CREATE 0x0001 
int freg_subkey( uint64_t parentid, char *name, uint32_t flags, uint64_t *id );

#endif

