
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
#include <fju/fdtab.h>

#define FREG_MAX_NAME 64

struct freg_entry {
    uint64_t id;
    char name[FREG_MAX_NAME];
    uint32_t flags;
#define FREG_TYPE_MASK   0x000f 
#define FREG_TYPE_OPAQUE 0x0000    /* data contains opaque octets */
#define FREG_TYPE_UINT32 0x0001    /* data contains uint32 */
#define FREG_TYPE_UINT64 0x0002    /* data contains uint64 */
#define FREG_TYPE_STRING 0x0003    /* data contains string */
#define FREG_TYPE_KEY    0x0004    /* data contains id of child node */
    uint32_t len;
};

struct freg_s {
  struct fdtab_s fdt;
  uint64_t rootid;  
};

int freg_open( char *path, struct freg_s *freg );
int freg_close( struct freg_s *freg );

int freg_list( struct freg_s *freg, uint64_t parentid, struct freg_entry *entry, int n );
int freg_entry_by_name( struct freg_s *freg, uint64_t parentid, char *name, struct freg_entry *entry, uint64_t *parentidp );
int freg_entry_by_id( struct freg_s *freg, uint64_t id, struct freg_entry *entry );

int freg_get( struct freg_s *freg, uint64_t id, uint32_t *flags, char *buf, int len, int *lenp );
int freg_get_by_name( struct freg_s *freg, uint64_t parentid, char *name, uint32_t flags, char *buf, int len, int *lenp );
int freg_put( struct freg_s *freg, uint64_t parentid, char *name, uint32_t flags, char *buf, int len, uint64_t *id );
int freg_set( struct freg_s *freg, uint64_t id, char *name, uint32_t *flags, char *buf, int len );
int freg_rem( struct freg_s *freg, uint64_t parentid, uint64_t id );

#define FREG_CREATE    0x0001        /* create all keys in the path */
#define FREG_VALUEPATH 0x0002        /* final name represents a value not a key */
int freg_subkey( struct freg_s *freg, uint64_t parentid, char *name, uint32_t flags, uint64_t *id );

int freg_ensure( struct freg_s *freg, char *path, uint32_t flags, char *buf, int len, uint64_t *id );

#define FREG_RPC_PROG 0x27E1FB10
#define FREG_RPC_VERS 1
void freg_register( void );

#endif

