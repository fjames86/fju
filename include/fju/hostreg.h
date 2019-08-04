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
 
/*
 * This file was generated by cfgen.lisp

(CFGEN:GEN ("hostreg")
           (("localid" :UINT64 NIL) ("publen" :UINT32 NIL) ("pubkey" :UINT8 64)
            ("privlen" :UINT32 NIL) ("privkey" :UINT8 96))
           (("host"
             (("id" :UINT64 NIL) ("name" :STRING 64) ("publen" :UINT32 NIL)
              ("pubkey" :UINT8 64) ("naddr" :UINT32 NIL) ("addr" :UINT32 8)))))

 *
 */

#ifndef HOSTREG_H
#define HOSTREG_H

#include <stdint.h>

#define HOSTREG_MAX_NAME       64 
#define HOSTREG_MAX_ADDR       8
#define HOSTREG_MAX_PUBKEY     64
#define HOSTREG_MAX_PRIVKEY    32

struct hostreg_host {
    uint64_t id;
    char name[HOSTREG_MAX_NAME];
    uint32_t publen;
    uint8_t pubkey[HOSTREG_MAX_PUBKEY];
    uint32_t naddr;
    uint32_t addr[HOSTREG_MAX_ADDR];
};

struct hostreg_prop {
    uint32_t version;
#define HOSTREG_VERSION 1
    uint64_t seq;
    uint32_t host_max;
    uint32_t host_count;
    uint64_t localid;
    uint32_t publen;
    uint8_t pubkey[HOSTREG_MAX_PUBKEY];
    uint32_t privlen;
    uint8_t privkey[HOSTREG_MAX_PRIVKEY];
};

int hostreg_open( void );
int hostreg_close( void );
int hostreg_prop( struct hostreg_prop *prop );
int hostreg_reset( int full );
uint64_t hostreg_localid( void );

int hostreg_host_local( struct hostreg_host *host );
int hostreg_host_list( struct hostreg_host *list, int n );
int hostreg_host_by_id( uint64_t id, struct hostreg_host *host );
int hostreg_host_by_name( char *name, struct hostreg_host *host );
uint64_t hostreg_hostid_by_name( char *name );
char *hostreg_name_by_hostid( uint64_t hostid, char *str );
int hostreg_host_put( struct hostreg_host *entry );
int hostreg_host_rem( uint64_t id );
int hostreg_host_common( uint64_t hostid, char *common, int *size );

#endif
