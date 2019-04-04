
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

#define HOSTREG_MAX_ADDR       8
#define HOSTREG_MAX_PUBKEY     64
#define HOSTREG_MAX_PRIVKEY    96

struct hostreg_host {
    uint64_t id;
    char name[64];
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

int hostreg_host_local( struct hostreg_host *host );
int hostreg_host_list( struct hostreg_host *list, int n );
int hostreg_host_by_id( uint64_t id, struct hostreg_host *host );
int hostreg_host_by_name( char *name, struct hostreg_host *host );
int hostreg_host_put( struct hostreg_host *entry );
int hostreg_host_rem( uint64_t id );

#endif

