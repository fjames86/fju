 
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

