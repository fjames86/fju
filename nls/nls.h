
/*
 * This file was generated by cfgen.lisp

(CFGEN:GEN ("nls") NIL (("share" (("name" :STRING 64) ("hshare" :UINT64 NIL)))))

 *
 */

#ifndef NLS_H
#define NLS_H

#include <stdint.h>
#include <log.h>

#define NLS_MAX_COOKIE 16

/* local log shared over RPC */
struct nls_share {
    char name[64];
    uint64_t hshare;
};

/* remote shared log */
struct nls_remote {
  uint64_t hostid;   /* remote host id */
  struct nls_share share; /* shared log descriptor */
  uint64_t seq;      /* last known log seqno and message id */
  uint64_t lastid;
  uint64_t timestamp;
};

struct nls_notify {
  uint64_t tag;
  uint64_t hostid;
  uint64_t hshare;
  uint64_t seq;
  uint64_t lastid;
  uint64_t timestamp;
  uint8_t cookie[NLS_MAX_COOKIE];
};

struct nls_prop {
    uint32_t version;
#define NLS_VERSION 1
    uint64_t seq;
    uint32_t share_max;
    uint32_t share_count;
    uint32_t remote_max;
    uint32_t remote_count;
    uint32_t notify_max;
    uint32_t notify_count;  
};

int nls_open( void );
int nls_close( void );
int nls_prop( struct nls_prop *prop );
int nls_reset( void );

int nls_share_list( struct nls_share *list, int n );
int nls_share_by_hshare( uint64_t hshare, struct nls_share *share );
int nls_share_add( struct nls_share *entry );
int nls_share_rem( uint64_t hshare );
int nls_share_set( struct nls_share *entry );
int nls_share_open( struct nls_share *share, struct log_s *log );

int nls_remote_list( struct nls_remote *list, int n );
int nls_remote_by_hshare( uint64_t hshare, struct nls_remote *remote );
int nls_remote_add( struct nls_remote *entry );
int nls_remote_rem( uint64_t hshare );
int nls_remote_set( struct nls_remote *entry );
int nls_remote_open( struct nls_remote *remote, struct log_s *log );

int nls_notify_list( struct nls_notify *list, int n );
int nls_notify_by_tag( uint64_t tag, struct nls_notify *notify );
int nls_notify_by_hshare( uint64_t hostid, uint64_t hshare, struct nls_notify *notify );
int nls_notify_add( struct nls_notify *entry );
int nls_notify_rem( uint64_t tag );
int nls_notify_set( struct nls_notify *entry );


#define NLS_RPC_PROG 0x27E1FAEE
#define NLS_RPC_VERS 1
void nls_register( void );

#endif

