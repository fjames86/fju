
#ifndef FTAB_H
#define FTAB_H

#include <fju/mmf.h>

#define FTAB_MAX_PRIV 16

/* 32 byte entry */
struct ftab_entry {
  uint64_t id;
  uint32_t blkidx;
  uint32_t seq;
  char priv[FTAB_MAX_PRIV];
};

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
};

struct ftab_opts {
    uint32_t mask;
#define FTAB_OPT_LBASIZE      0x0001
#define FTAB_OPT_LBACOUNT     0x0002 
    uint32_t lbasize;
    uint32_t lbacount;
};

int ftab_open( char *path, struct ftab_opts *opts, struct ftab_s *ftab );
int ftab_close( struct ftab_s *ftab );
int ftab_prop( struct ftab_s *ftab, struct ftab_prop *prop );
int ftab_reset( struct ftab_s *ftab );

int ftab_list( struct ftab_s *ftab, struct ftab_entry *elist, int n );
int ftab_entry_by_id( struct ftab_s *ftab, uint64_t id, struct ftab_entry *entry );

int ftab_alloc( struct ftab_s *ftab, char *priv, int npriv, uint64_t *id );
int ftab_free( struct ftab_s *ftab, uint64_t id );
int ftab_read( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset );
int ftab_write( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset );
int ftab_set_priv( struct ftab_s *ftab, uint64_t id, char *priv, int n );
int ftab_swap( struct ftab_s *ftab, uint64_t id1, uint64_t id2 );

#endif

