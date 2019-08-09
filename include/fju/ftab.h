
#ifndef FTAB_H
#define FTAB_H

#include <fju/mmf.h>

/* 32 byte entry */
struct ftab_entry {
  uint64_t id;
  uint32_t blkidx;
  uint32_t seq;
  uint32_t flags;
  uint32_t refcount;
  uint64_t nextid;
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

int ftab_open( char *path, void *reserved, struct ftab_s *ftab );
int ftab_close( struct ftab_s *ftab );
int ftab_prop( struct ftab_s *ftab, struct ftab_prop *prop );

int ftab_list( struct ftab_s *ftab, struct ftab_entry *elist, int n );
int ftab_alloc( struct ftab_s *ftab, uint64_t *id );
int ftab_acquire( struct ftab_s *ftab, uint64_t id );
int ftab_free( struct ftab_s *ftab, uint64_t id );
int ftab_read( struct ftab_s *ftab, uint64_t id, char *buf, int n );
int ftab_write( struct ftab_s *ftab, uint64_t id, char *buf, int n );

#endif

