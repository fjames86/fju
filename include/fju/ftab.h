
#ifndef FTAB_H
#define FTAB_H

#include <fju/mmf.h>

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

int ftab_alloc( struct ftab_s *ftab, uint64_t *id );
int ftab_free( struct ftab_s *ftab, uint64_t id );
int ftab_read( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset );
int ftab_write( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset );

#endif

