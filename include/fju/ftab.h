
#ifndef FTAB_H
#define FTAB_H

#include <fju/mmf.h>

#define FTAB_MAX_COOKIE 16

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
  char cookie[FTAB_MAX_COOKIE];
};

struct ftab_opts {
    uint32_t mask;
#define FTAB_OPT_LBASIZE      0x0001
#define FTAB_OPT_LBACOUNT     0x0002
#define FTAB_OPT_COOKIE       0x0004 
    uint32_t lbasize;
    uint32_t lbacount;
    char cookie[FTAB_MAX_COOKIE];
};

int ftab_open( char *path, struct ftab_opts *opts, struct ftab_s *ftab );
int ftab_close( struct ftab_s *ftab );
int ftab_prop( struct ftab_s *ftab, struct ftab_prop *prop );
int ftab_reset( struct ftab_s *ftab );
int ftab_set_cookie( struct ftab_s *ftab, char *cookie );
int ftab_sync( struct ftab_s *ftab, int sync );

int ftab_alloc( struct ftab_s *ftab, uint64_t *id );
int ftab_free( struct ftab_s *ftab, uint64_t id );
int ftab_read( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset );
int ftab_write( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset );


#endif

