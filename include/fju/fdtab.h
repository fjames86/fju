
#ifndef FDTAB_H
#define FDTAB_H

#include <fju/ftab.h>

struct fdtab_s {
  struct ftab_s ftab;
  uint32_t lbasize;  
};

int fdtab_open( char *path, struct ftab_opts *opts, struct fdtab_s *fdt );
int fdtab_close( struct fdtab_s *fdt );
int fdtab_alloc( struct fdtab_s *fdt, uint32_t size, uint64_t *id );
int fdtab_free( struct fdtab_s *fdt, uint64_t id );
int fdtab_realloc( struct fdtab_s *fdt, uint64_t id, uint32_t newsize );
int fdtab_size( struct fdtab_s *fdt, uint64_t id );
int fdtab_maxsize( struct fdtab_s *fdt, uint64_t id );
int fdtab_read( struct fdtab_s *fdt, uint64_t id, char *buf, int n, uint32_t offset );
int fdtab_write( struct fdtab_s *fdt, uint64_t id, char *buf, int n, uint32_t offset );
int fdtab_truncate( struct fdtab_s *fdt, uint64_t id, uint32_t size );

#endif

