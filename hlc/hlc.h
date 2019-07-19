

#ifndef HLC_H
#define HLC_H

/* 
 * hashed log chain 
 */

#include <log.h>

struct hlc_s {
  struct log_s log;
};

struct hlc_entry {
  uint64_t id;
  uint64_t prevhash; 
  uint64_t hash;
  char *buf;
  int len;
};
  
struct hlc_prop {
  struct log_prop lprop;
  uint64_t starthash;
};

int hlc_open( char *path, struct hlc_s *hlc );
int hlc_close( struct hlc_s *hlc );
int hlc_prop( struct hlc_s *hlc, struct hlc_prop *prop );

int hlc_read( struct hlc_s *hlc, uint64_t id, struct hlc_entry *elist, int n, int *nelist );
int hlc_write( struct hlc_s *hlc, struct hlc_entry *entry );


#endif
