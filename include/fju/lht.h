 
#ifndef LHT_H
#define LHT_H

#include <stdint.h>
#include <fju/log.h>

/* 
 * logged hash table. 
 * hash table kept in memory. each change is written to a log. 
 * restoring the hash table is done by replaying the log 
 */

#define LHT_MAX_KEY 64
#define LHT_NBUCKETS 256

struct lht_entry {
  struct lht_entry *next;
  
  char key[LHT_MAX_KEY];
  uint32_t hash;
  uint32_t len;
  uint8_t *buf;
};

struct lht_s {
  struct log_s log;
  uint64_t log_id;
  struct lht_entry **buckets;
  int nbuckets;
  
  uint32_t count;
  uint32_t memcount;
  uint32_t rehash_threshold;
};

int lht_open( char *path, struct lht_s *lht );
int lht_close( struct lht_s *lht );
int lht_reset( struct lht_s *lht );
int lht_rehash( struct lht_s *lht, int nbuckets );

int lht_put( struct lht_s *lht, char *key, char *buf, int len );
struct lht_entry *lht_get( struct lht_s *lht, char *key );
int lht_rem( struct lht_s *lht, char *key );

#endif 
