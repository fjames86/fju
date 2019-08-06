/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
*/
 
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
