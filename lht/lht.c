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
 
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#endif


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#include <fju/lht.h>

#define LHT_REHASH_THRESHOLD 75

static uint32_t lht_hash( char *key );
static void lht_replay( struct lht_s *lht );
static int lht_cmd_put( struct lht_s *lht, char *key, char *buf, int len );
static int lht_cmd_rem( struct lht_s *lht, char *key );

static void *lht_malloc( struct lht_s *lht, int count ) {
  void *p;
  
  lht->memcount += count;
  p = malloc( count );
  memset( p, 0, count );
  return p;
}
static void *lht_realloc( struct lht_s *lht, void *ptr, int oldcount, int count ) {
  void *p;
  
  lht->memcount -= oldcount;
  p = realloc( ptr, count );
  lht->memcount += count;
  
  return p;
}

static void lht_free( struct lht_s *lht, void *ptr, int count ) {
  lht->memcount -= count;
  free( ptr );
}

int lht_open( char *path, struct lht_s *lht ) {
  int sts;
  struct log_opts opts;

  memset( lht, 0, sizeof(*lht) );
  lht->rehash_threshold = LHT_REHASH_THRESHOLD;
  
  memset( &opts, 0, sizeof(opts) );
  opts.mask = LOG_OPT_FLAGS|LOG_OPT_LBACOUNT;
  opts.lbacount = (2*1024*1024) / LOG_LBASIZE;
  opts.flags = LOG_FLAG_FIXED;
  strcpy( opts.cookie, "LHT" );
  opts.mask |= LOG_OPT_COOKIE;
  sts = log_open( path ? path : "lht.dat", &opts, &lht->log );
  if( sts ) return sts;
  lht->log.flags = LOG_ASYNC; /* set flushing mode */
  
  /* setup buckets */
  lht->nbuckets = LHT_NBUCKETS;
  lht->buckets = lht_malloc( lht, sizeof(*lht->buckets) * lht->nbuckets );
  
  /* replay any existing log entries */
  lht_replay( lht );

  return 0;
}

int lht_close( struct lht_s *lht ) {
  int i;
  struct lht_entry *entry, *next;
  
  /* reset log */
  //  lht_reset( lht );
  
  /* free all entries */
  for( i = 0; i < lht->nbuckets; i++ ) {
    entry = lht->buckets[i];
    while( entry ) {
      next = entry->next;
      lht_free( lht, entry->buf, entry->len );
      lht_free( lht, entry, sizeof(*entry) );
      lht->count--;
	
      entry = next;
    }
  }
  lht_free( lht, lht->buckets, sizeof(*lht->buckets) * lht->nbuckets );

  assert( lht->memcount == 0 );
  
  log_close( &lht->log );
  return 0;  
}

int lht_reset( struct lht_s *lht ) {
  int i, sts;
  struct lht_entry *entry;
  
  /* reset log */
  log_reset( &lht->log );

  /* write log entries */
  for( i = 0; i < lht->nbuckets; i++ ) {
    entry = lht->buckets[i];
    while( entry ) {
      sts = lht_cmd_put( lht, entry->key, (char *)entry->buf, entry->len );
      if( sts ) return sts;
      entry = entry->next;
    }
  }

  return 0;
}

static int lht_put2( struct lht_s *lht, char *key, char *buf, int len, int cmd ) {
  uint32_t idx, hash;
  struct lht_entry *entry;
  int sts;
  
  /* update in memory, then write log entry */
  hash = lht_hash( key );
  idx = hash % lht->nbuckets;

  entry = lht->buckets[idx];
  while( entry ) {
    if( (entry->hash == hash) && (strcmp( entry->key, key ) == 0) ) {
      lht_free( lht, entry->buf, entry->len );
      entry->buf = lht_malloc( lht, len );
      memcpy( entry->buf, buf, len );
      entry->len = len;
      goto done;
    }
    entry = entry->next;
  }

  entry = lht_malloc( lht, sizeof(*entry) );
  entry->hash = hash;
  strncpy( entry->key, key, sizeof(entry->key) - 1 );
  entry->buf = lht_malloc( lht, len );
  memcpy( entry->buf, buf, len );
  entry->len = len;

  entry->next = lht->buckets[idx];
  lht->buckets[idx] = entry;

  lht->count++;

  /* rehash if population count is over 75% */
  if( (100 * lht->count) / lht->nbuckets > lht->rehash_threshold ) {
    lht_rehash( lht, 0 );
  }

 done:
  /* write command */
  if( cmd ) {
    sts = lht_cmd_put( lht, key, buf, len );
    if( sts ) return sts;
  }
  
  return 0;  
}
int lht_put( struct lht_s *lht, char *key, char *buf, int len ) {
  lht_replay( lht );
  return lht_put2( lht, key, buf, len, 1 );
}

struct lht_entry *lht_get( struct lht_s *lht, char *key ) {
  uint32_t hash, idx;
  struct lht_entry *entry;

  lht_replay( lht );
  
  hash = lht_hash( key );
  idx = hash % lht->nbuckets;
  entry = lht->buckets[idx];
  while( entry ) {
    if( (entry->hash == hash) && (strcmp( entry->key, key ) == 0) ) return entry;
    entry = entry->next;
  }

  return NULL;
}

static int lht_rem2( struct lht_s *lht, char *key, int cmd ) {
  /* remove from memory, then write log entry */
  uint32_t hash, idx;
  struct lht_entry *entry, *prev;
  int found = 0, sts;

  hash = lht_hash( key );
  idx = hash % lht->nbuckets;
  entry = lht->buckets[idx];
  prev = NULL;
  while( entry ) {
    if( (entry->hash == hash) && (strcmp( entry->key, key ) == 0) ) {
      if( prev ) prev->next = entry->next;
      else lht->buckets[idx] = entry->next;

      lht_free( lht, entry->buf, entry->len );
      lht_free( lht, entry, sizeof(*entry) );
      found = 1;
      lht->count--;
      break;
    }
  }

  if( found && cmd ) {    
    sts = lht_cmd_rem( lht, key );
    if( sts ) return sts;
  }

  return found ? 0 : -1;
}
int lht_rem( struct lht_s *lht, char *key ) {
  lht_replay( lht );
  return lht_rem2( lht, key, 1 );
}

static uint32_t lht_hash( char *key ) {
  int i;
  uint32_t hash = 0;

  i = 0;
  while( key[i] ) {
    hash += key[i];
    hash += hash << 10;
    hash ^= hash >> 6;
    i++;
  }
  
  hash += hash << 3;
  hash ^= hash >> 11;
  hash += hash << 15;
  
  return hash;
}


struct lht_cmd {
  uint32_t cmd;
#define LHT_CMD_PUT 1
#define LHT_CMD_REM 2
  char key[LHT_MAX_KEY];
  uint32_t len;
};

static void lht_replay( struct lht_s *lht ) {
  struct log_entry entry;
  struct log_iov iov[2];
  char *buf;
  int len, sts, ne;
  struct lht_cmd cmd;
  
  /* read from current id to end, applying each entry */
  len = 4096;
  buf = lht_malloc( lht, len );
  
  while( 1 ) {
    iov[0].buf = (char *)&cmd;
    iov[0].len = sizeof(cmd);
    entry.iov = iov;
    entry.niov = 1;

    sts = log_read( &lht->log, lht->log_id, &entry, 1, &ne );
    if( sts || !ne ) break;

    switch( cmd.cmd ) {
    case LHT_CMD_PUT:
      /* re-read with data buffer */
      if( len < cmd.len ) {
	int oldlen = len;
	len = (cmd.len * 3) / 2;
	buf = lht_realloc( lht, buf, oldlen, len );
      }
      iov[1].buf = buf;
      iov[1].len = len;
      entry.niov = 2;
      sts = log_read( &lht->log, lht->log_id, &entry, 1, &ne );
      if( sts || !ne ) break;

      lht_put2( lht, cmd.key, buf, cmd.len, 0 );
      
      break;
    case LHT_CMD_REM:
      lht_rem2( lht, cmd.key, 0 );
      break;
    }
    
    lht->log_id = entry.id;
  }

  lht_free( lht, buf, len );
  
}


static int lht_cmd_put( struct lht_s *lht, char *key, char *buf, int len ) {
  struct log_entry entry;
  struct log_iov iov[2];
  struct lht_cmd cmd;

  memset( &cmd, 0, sizeof(cmd) );
  cmd.cmd = LHT_CMD_PUT;
  strncpy( cmd.key, key, sizeof(cmd.key) - 1 );
  cmd.len = len;  
  
  memset( &entry, 0, sizeof(entry) );
  entry.flags = LOG_BINARY;

  iov[0].buf = (char *)&cmd;
  iov[0].len = sizeof(cmd);
  iov[1].buf = buf;
  iov[1].len = len;
  entry.iov = iov;
  entry.niov = 2;

  return log_write( &lht->log, &entry );  
}

static int lht_cmd_rem( struct lht_s *lht, char *key ) {
  struct log_entry entry;
  struct log_iov iov[1];
  struct lht_cmd cmd;

  memset( &cmd, 0, sizeof(cmd) );
  cmd.cmd = LHT_CMD_REM;
  strncpy( cmd.key, key, sizeof(cmd.key) - 1 );
  cmd.len = 0;  
  
  memset( &entry, 0, sizeof(entry) );
  entry.flags = LOG_BINARY;

  iov[0].buf = (char *)&cmd;
  iov[0].len = sizeof(cmd);
  entry.iov = iov;
  entry.niov = 1;
  
  return log_write( &lht->log, &entry );    
}

int lht_rehash( struct lht_s *lht, int nbuckets ) {

  int n, i;
  struct lht_entry **bkts, *entry, *next;
  uint32_t idx;
  
  n = lht->nbuckets;
  bkts = lht->buckets;

  lht->nbuckets = (lht->nbuckets * 3) / 2;
  if( nbuckets > lht->nbuckets ) lht->nbuckets = nbuckets;
  lht->buckets = lht_malloc( lht, sizeof(*lht->buckets) * lht->nbuckets );
  memset( lht->buckets, 0, sizeof(*lht->buckets) * lht->nbuckets );
  
  for( i = 0; i < n; i++ ) {
    entry = bkts[i];
    while( entry ) {
      next = entry->next;
      
      idx = entry->hash % lht->nbuckets;
      entry->next = lht->buckets[idx];
      lht->buckets[idx] = entry;
      
      entry = next;
    }
  }

  lht_free( lht, bkts, sizeof(*bkts) * n );
  
  return 0;
}
