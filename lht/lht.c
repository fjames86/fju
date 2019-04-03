
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "lht.h"

static uint32_t lht_hash( char *key );
static void lht_replay( struct lht_s *lht );
static void lht_cmd_put( struct lht_s *lht, char *key, char *buf, int len );
static void lht_cmd_rem( struct lht_s *lht, char *key );

int lht_open( char *path, struct lht_s *lht ) {
  int sts;
  struct log_opts opts;

  memset( lht, 0, sizeof(*lht) );
  
  memset( &opts, 0, sizeof(opts) );
  opts.mask = LOG_OPT_FLAGS|LOG_OPT_LBACOUNT;
  opts.lbacount = (2*1024*1024) / LOG_LBASIZE;
  opts.flags = LOG_FLAG_FIXED;  
  sts = log_open( path ? path : "lgt.dat", &opts, &lht->log );
  if( sts ) return sts;

  /* setup buckets */
  lht->nbuckets = LHT_NBUCKETS;
  lht->buckets = malloc( sizeof(*lht->buckets) * lht->nbuckets );
  memset( lht->buckets, 0, sizeof(*lht->buckets) * lht->nbuckets );
  
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
      free( entry->buf );
      free( entry );
      entry = next;
    }
  }
  free( lht->buckets );

  log_close( &lht->log );
  return 0;  
}

int lht_reset( struct lht_s *lht ) {
  int i;
  struct lht_entry *entry;
  
  /* reset log */
  log_reset( &lht->log );

  /* write log entries */
  for( i = 0; i < lht->nbuckets; i++ ) {
    entry = lht->buckets[i];
    while( entry ) {
      lht_cmd_put( lht, entry->key, (char *)entry->buf, entry->len );
      entry = entry->next;
    }
  }

  return 0;
}

static int lht_put2( struct lht_s *lht, char *key, char *buf, int len, int cmd ) {
  uint32_t idx, hash;
  struct lht_entry *entry;
  
  /* update in memory, then write log entry */
  hash = lht_hash( key );
  idx = hash % lht->nbuckets;

  entry = lht->buckets[idx];
  while( entry ) {
    if( (entry->hash == hash) && (strcmp( entry->key, key ) == 0) ) {
      free( entry->buf );
      entry->buf = malloc( len );
      memcpy( entry->buf, buf, len );
      entry->len = len;
      return 0;
    }
  }

  entry = malloc( sizeof(*entry) );
  entry->hash = hash;
  strncpy( entry->key, key, sizeof(entry->key) - 1 );
  entry->buf = malloc( len );
  memcpy( entry->buf, buf, len );
  entry->len = len;

  entry->next = lht->buckets[idx];
  lht->buckets[idx] = entry;

  lht->count++;

  /* rehash if population count is over 75% */
  if( lht->count > ((lht->nbuckets * 3) / 4) ) {
    lht_rehash( lht, 0 );
  }
  
  /* write command */
  if( cmd ) lht_cmd_put( lht, key, buf, len );
  
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
  int found = 0;

  hash = lht_hash( key );
  idx = hash % lht->nbuckets;
  entry = lht->buckets[idx];
  prev = NULL;
  while( entry ) {
    if( (entry->hash == hash) && (strcmp( entry->key, key ) == 0) ) {
      if( prev ) prev->next = entry->next;
      else lht->buckets[idx] = entry->next;

      free( entry->buf );
      free( entry );
      found = 1;
      lht->count--;
      break;
    }
  }

  if( found ) {    
    if( cmd ) lht_cmd_rem( lht, key );
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
  buf = malloc( len );
  
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
	len = (cmd.len * 3) / 2;
	buf = realloc( buf, len );
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

  free( buf );
  
}


static void lht_cmd_put( struct lht_s *lht, char *key, char *buf, int len ) {
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
  
  log_write( &lht->log, &entry );  
}

static void lht_cmd_rem( struct lht_s *lht, char *key ) {
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
  
  log_write( &lht->log, &entry );  
  
}

int lht_rehash( struct lht_s *lht, int nbuckets ) {

  int n, i;
  struct lht_entry **bkts, *entry, *next;
  uint32_t idx;
  
  n = lht->nbuckets;
  bkts = lht->buckets;

  lht->nbuckets = (lht->nbuckets * 3) / 2;
  if( nbuckets > lht->nbuckets ) lht->nbuckets = nbuckets;
  lht->buckets = malloc( sizeof(*lht->buckets) * lht->nbuckets );
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

  free( bkts );
  
  return 0;
}
