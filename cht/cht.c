
/*
 * cht - Content addressed hash table 
 * 
 * Provides an openaddressed hash table with the following properties:
 *  - Keys are fixed opaque 16 byte array, e.g. derived from hash of data
 *  - Client chooses the key. This allows the client to stored encrypted 
 *    data and use an hmac as the key. 
 *  - Data is opaque block of up to 16k 
 *  - Entries are allocated using cuckoo hashing
 *  - This means reads can be serviced very quicky
 *  - As the population count increases, writes (actually allocations) may 
 *    require moving entries to alternate locations, this is the cuckoo hashing part.
 *  - If, after recursing upto a max number of times, no entry can be moved then
 *    an entry will be deleted.
 *  - Entries can be marked "sticky" so that they are never deleted by this process. 
 *  - If no entries can be deleted the write will fail
 *  - Once allocated, the entry data can be updated. Entries maintain a seqno to track writes.
 *  - Entries can be marked readonly to prevent subsequent writes. 
 * 
 * Intended use case is some form of distrubuted hash table (does not yet exist).
 * 
 * TODO: 
 *  - some form of RPC interface? 
 *  - store all writes into a log (only need to write the key)
 *  - use nls to distribute the log
 *  - nls clients can then update their local cht 
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include <fju/mmf.h>
#include <fju/cht.h>
#include <fju/sec.h>
#include <fju/log.h>
#include <fju/nls.h>

#include "cht-private.h"

struct cht_file {
  struct cht_prop header;
  struct cht_entry entry[1];
};

int cht_open( char *path, struct cht_s *cht, struct cht_opts *opts ) {
  int sts;
  uint32_t count;

  if( path == NULL ) path = mmf_default_path( "cht.dat", NULL );
  
  memset( cht, 0, sizeof(*cht) );
  sts = mmf_open( path, &cht->mmf );
  if( sts ) return sts;
  
  count = CHT_DEFAULT_COUNT;
  if( opts && opts->mask & CHT_OPT_COUNT ) count = opts->count;

  sts = mmf_remap( &cht->mmf, sizeof(struct cht_prop) );
  if( sts < 0 ) goto bad;
  cht->file = (struct cht_file *)cht->mmf.file;
  
  /* check header */
  if( cht->file->header.magic == 0 ) {
    memset( &cht->file->header, 0, sizeof(cht->file->header) );
    cht->file->header.magic = CHT_MAGIC;
    cht->file->header.version = CHT_VERSION;
    cht->file->header.seq = 1;
    cht->file->header.count = count;
    cht->file->header.fill = 0;
    cht->file->header.tag = time( NULL );
    if( opts && opts->mask & CHT_OPT_ALOG ) cht->file->header.alog_hshare = opts->alog_hshare;
    
    mmf_write( &cht->mmf, "", 1, sizeof(struct cht_prop) + count * (sizeof(struct cht_entry) + CHT_BLOCK_SIZE) - 1 );
  } else if( cht->file->header.magic != CHT_MAGIC ) {
    goto bad;
  } else if( cht->file->header.version != CHT_VERSION ) {
    goto bad;
  }
  
  count = cht->file->header.count;
  
  sts = mmf_remap( &cht->mmf, sizeof(struct cht_prop) + sizeof(struct cht_entry) * count );
  if( sts < 0 ) goto bad;

  cht->file = (struct cht_file *)cht->mmf.file;
  cht->count = count;

  /* set the recursion depth to something like sqrt(count) */
  {
    int c = count;
    cht->rdepth = 1;
    while( c > 0 ) {
      c >>= 2;
      cht->rdepth <<= 1;
    }
  }

  if( cht->file->header.alog_hshare ) {
    struct nls_share share;

    sts = nls_share_by_hshare( cht->file->header.alog_hshare, &share );
    if( sts ) goto bad;
    sts = nls_share_open( &share, &cht->alog );
    if( sts ) goto bad;
    log_set_cookie( &cht->alog, "CHT", 3 );
    
    cht->flags |= CHT_AUDIT;
  }
  
  return 0;
  
 bad:
  mmf_close( &cht->mmf );
  return -1;
}

int cht_close( struct cht_s *cht ) {
  mmf_close( &cht->mmf );
  if( cht->flags & CHT_AUDIT ) {
    log_close( &cht->alog );
  }
  
  return 0;
}

int cht_prop( struct cht_s *cht, struct cht_prop *prop ) {

  mmf_lock( &cht->mmf );
  *prop = cht->file->header;
  mmf_unlock( &cht->mmf );

  return 0;
}

int cht_set_alog( struct cht_s *cht, uint64_t alog_hshare ) {
  mmf_lock( &cht->mmf );
  cht->file->header.alog_hshare = alog_hshare;
  mmf_unlock( &cht->mmf );
  return 0;
}

static int cht_read_block( struct cht_s *cht, int idx, char *buf, int size ) {
  int sts;

  if( size > CHT_BLOCK_SIZE ) size = CHT_BLOCK_SIZE;
  if( idx >= cht->count ) return -1;
  
  sts = mmf_read( &cht->mmf, buf, size,
		  sizeof(struct cht_prop) +
		  (sizeof(struct cht_entry) * cht->count) +
		  (CHT_BLOCK_SIZE * idx) );
  if( sts < 0 ) return -1;
    
  return 0;
}

static int zerokey( char *key ) {
  int i;
  uint32_t *p = (uint32_t *)key;
  
  for( i = 0; i < 4; i++ ) {
    if( p[i] ) return 0;
  }
  return 1;
}

int cht_read( struct cht_s *cht, char *key, char *buf, int size, struct cht_entry *entry ) {
  int sts, i;
  uint32_t idx;
  
  mmf_lock( &cht->mmf );

  sts = -1;
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)key)[i] % cht->count;
    if( memcmp( cht->file->entry[idx].key, key, CHT_KEY_SIZE ) == 0 ) {
      sts = 0;
      if( entry ) *entry = cht->file->entry[idx];

      if( buf && size > 0 ) {
	if( size > (cht->file->entry[idx].flags & CHT_SIZE_MASK) ) {
	  size = (cht->file->entry[idx].flags & CHT_SIZE_MASK);
	}
	
	sts = cht_read_block( cht, idx, buf, size );
      }
      break;
    }
  }
  
  mmf_unlock( &cht->mmf );

  return sts;
}


static int cht_write_block( struct cht_s *cht, int idx, char *buf, int size ) {
  int sts;

  if( size > CHT_BLOCK_SIZE ) size = CHT_BLOCK_SIZE;
  if( idx >= cht->count ) return -1;

  sts = mmf_write( &cht->mmf, buf, size,
		   sizeof(struct cht_prop) +
		   (sizeof(struct cht_entry) * cht->count) +
		   (CHT_BLOCK_SIZE * idx) );
  if( sts < 0 ) return -1;
  
  return 0;
}

static int cht_evict( struct cht_s *cht, int idx, int rdepth ) {
  char *key;
  int sts, i, p;
  char buf[CHT_BLOCK_SIZE];
  
  if( rdepth >= cht->rdepth ) return -1;

  /* evict entry at index idx to one of its 3 other possible locations */

  key = (char *)cht->file->entry[idx].key;

  for( i = 0; i < 4; i++ ) {
    p = ((uint32_t *)key)[i] % cht->count;
    if( p == idx ) continue;

    if( zerokey( (char *)cht->file->entry[p].key ) ) {
      /* move to unallocated entry */
      cht->file->entry[p] = cht->file->entry[idx];
      /* delete old entry */
      memset( &cht->file->entry[idx], 0, sizeof(struct cht_entry) );

      /* move data block */
      sts = mmf_read( &cht->mmf, buf, sizeof(buf), sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * idx );
      sts = mmf_write( &cht->mmf, buf, sizeof(buf), sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * p );      

      cht->file->header.seq++;
      return 0;
    }
  }

  /* all in use, try and evict entries and use that */
  sts = -1;
  for( i = 0; i < 1/*4*/; i++ ) {
    p = ((uint32_t *)key)[i] % cht->count;
    if( p == idx ) continue;
    sts = cht_evict( cht, p, rdepth + 1 );
    if( sts == 0 ) break;
  }
  if( sts ) {
    //printf( "Unable to evict any entry, failure rdepth=%u\n", rdepth );
    int foundidx = 0;
    for( i = 0; i < 4; i++ ) {
      p = ((uint32_t *)key)[i] % cht->count;
      if( p == idx ) continue;
      if( cht->file->entry[p].flags & CHT_STICKY ) continue;
      foundidx = 1;
      break;
    }
    if( !foundidx ) return -1;
    //printf( "Evicting entry\n" );
    cht->file->header.fill--;
  }

  cht->file->entry[p] = cht->file->entry[idx];
  memset( cht->file->entry[idx].key, 0, CHT_KEY_SIZE );
  
  sts = mmf_read( &cht->mmf, buf, sizeof(buf), sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * idx );
  sts = mmf_write( &cht->mmf, buf, sizeof(buf), sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * p );      

  return 0;  
}

static void cht_alog_write( struct cht_s *cht, uint32_t op, struct cht_entry *entry ) {
  struct cht_alog_entry ae;
  struct log_entry le;
  struct log_iov iov[1];
  
  memset( &ae, 0, sizeof(ae) );
  ae.op = op;
  ae.entry = *entry;
  
  memset( &le, 0, sizeof(le) );
  le.flags = LOG_BINARY;
  iov[0].buf = (char *)&ae;
  iov[0].len = sizeof(ae);
  le.iov = iov;
  le.niov = 1;
  log_write( &cht->alog, &le );  
}

int cht_write( struct cht_s *cht, struct cht_entry *entry, char *buf, int size ) {
  int sts, i;
  uint32_t idx;

  if( size > CHT_BLOCK_SIZE ) return -1;

  if( zerokey( (char *)entry->key ) ) {
    uint8_t hash[SEC_SHA1_MAX_HASH];
    struct sec_buf iov[1];
    iov[0].buf = buf;
    iov[0].len = size;
    sha1( hash, iov, 1 );
    memcpy( entry->key, hash, CHT_KEY_SIZE );
  }
  
  mmf_lock( &cht->mmf );

  /* check each location, if unallocated then use */
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)entry->key)[i] % cht->count;
    if( zerokey( (char *)cht->file->entry[idx].key ) ||
	(memcmp( cht->file->entry[idx].key, entry->key, CHT_KEY_SIZE ) == 0) ) {

      if( zerokey( (char *)cht->file->entry[idx].key ) ) {
	cht->file->header.fill++;
	cht->file->entry[idx].seq = 0;	
      } else if( cht->file->entry[idx].flags & CHT_READONLY ) {
	/* entry marked as readonly - cannot write */
	sts = -1;
	goto done;
      }
      
      memcpy( cht->file->entry[idx].key, entry->key, CHT_KEY_SIZE );      
      cht->file->entry[idx].seq++;
      entry->seq = cht->file->entry[idx].seq;
      cht->file->entry[idx].flags = size | (entry->flags & ~CHT_SIZE_MASK);
      entry->flags = cht->file->entry[idx].flags;
      
      cht_write_block( cht, idx, buf, size );
      
      sts = 0;
      cht->file->header.seq++;

      goto done;
    }
  }
  
  /* all locations in use. Try and evict candidate locations and use that */
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)entry->key)[i] % cht->count;  
    sts = cht_evict( cht, idx, 0 );
    if( sts == 0 ) break;
  }
  if( sts ) {
    /* failed to evict? */
    goto done;
  }
  
  memcpy( cht->file->entry[idx].key, entry->key, CHT_KEY_SIZE );
  cht->file->entry[idx].seq = 1;
  entry->seq = cht->file->entry[idx].seq;
  cht->file->entry[idx].flags = (size & CHT_SIZE_MASK) | (entry->flags & ~CHT_SIZE_MASK);
  entry->flags = cht->file->entry[idx].flags;  
  cht_write_block( cht, idx, buf, size );
  
  cht->file->header.seq++;
  cht->file->header.fill++;
  sts = 0;

 done:

  if( !sts && cht->flags & CHT_AUDIT ) {
      cht_alog_write( cht, CHT_ALOG_OP_WRITE, entry );
  }

  mmf_unlock( &cht->mmf );

  return sts;
}

int cht_delete( struct cht_s *cht, char *key ) {
  int sts, i, idx;
  
  mmf_lock( &cht->mmf );

  sts = -1;
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)key)[i] % cht->count;
    if( memcmp( cht->file->entry[idx].key, key, CHT_KEY_SIZE ) == 0 ) {
      sts = 0;
      memset( cht->file->entry[idx].key, 0, CHT_KEY_SIZE );
      cht->file->entry[idx].seq = 0;
      cht->file->entry[idx].flags = 0;
      cht->file->header.fill--;
      cht->file->header.seq++;

      if( cht->flags & CHT_AUDIT ) {
	struct cht_entry entry;
	memset( &entry, 0, sizeof(entry) );
	memcpy( entry.key, key, CHT_KEY_SIZE );
	cht_alog_write( cht, CHT_ALOG_OP_DELETE, &entry );
      }
      
      break;
    }
  }
  
  mmf_unlock( &cht->mmf );

  return sts;
}

int cht_purge( struct cht_s *cht, uint32_t mask, uint32_t flags ) {
  int sts, i;
  
  mmf_lock( &cht->mmf );

  sts = -1;
  for( i = 0; i < cht->count; i++ ) {
    if( !zerokey( (char *)cht->file->entry[i].key ) && ((cht->file->entry[i].flags & mask) == flags) ) {
      memset( &cht->file->entry[i].key, 0, CHT_KEY_SIZE );
      cht->file->entry[i].seq = 0;
      cht->file->entry[i].flags = 0;
      cht->file->header.fill--;
      sts = 0;
    }
  }
  if( !sts ) {
    cht->file->header.seq++;
  }

  if( cht->flags & CHT_AUDIT ) {
    struct cht_entry entry;
    memset( &entry, 0, sizeof(entry) );
    entry.flags = (flags & 0xffff0000) | (mask >> 16);
    cht_alog_write( cht, CHT_ALOG_OP_PURGE, &entry );
  }
  
  mmf_unlock( &cht->mmf );

  return sts;
}

int cht_entry_by_index( struct cht_s *cht, int idx, uint32_t seq, struct cht_entry *entry ) {
  int sts;
  
  if( idx >= cht->count ) return -1;

  sts = -1;
  mmf_lock( &cht->mmf );

  if( !zerokey( (char *)cht->file->entry[idx].key ) ) {
    if( !seq || cht->file->entry[idx].seq == seq ) {
      *entry = cht->file->entry[idx];
      sts = 0;
    }
  }
  
  mmf_unlock( &cht->mmf );
  
  return sts;
}


int cht_set_flags( struct cht_s *cht, char *key, uint32_t mask, uint32_t flags ) {
  int sts, i;
  uint32_t idx, f;
  
  mmf_lock( &cht->mmf );

  sts = -1;
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)key)[i] % cht->count;
    if( memcmp( cht->file->entry[idx].key, key, CHT_KEY_SIZE ) == 0 ) {
      sts = 0;
      flags &= ~CHT_SIZE_MASK;
      mask &= ~CHT_SIZE_MASK;
      f = cht->file->entry[idx].flags;
      
      cht->file->entry[idx].flags = (f & CHT_SIZE_MASK) | (f & ~mask) | (flags & mask);

      if( cht->flags & CHT_AUDIT ) {
	struct cht_entry entry;
	memset( &entry, 0, sizeof(entry) );
	memcpy( entry.key, key, CHT_KEY_SIZE );
	entry.flags = (flags & 0xffff0000) | (mask >> 16);
	cht_alog_write( cht, CHT_ALOG_OP_SETFLAGS, &entry );
      }
      
      break;
    }
  }
  
  mmf_unlock( &cht->mmf );

  return sts;  
}
