
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include <fju/mmf.h>
#include <fju/cht.h>
#include <fju/sec.h>

struct cht_file {
  struct cht_prop header;
  struct cht_entry entry[1];
};

int cht_open( char *path, struct cht_s *cht, struct cht_opts *opts ) {
  int sts;
  uint32_t count;
  
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
    cht->file->header.magic = CHT_MAGIC;
    cht->file->header.version = CHT_VERSION;
    cht->file->header.seq = 1;
    cht->file->header.count = count;
  } else if( cht->file->header.magic != CHT_MAGIC ) {
    goto bad;
  } else if( cht->file->header.version != CHT_VERSION ) {
    goto bad;
  } else {
    count = cht->file->header.count;
  }
        
  sts = mmf_remap( &cht->mmf, sizeof(struct cht_prop) + sizeof(struct cht_entry) * count );
  if( sts < 0 ) goto bad;

  cht->file = (struct cht_file *)cht->mmf.file;
  cht->count = count;
  
  return 0;
  
 bad:
  mmf_close( &cht->mmf );
  return -1;
}

int cht_close( struct cht_s *cht ) {
  mmf_close( &cht->mmf );
  return 0;
}

int cht_prop( struct cht_s *cht, struct cht_prop *prop ) {

  mmf_lock( &cht->mmf );
  *prop = cht->file->header;
  mmf_unlock( &cht->mmf );

  return 0;
}

static int cht_read_block( struct cht_s *cht, int idx, char *buf, int size ) {
  return mmf_read( &cht->mmf, buf, size, sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * idx );  
}

int cht_read( struct cht_s *cht, char *key, struct cht_entry *entry, char *buf, int size ) {
  int sts, i;
  uint32_t idx;
  
  mmf_lock( &cht->mmf );

  sts = -1;
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)key)[i] % cht->count;
    if( memcmp( cht->file->entry[idx].key, key, CHT_KEY_SIZE ) == 0 ) {
      if( entry ) *entry = cht->file->entry[idx];
      if( buf && size > 0 ) {
	if( size > (cht->file->entry[idx].flags & CHT_SIZE_MASK) ) size = (cht->file->entry[idx].flags & CHT_SIZE_MASK);
	cht_read_block( cht, idx, buf, size );
      }
      break;
    }
  }

  sts = 0;
  mmf_unlock( &cht->mmf );

  return sts;
}

static void cht_hash( char *key, char *buf, int size ) {
  char hash[SEC_SHA1_MAX_HASH];
  struct sec_buf iov[1];

  iov[0].len = size;
  iov[0].buf = buf;
  sha1( (uint8_t *)hash, iov, 1 );
  memcpy( key, hash, CHT_KEY_SIZE );
}

#define CHT_MAX_RDEPTH 30

static int cht_write_block( struct cht_s *cht, int idx, char *buf, int size ) {
  return mmf_write( &cht->mmf, buf, size, sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * idx );  
}

static int cht_evict( struct cht_s *cht, int idx, int rdepth ) {
  char *key;
  int sts, i, p;
  char buf[CHT_BLOCK_SIZE];
  
  if( rdepth >= CHT_MAX_RDEPTH ) return -1;

  /* evict entry at index idx to one of its 3 other possible locations */

  key = (char *)cht->file->entry[idx].key;

  for( i = 0; i < 4; i++ ) {
    p = ((uint32_t *)key)[i] % cht->count;
    if( p == idx ) continue;

    if( cht->file->entry[p].seq == 0 ) {
      /* unallocated entry */
      cht->file->entry[p] = cht->file->entry[idx];
      memset( buf, 0, sizeof(buf) );
      cht_read_block( cht, idx, buf, sizeof(buf) );
      cht_write_block( cht, p, buf, sizeof(buf) );
      return 0;
    }
  }

  /* all in use, evict entry 0 and use that */
  p = ((uint32_t *)key)[0] % cht->count;  
  sts = cht_evict( cht, p, rdepth + 1 );
  if( sts ) {
    /* can't evict this block, overwrite it */
  }
  
  cht->file->entry[p] = cht->file->entry[idx];
  memset( buf, 0, sizeof(buf) );
  cht_read_block( cht, idx, buf, sizeof(buf) );
  cht_write_block( cht, p, buf, sizeof(buf) );
  
  return 0;  
}

int cht_write( struct cht_s *cht, char *buf, int size, uint32_t flags, struct cht_entry *entry ) {
  int sts, i;
  char key[CHT_KEY_SIZE];
  uint32_t idx;

  if( size > CHT_BLOCK_SIZE ) return -1;
  
  cht_hash( key, buf, size );

  mmf_lock( &cht->mmf );

  /* check each location, if unallocated then use */
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)key)[i] % cht->count;
    if( cht->file->entry[idx].seq == 0 ) {
      memcpy( cht->file->entry[idx].key, key, CHT_KEY_SIZE );
      cht->file->entry[idx].seq = 1;
      cht->file->entry[idx].flags = (size & CHT_SIZE_MASK) | (entry != NULL ? entry->flags : 0);
      cht_write_block( cht, idx, buf, size );
      
      if( entry ) *entry = cht->file->entry[idx];
      sts = 0;
      goto done;
    }
  }

  /* all locations in use. Evict first candidate location and use that */
  idx = ((uint32_t *)key)[0] % cht->count;  
  sts = cht_evict( cht, ((uint32_t *)key)[idx] % cht->count, 0 );
  if( sts ) {
    /* failed to evict? */
    sts = -1;
    goto done;
  }
  
  memcpy( cht->file->entry[idx].key, key, CHT_KEY_SIZE );
  cht->file->entry[idx].seq = 1;
  cht->file->entry[idx].flags = (size & CHT_SIZE_MASK) | flags;
  mmf_write( &cht->mmf, buf, size, sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * idx );
  
  if( entry ) *entry = cht->file->entry[idx];
  
 done:
  mmf_unlock( &cht->mmf );

  return sts;
}

