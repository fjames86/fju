
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

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
    memset( &cht->file->header, 0, sizeof(cht->file->header) );
    cht->file->header.magic = CHT_MAGIC;
    cht->file->header.version = CHT_VERSION;
    cht->file->header.seq = 1;
    cht->file->header.count = count;
    if( opts && opts->mask & CHT_OPT_ENCRYPT ) {
      uint8_t hash[20];
      struct sec_buf iov[1];
      
      cht->file->header.flags |= CHT_ENCRYPT;
      memcpy( cht->key, opts->key, 16 );

      iov[0].buf = (char *)cht->key;
      iov[0].len = 16;
      sha1( hash, iov, 1 );
      memcpy( cht->file->header.keyhash, hash, 16 );
    }
    mmf_write( &cht->mmf, "", 1, sizeof(struct cht_prop) + count * (sizeof(struct cht_entry) + CHT_BLOCK_SIZE) );
  } else if( cht->file->header.magic != CHT_MAGIC ) {
    goto bad;
  } else if( cht->file->header.version != CHT_VERSION ) {
    goto bad;
  } else if( (cht->file->header.flags & CHT_ENCRYPT) ) {
    uint8_t hash[20];
    struct sec_buf iov[1];
    
    if( !(opts && opts->mask & CHT_OPT_ENCRYPT) ) goto bad;
    
    iov[0].buf = (char *)opts->key;
    iov[0].len = 16;
    sha1( hash, iov, 1 );
    if( memcmp( hash, cht->file->header.keyhash, 16 ) != 0 ) goto bad;
    
  } else {
    count = cht->file->header.count;
  }
  
  sts = mmf_remap( &cht->mmf, sizeof(struct cht_prop) + sizeof(struct cht_entry) * count );
  if( sts < 0 ) goto bad;

  cht->file = (struct cht_file *)cht->mmf.file;
  cht->count = count;
  cht->rdepth = (int)sqrt( (double)count );
  
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
  int sts;

  sts = mmf_read( &cht->mmf, buf, size, sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * idx );
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

int cht_read( struct cht_s *cht, char *key, struct cht_entry *entry, char *buf, int size ) {
  int sts, i;
  uint32_t idx;
  char ibuf[CHT_BLOCK_SIZE];
  
  mmf_lock( &cht->mmf );

  sts = -1;
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)key)[i] % cht->count;
    if( memcmp( cht->file->entry[idx].key, key, CHT_KEY_SIZE ) == 0 ) {
      sts = 0;
      if( entry ) *entry = cht->file->entry[idx];
      if( buf && size > 0 ) {
	if( size > (cht->file->entry[idx].flags & CHT_SIZE_MASK) ) size = (cht->file->entry[idx].flags & CHT_SIZE_MASK);
	sts = cht_read_block( cht, idx, ibuf, CHT_BLOCK_SIZE );
	if( cht->file->header.flags & CHT_ENCRYPT ) {
	  int s = size;
	  if( s % 16 ) s += 16 - (s % 16);
	  aes_decrypt( cht->key, (uint8_t *)ibuf, s );
	}
	memcpy( buf, ibuf, size );
      }
      break;
    }
  }
  
  mmf_unlock( &cht->mmf );

  return sts;
}

static void cht_hash( struct cht_s *cht, char *key, char *buf, int size ) {
  char hash[SEC_SHA1_MAX_HASH];
  struct sec_buf iov[1];

  iov[0].len = size;
  iov[0].buf = buf;
  
  if( cht->file->header.flags & CHT_ENCRYPT ) {
    sha1_hmac( (uint8_t *)hash, cht->key, iov, 1 );
  } else {
    sha1( (uint8_t *)hash, iov, 1 );
  }
  
  memcpy( key, hash, CHT_KEY_SIZE );
}

static int cht_write_block( struct cht_s *cht, int idx, char *buf, int size ) {
  int sts;  
  sts = mmf_write( &cht->mmf, buf, size, sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * idx );
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
      memcpy( cht->file->entry[p].key, cht->file->entry[idx].key, CHT_KEY_SIZE );
      cht->file->entry[p].seq++;
      cht->file->entry[p].flags = cht->file->entry[idx].flags;
      memset( cht->file->entry[idx].key, 0, CHT_KEY_SIZE );
      
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
  
  memcpy( cht->file->entry[p].key, cht->file->entry[idx].key, CHT_KEY_SIZE );
  cht->file->entry[p].seq++;
  cht->file->entry[p].flags = cht->file->entry[idx].flags;
  memset( cht->file->entry[idx].key, 0, CHT_KEY_SIZE );

  sts = mmf_read( &cht->mmf, buf, sizeof(buf), sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * idx );
  sts = mmf_write( &cht->mmf, buf, sizeof(buf), sizeof(struct cht_prop) + sizeof(struct cht_entry) * cht->count + CHT_BLOCK_SIZE * p );      

  return 0;  
}

int cht_write( struct cht_s *cht, char *buf, int size, uint32_t flags, struct cht_entry *entry ) {
  int sts, i, s;
  char key[CHT_KEY_SIZE];
  uint32_t idx;
  char ibuf[CHT_BLOCK_SIZE];

  if( size > CHT_BLOCK_SIZE ) return -1;

  memcpy( ibuf, buf, size );
  memset( ibuf + size, 0, CHT_BLOCK_SIZE - size );
  s = size;    
  if( s % 16 ) s += 16 - (s % 16);
  
  if( cht->file->header.flags & CHT_ENCRYPT ) {
    aes_encrypt( cht->key, (uint8_t *)ibuf, s );
  }   
  cht_hash( cht, key, ibuf, s );
  
  mmf_lock( &cht->mmf );

  /* check each location, if unallocated then use */
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)key)[i] % cht->count;
    if( zerokey( (char *)cht->file->entry[idx].key ) ||
	(memcmp( cht->file->entry[idx].key, key, CHT_KEY_SIZE ) == 0) ) {

      if( zerokey( (char *)cht->file->entry[idx].key ) ) {
	cht->file->header.fill++;
      }
      
      memcpy( cht->file->entry[idx].key, key, CHT_KEY_SIZE );
      cht->file->entry[idx].seq++;
      cht->file->entry[idx].flags = size | (flags & ~CHT_SIZE_MASK);
      cht_write_block( cht, idx, ibuf, CHT_BLOCK_SIZE );
      
      if( entry ) *entry = cht->file->entry[idx];
      sts = 0;
      cht->file->header.seq++;

      goto done;
    }
  }
  
  /* all locations in use. Try and evict candidate locations and use that */
  for( i = 0; i < 4; i++ ) {
    idx = ((uint32_t *)key)[i] % cht->count;  
    sts = cht_evict( cht, idx, 0 );
    if( sts == 0 ) break;
  }
  if( sts ) {
    /* failed to evict? */
    goto done;
  }
  
  memcpy( cht->file->entry[idx].key, key, CHT_KEY_SIZE );
  cht->file->entry[idx].seq++;
  cht->file->entry[idx].flags = (size & CHT_SIZE_MASK) | (flags & ~CHT_SIZE_MASK);
  cht_write_block( cht, idx, ibuf, CHT_BLOCK_SIZE );
  
  if( entry ) *entry = cht->file->entry[idx];
  cht->file->header.seq++;
  cht->file->header.fill++;
  sts = 0;
  
 done:
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
      cht->file->entry[idx].seq++;
      cht->file->entry[idx].flags = 0;
      cht->file->header.fill--;
      cht->file->header.seq++;
      break;
    }
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


  
