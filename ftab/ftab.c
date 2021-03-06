
#include <stdlib.h>
#include <string.h>

#include <fju/ftab.h>
#include <fju/sec.h>

#include "ftab-private.h"

/* these parameters give a default 1MB file */
#define FTAB_LBASIZE     64          /* data block size */
#define FTAB_LBACOUNT    14560       /* number of blocks */

/* 8 byte entry */
struct ftab_entry {
  uint32_t flags;
#define FTAB_ACTIVE      0x0001 
  uint32_t seq;
};

struct ftab_header {
  uint32_t magic;
#define FTAB_MAGIC 0xf7a6494c
  uint32_t version;
  uint64_t seq;
  uint32_t max;
  uint32_t count;
  uint32_t lbasize;

  uint32_t cookie[FTAB_MAX_COOKIE];
  
  uint32_t spare[41];
};

struct ftab_file {
  struct ftab_header header;
  struct ftab_entry entry[1];
};

static struct ftab_entry *ftab_get_entry( struct ftab_s *ftab, uint64_t id );

int ftab_open( char *path, struct ftab_opts *opts, struct ftab_s *ftab ) {
  int sts, i;
  struct ftab_file *f;
  struct ftab_entry *e;
  
  memset( ftab, 0, sizeof(*ftab) );

  if( !path ) path = mmf_default_path( "ftab.dat", NULL );
  sts = mmf_open( path, &ftab->mmf );
  if( sts ) return sts;

  sts = mmf_remap( &ftab->mmf, sizeof(f->header) );
  if( sts ) goto bad;
  f = ftab->mmf.file;
  
  if( f->header.magic != FTAB_MAGIC ) {
    f->header.magic = FTAB_MAGIC;
    f->header.version = FTAB_VERSION;
    f->header.seq = 1;
    f->header.count = 0;
    f->header.max = (opts && (opts->mask & FTAB_OPT_LBACOUNT)) ? opts->lbacount : FTAB_LBACOUNT;
    f->header.lbasize = (opts && (opts->mask & FTAB_OPT_LBASIZE)) ? opts->lbasize : FTAB_LBASIZE;
    if( opts && (opts->mask & FTAB_OPT_COOKIE) ) memcpy( f->header.cookie, opts->cookie, FTAB_MAX_COOKIE );
    
    sts = mmf_remap( &ftab->mmf, sizeof(f->header) + sizeof(struct ftab_entry) * f->header.max + (f->header.lbasize * f->header.max) );    
    if( sts ) goto bad;
    f = ftab->mmf.file;
    for( i = 0; i < f->header.max; i++ ) {
      e = &f->entry[i];
      e->flags = 0;
      e->seq = sec_rand_uint32();
    }
  } else if( f->header.version != FTAB_VERSION ) {
    goto bad;
  } else {
      sts = mmf_remap( &ftab->mmf, sizeof(f->header) + sizeof(struct ftab_entry) * f->header.max + (f->header.lbasize * f->header.max) );
      if( sts ) goto bad;
      f = ftab->mmf.file;
  }
    
  return 0;
  
 bad:
  mmf_close( &ftab->mmf );
  return -1;  
}

int ftab_close( struct ftab_s *ftab ) {
  mmf_close( &ftab->mmf );
  return 0;
}

static void ftab_lock( struct ftab_s *ftab ) {
  mmf_lock( &ftab->mmf );
}

static void ftab_unlock( struct ftab_s *ftab ) {
  mmf_unlock( &ftab->mmf );
}

int ftab_prop( struct ftab_s *ftab, struct ftab_prop *prop ) {
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  
  ftab_lock( ftab );
  prop->version = f->header.version;
  prop->seq = f->header.seq;
  prop->max = f->header.max;
  prop->count = f->header.count;
  prop->lbasize = f->header.lbasize;
  memcpy( prop->cookie, f->header.cookie, FTAB_MAX_COOKIE );
  ftab_unlock( ftab );
  return 0;
}

int ftab_set_cookie( struct ftab_s *ftab, char *cookie ) {
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  ftab_lock( ftab );
  memcpy( f->header.cookie, cookie, FTAB_MAX_COOKIE );
  ftab_unlock( ftab );
  return 0;
}

int ftab_reset( struct ftab_s *ftab ) {
    int i;
    struct ftab_entry *e;
    struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
    
    ftab_lock( ftab );
    
    f->header.magic = FTAB_MAGIC;
    f->header.version = FTAB_VERSION;
    f->header.seq = 1;
    f->header.count = 0;

    for( i = 0; i < f->header.max; i++ ) {
      e = &f->entry[i];
      e->flags = 0;
      e->seq = sec_rand_uint32();
    }

    ftab_unlock( ftab );

    return 0;
}

int ftab_list( struct ftab_s *ftab, uint64_t *id, int n ) {
  int i, idx;
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  
  ftab_lock( ftab );
  idx = 0;
  for( i = 0; i < f->header.max; i++ ) {
      if( f->entry[i].flags & FTAB_ACTIVE ) {
	  if( idx < n ) {
	    id[idx] = ((uint64_t)f->entry[i].seq) << 32 | i;
	  }
	  idx++;
      }
  }
  ftab_unlock( ftab );
  
  return idx;
}

int ftab_alloc( struct ftab_s *ftab, uint64_t *id ) {
  int sts, i;
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  struct ftab_entry *e;
  uint32_t startidx;
  
  sts = -1;
  
  ftab_lock( ftab );
  startidx = sec_rand_uint32() % f->header.max;  
  e = NULL;
  for( i = 0; i < f->header.max; i++ ) {
      if( (f->entry[(startidx + i) % f->header.max].flags & FTAB_ACTIVE) == 0 ) {
	  e = &f->entry[(startidx + i) % f->header.max];
	  i = (startidx + i) % f->header.max;
	  break;
      }
  }
  if( e ) {
      e->seq++;
      if( e->seq == 0 ) e->seq = 1;
      e->flags |= FTAB_ACTIVE;
      f->header.seq++;
      f->header.count++;
      if( id ) *id = (((uint64_t)e->seq) << 32) | i;

      sts = 0;
  }
  ftab_unlock( ftab );
  
  return sts;
}

int ftab_free( struct ftab_s *ftab, uint64_t id ) {
  int sts;
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  struct ftab_entry *e;
 
  sts = -1;  
  ftab_lock( ftab );
  e = ftab_get_entry( ftab, id );
  if( e ) {
      e->flags &= ~FTAB_ACTIVE;
      e->seq++;
      if( e->seq == 0 ) e->seq = 1;
      sts = 0;
      f->header.count--;
      f->header.seq++;
  }
  ftab_unlock( ftab );
  
  return sts;
}

static int ftab_read_block( struct ftab_s *ftab, uint32_t idx, char *buf, int n, uint32_t offset ) {
    struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
    int nbytes;
    uint64_t off;
  
    off = sizeof(struct ftab_header) + (sizeof(struct ftab_entry) * f->header.max) + (idx * f->header.lbasize) + offset;
    nbytes = n;
    if( nbytes > (f->header.lbasize - offset) ) nbytes = f->header.lbasize - offset;
    memcpy( buf, ((char *)f) + off, nbytes );
    return nbytes;
}

static int ftab_write_block( struct ftab_s *ftab, uint32_t idx, char *buf, int n, uint32_t offset ) {
    struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
    int nbytes;
    uint64_t off;
  
    off = sizeof(struct ftab_header) + (sizeof(struct ftab_entry) * f->header.max) + (idx * f->header.lbasize) + offset;
    nbytes = n;
    if( nbytes > (f->header.lbasize - offset) ) nbytes = f->header.lbasize - offset;
    memcpy( ((char *)f) + off, buf, nbytes );
    return nbytes;
}

static struct ftab_entry *ftab_get_entry( struct ftab_s *ftab, uint64_t id ) {
    uint32_t idx, seq;
    struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
    
    idx = id & 0xffffffff;
    if( idx >= f->header.max ) return NULL;
    
    seq = (id >> 32) & 0xffffffff;
    if( (f->entry[idx].flags & FTAB_ACTIVE) && (f->entry[idx].seq == seq) ) {
	return &f->entry[idx];
    }
    return NULL;
}

int ftab_read( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset ) {
    uint32_t nbytes;
    struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
    struct ftab_entry *e;
    
    e = ftab_get_entry( ftab, id );
    if( e ) {
	if( offset > f->header.lbasize ) offset = f->header.lbasize;
	nbytes = f->header.lbasize - offset;
	if( nbytes > n ) nbytes = n;    
	ftab_read_block( ftab, id & 0xffffffff, buf, nbytes, offset );
    } else {
	nbytes = -1;
    }

    return nbytes;
}

int ftab_write( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset ) {
    uint32_t nbytes;
    struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
    struct ftab_entry *e;

    e = ftab_get_entry( ftab, id );
    if( e ) {
	if( offset > f->header.lbasize ) offset = f->header.lbasize;
	nbytes = f->header.lbasize - offset;
	if( nbytes > n ) nbytes = n;    
	ftab_write_block( ftab, id & 0xffffffff, buf, nbytes, offset );
    } else {
	nbytes = -1;
    }

    return nbytes;
}

int ftab_sync( struct ftab_s *ftab, int sync ) {
  mmf_sync( &ftab->mmf, sync );
  return 0;
}

