
#include <stdlib.h>
#include <string.h>

#include <fju/ftab.h>

#define FTAB_LBASIZE     64
#define FTAB_LBACOUNT    4096 

struct ftab_header {
  uint32_t magic;
#define FTAB_MAGIC 0xf7a6494c
  uint32_t version;
  uint64_t seq;
  uint32_t max;
  uint32_t count;
  uint32_t lbasize;
  
  uint32_t spare[57];
};

struct ftab_file {
  struct ftab_header header;
  struct ftab_entry entry[1];
};

int ftab_open( char *path, struct ftab_opts *opts, struct ftab_s *ftab ) {
  int sts, i;
  struct ftab_file *f;
  struct ftab_entry *e;
  
  memset( ftab, 0, sizeof(*ftab) );
  
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

    sts = mmf_remap( &ftab->mmf, sizeof(f->header) + sizeof(struct ftab_entry) * f->header.max + (f->header.lbasize * f->header.max) );
    if( sts ) goto bad;
    for( i = 0; i < f->header.max; i++ ) {
      e = &f->entry[i];
      e->id = 0;
      e->blkidx = i;
      e->seq = 1;
      e->flags = 0;
      e->refcount = 0;
      e->nextid = 0;
    }
  } else if( f->header.version != FTAB_VERSION ) {
    goto bad;
  } else {
      sts = mmf_remap( &ftab->mmf, sizeof(f->header) + sizeof(struct ftab_entry) * f->header.max + (f->header.lbasize * f->header.max) );
      if( sts ) goto bad;
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
  ftab_unlock( ftab );
  return 0;
}

int ftab_list( struct ftab_s *ftab, struct ftab_entry *elist, int n ) {
  int i;
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  
  ftab_lock( ftab );
  for( i = 0; i < f->header.count; i++ ) {
    if( i < n ) {
      elist[i] = f->entry[i];
    }
  }
  i = f->header.count;
  ftab_unlock( ftab );
  
  return i;
}

int ftab_entry_by_id( struct ftab_s *ftab, uint64_t id, struct ftab_entry *entry ) {
    int sts, i;
    struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;

    sts = -1;
    ftab_lock( ftab );
    for( i = 0; i < f->header.count; i++ ) {
	if( f->entry[i].id == id ) {
	    if( entry ) *entry = f->entry[i];
	    sts = 0;
	    break;
	}
    }
    ftab_unlock( ftab );
    
    return sts;
}

int ftab_alloc( struct ftab_s *ftab, uint64_t *id ) {
  int sts, i;
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;

  sts = -1;
  
  ftab_lock( ftab );
  if( f->header.count < f->header.max ) {
    i = f->header.count;
    f->entry[i].seq++;
    f->entry[i].id = (f->header.seq << 32) | f->entry[i].blkidx;
    f->entry[i].flags = 0;
    f->entry[i].refcount = 1;
    f->entry[i].nextid = 0;
    f->header.seq++;
    f->header.count++;
    if( id ) *id = f->entry[i].id;
    sts = 0;
  }
  ftab_unlock( ftab );
  
  return sts;
}

int ftab_acquire( struct ftab_s *ftab, uint64_t id ) {
  int sts, i;
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  
  sts = -1;  
  ftab_lock( ftab );
  for( i = 0; i < f->header.count; i++ ) {
    if( f->entry[i].id == id ) {
      f->entry[i].seq++;
      f->entry[i].refcount++;
      f->header.seq++;      
      sts = 0;
      break;
    }
  }
  ftab_unlock( ftab );
  
  return sts;
}

int ftab_free( struct ftab_s *ftab, uint64_t id ) {
  int sts, i;
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  struct ftab_entry tmpe;
  
  sts = -1;  
  ftab_lock( ftab );
  for( i = 0; i < f->header.count; i++ ) {
    if( f->entry[i].id == id ) {
      f->entry[i].seq++;
      f->entry[i].refcount--;

      if( f->entry[i].refcount == 0 ) {	
	if( i != (f->header.count - 1) ) {
	  tmpe = f->entry[i];
	  f->entry[i] = f->entry[f->header.count - 1];
	  f->entry[f->header.count - 1] = tmpe;
	}
	f->header.count--;
	f->header.seq++;
      }
      
      sts = 0;
      break;
    }
  }
  ftab_unlock( ftab );
  
  return sts;
}

int ftab_read( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset ) {
  int sts, i, nbytes;
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  char *p;

  /* TODO: offset */
  
  sts = -1;  
  ftab_lock( ftab );
  for( i = 0; i < f->header.count; i++ ) {
    if( f->entry[i].id == id ) {
      p = ((char *)f) + sizeof(f->header) + (sizeof(struct ftab_entry) * f->header.max) + (f->header.lbasize * f->entry[i].blkidx);
      nbytes = n;
      if( nbytes > f->header.lbasize ) nbytes = f->header.lbasize;
      memcpy( buf, p, nbytes );
      sts = 0;
      break;
    }
  }
  ftab_unlock( ftab );
  
  return sts;  
}

int ftab_write( struct ftab_s *ftab, uint64_t id, char *buf, int n, uint32_t offset ) {
  int sts, i, nbytes;
  struct ftab_file *f = (struct ftab_file *)ftab->mmf.file;
  char *p;

  /* TODO: offset */
  
  sts = -1;  
  ftab_lock( ftab );
  for( i = 0; i < f->header.count; i++ ) {
    if( f->entry[i].id == id ) {
      p = ((char *)f) + sizeof(f->header) + (sizeof(struct ftab_entry) * f->header.max) + (f->header.lbasize * f->entry[i].blkidx);
      nbytes = n;
      if( nbytes > f->header.lbasize ) nbytes = f->header.lbasize;
      memcpy( p, buf, nbytes );
      f->entry[i].seq++;
      f->header.seq++;
      sts = 0;
      break;
    }
  }
  ftab_unlock( ftab );
  
  return sts;  
}
