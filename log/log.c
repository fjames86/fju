/*
 * MIT License
 *
 * Copyright (c) 2018 Frank James
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
#include <WinSock2.h>
#include <Windows.h>
#endif

#include <fju/log.h>

#include <stdio.h>
#include <string.h>
#include <time.h>

#define LOG_LBASIZE 64

//#define LOG_MAKE_ID(seq,idx,flags) ((seq << 32) | idx | flags)
/* reserve a few bits in the ID for control flags */
#define LOG_FLAG_MASK    0xf0000000
#define LOG_FLAG_READ    0x80000000  
#define LOG_INDEX_MASK   0x0fffffff 

#define LOG_LOCK_EX      0x109437ac
#define LOG_LOCK_UN      0x8f5cb3a9


struct _header {
  uint32_t magic;
#define LOG_MAGIC 0x109a1cdf
  uint32_t version;
#define LOG_VERSION 1

  uint32_t lbacount;  /* total number of blocks */
#define LOG_LBACOUNT 16384
  uint32_t unused;
  
  uint64_t seq;       /* seqno: increments on msg write */
  uint32_t start;     /* index of first (oldest) msg */
  uint32_t count;     /* number of blocks following index containing msgs */
  uint64_t last_id;   /* most recently written msg */
  uint32_t lock_pid; 
  uint32_t lock_mode;
  uint32_t flags;
  uint32_t unused2;
  uint64_t tag;
  char cookie[LOG_MAX_COOKIE];
  
  uint32_t spare[44]; /* future expansion */
};

struct _entry {
  uint32_t magic;      /* msg start identifier */
  uint32_t msglen;     /* length of msg data */
  uint64_t id;         /* msg identifier */
  uint64_t timestamp;  /* when msg written */
  uint32_t pid;        /* who wrote msg */
  uint32_t flags;      /* msg flags */
  uint64_t prev_id;    /* id of previous message */
  uint64_t seq;
  uint32_t ltag;       /* writer identifier */
  
  uint32_t spare[3];   /* future expansion */
};

#if 1

static int log_lock( struct log_s *log ) {
  struct _header *hdr;
  
  int sts = mmf_lock( &log->mmf );
  if( sts ) printf( "log_lock: %d\n", sts );

  hdr = (struct _header *)log->mmf.file;
  if( log->mmf.msize < (sizeof(*hdr) + LOG_LBASIZE * hdr->lbacount) ) {
    mmf_remap( &log->mmf, sizeof(*hdr) + LOG_LBASIZE * hdr->lbacount );
  }
  
  return sts;
}
static int log_unlock( struct log_s *log ) {
  int sts = mmf_unlock( &log->mmf );
  if( sts ) printf( "log_unlock: %d\n", sts );
  return sts;
}

#else

static int log_lock_cas( struct log_s *log ) {
  int b;
  struct _header *hdr = (struct _header *)log->mmf.file;
  do {
    b = __sync_bool_compare_and_swap( &hdr->lock_mode, hdr->lock_mode, LOG_LOCK_EX );
  } while( !b );
  hdr->lock_pid = log->pid;
  return 0;
}
static int log_lock( struct log_s *log ) {
  return log_lock_cas( log );
}

static int log_unlock_cas( struct log_s *log ) {
  int b;
  struct _header *hdr = (struct _header *)log->mmf.file;
  do {
    b = __sync_bool_compare_and_swap( &hdr->lock_mode, hdr->lock_mode, LOG_LOCK_UN );
  } while( !b );
  hdr->lock_pid = 0;
  //mmf_sync( &log->mmf );
  return 0;
}
static int log_unlock( struct log_s *log ) {
  return log_unlock_cas( log );
}

#endif


static struct log_s *default_log( void ) {
  static int initialized = 0;
  static struct log_s log;
  int sts;
  
  if( !initialized ) {
    sts = log_open( NULL, NULL, &log );
    if( sts ) return NULL;
    
    initialized = 1;
  }
  return &log;
}

int log_open( char *path, struct log_opts *opts, struct log_s *log ) {
  int sts;
  struct _header *hdr;
  struct log_prop prop;

  if( !log ) return -1;
  if( !path ) path = mmf_default_path( "fju.log", NULL );

  memset( log, 0, sizeof(*log) );

  sts = mmf_open( path, &log->mmf );
  if( sts ) return sts;

  sts = mmf_remap( &log->mmf, sizeof(struct _header) );
  if( sts ) goto bad;

  hdr = (struct _header *)log->mmf.file;
  
  /* check header */
  if( hdr->magic != LOG_MAGIC ) {
    hdr->magic = LOG_MAGIC;
    hdr->version = LOG_VERSION;
    hdr->lbacount = (opts && (opts->mask & LOG_OPT_LBACOUNT)) ? opts->lbacount : LOG_LBACOUNT;
    hdr->seq = 1;
    hdr->start = 0;
    hdr->count = 0;
    hdr->tag = time( NULL );
    hdr->flags = (opts && (opts->mask & LOG_OPT_FLAGS)) ? opts->flags : 0;
    if( opts && opts->mask & LOG_OPT_COOKIE ) {
      memcpy( hdr->cookie, opts->cookie, LOG_MAX_COOKIE );
    }
    
    sts = mmf_remap( &log->mmf, (sizeof(struct _header) + LOG_LBASIZE * hdr->lbacount) );
    if( sts ) goto bad;
  } else if( hdr->version != LOG_VERSION ) {
    sts = -1;
    goto bad;
  } else {
    sts = mmf_remap( &log->mmf, (sizeof(struct _header) + LOG_LBASIZE * hdr->lbacount) );
    if( sts ) goto bad;
  }

#ifdef WIN32
  log->pid = GetCurrentProcessId();
#else
  log->pid = getpid();
#endif

  if( opts && opts->mask & LOG_OPT_COOKIE ) {
    hdr = (struct _header *)log->mmf.file;
    if( memcmp( hdr->cookie, opts->cookie, LOG_MAX_COOKIE ) != 0 ) {
      /* cookie mismatch */
      goto bad;
    }
  }
  
  log_prop( log, &prop );
  if( prop.count > prop.lbacount ) {
    log_reset( log );
    log_writef( log, LOG_LVL_FATAL, "Log corruption detected - count=%d max=%d", prop.count, prop.lbacount );
  }
  if( prop.start >= prop.lbacount ) {
    log_reset( log );
    log_writef( log, LOG_LVL_FATAL, "Log corruption detected - start=%d max=%d", prop.start, prop.lbacount );
  }

  return 0;

 bad:
  mmf_close( &log->mmf );
  return -1;  
}


int log_close( struct log_s *log ) {
  if( !log ) return -1;
  
  mmf_close( &log->mmf );
  memset( log, 0, sizeof(*log) );
  return 0;
}

int log_reset( struct log_s *log ) {
  int sts;
  struct _header *hdr;

  if( !log ) log = default_log();
  if( !log ) return -1;
  
  hdr = (struct _header *)log->mmf.file;
  if( !hdr ) return -1;
  
  sts = log_lock( log );
  if( sts ) return sts;

  hdr->start = 0;
  hdr->count = 0;
  hdr->seq = 1;
  hdr->tag = time( NULL );
  hdr->last_id = 0;
  
  log_unlock( log );
  return 0;
}

int log_prop( struct log_s *log, struct log_prop *prop ) {
  struct _header *hdr;
  int sts;

  if( !log ) log = default_log();
  if( !log ) return -1;
  
  hdr = (struct _header *)log->mmf.file;
  if( !hdr ) return -1;

  sts = log_lock( log );
  if( sts ) return sts;

  prop->version = hdr->version;
  prop->seq = hdr->seq;
  prop->start = hdr->start;
  prop->count = hdr->count;
  prop->lbacount = hdr->lbacount;
  prop->last_id = hdr->last_id;
  prop->flags = hdr->flags;
  prop->tag = hdr->tag;
  memcpy( prop->cookie, hdr->cookie, LOG_MAX_COOKIE );
  
  log_unlock( log );

  return 0;
}

int log_read( struct log_s *log, uint64_t id, struct log_entry *elist, int n, int *nelist ) {
  int sts;
  char *p;
  struct _header *hdr;
  struct _entry *e;
  int i, j;
  int idx, nbytes, msgcnt, offset, blk_offset, ecount;

  if( !log ) log = default_log();
  if( !log ) return -1;
  
  if( nelist ) *nelist = 0;
  i = 0;
  hdr = (struct _header *)log->mmf.file;

  sts = log_lock( log );
  if( sts ) return sts;

  if( id == 0 ) {
    /* id=0 implies start from beginning */
    idx = hdr->start;
  } else if( id & LOG_FLAG_READ ) {
    /* if LOG_FLAG_READ bit set implies get this exact item */
    idx = (uint32_t)(id & LOG_INDEX_MASK);
    if( idx >= hdr->lbacount ) goto done;	
  } else {
    /* start from next item */
    idx = (uint32_t)(id & LOG_INDEX_MASK);
    if( idx >= hdr->lbacount ) goto done;
    p = (char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx);
    e = (struct _entry *)p;
    if( e->magic != LOG_MAGIC ) goto done;
    ecount = 1 + (e->msglen / LOG_LBASIZE) + (e->msglen % LOG_LBASIZE ? 1 : 0);
    idx = (idx + ecount) % hdr->lbacount;

    /* test if we have reached the end of the log - if so there are no more messages */
    if( idx == ((hdr->start + hdr->count) % hdr->lbacount) ) goto done;
  }

  do {
    /* check we are still in active block range */
    if( (idx + hdr->lbacount - hdr->start) % hdr->lbacount >= hdr->count ) goto done;
      
    /* lookup item at index */
    p = (char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx);
    e = (struct _entry *)p;
    if( e->magic != LOG_MAGIC ) goto done;
    
    /* all good - copy out */
    elist[i].id = e->id;
    elist[i].timestamp = e->timestamp;
    elist[i].pid = e->pid;
    elist[i].flags = e->flags;
    elist[i].msglen = e->msglen;
    elist[i].prev_id = e->prev_id;
    elist[i].seq = e->seq;
    elist[i].ltag = e->ltag;
    
    j = 0; /* iov index */
    offset = 0; /* current iov offset */
    blk_offset = 0; /* currnet block offset */
    idx = (idx + 1) % hdr->lbacount;
    msgcnt = 0;
    while( (msgcnt < e->msglen) && (j < elist[i].niov) ) {
      nbytes = LOG_LBASIZE;
      if( nbytes > (elist[i].iov[j].len - offset) ) nbytes = elist[i].iov[j].len - offset;
      if( nbytes > e->msglen - msgcnt ) nbytes = e->msglen - msgcnt;

      p = (char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx) + blk_offset;
      if( elist[i].iov[j].buf ) memcpy( elist[i].iov[j].buf + offset, p, nbytes );
      msgcnt += nbytes;
      
      offset += nbytes;
      if( offset == elist[i].iov[j].len ) {
	offset = 0;
	j++;
      }

      blk_offset += nbytes;
      if( blk_offset == LOG_LBASIZE ) {
	idx = (idx + 1) % hdr->lbacount;
	blk_offset = 0;
      }
      
    }

    /* if non-binary append a null terminator */
    if( !(elist[i].flags & LOG_BINARY) ) {
      if( (j < elist[i].niov) && (offset < elist[i].iov[j].len) ) {
	if( elist[i].iov[j].buf ) {
	  elist[i].iov[j].buf[offset] = '\0';
	  offset++;
	}
      }
    }

    /* record amount actually written to each trailing iov */
    if( j < elist[i].niov ) elist[i].iov[j].len = offset;
    j++;
    while( j < elist[i].niov ) {
      elist[i].iov[j].len = 0;
      j++;
    }
    
    i++;
   
    idx = (idx + 1) % hdr->lbacount;
  } while( (i < n) && (idx != ((hdr->start + hdr->count) % hdr->lbacount)) );

 done:
  if( nelist ) *nelist = i;
  log_unlock( log );

  return 0;
}

int log_read_end( struct log_s *log, uint64_t id, struct log_entry *elist, int n, int *nelist ) {
  struct _header *hdr = (struct _header *)log->mmf.file;
  int i, ne, sts;
  uint64_t the_id;
  int nel;

  if( !log ) log = default_log();
  if( !log ) return -1;
  
  the_id = id;  
  nel = 0;
  
  if( the_id == 0 ) {
    log_lock( log );
    the_id = hdr->last_id;
    log_unlock( log );
  }  
  if( the_id == 0 ) {
    if( nelist ) *nelist = 0;
    return 0;
  }
    
  for( i = 0; i < n; i++ ) {
    ne = 0;
    sts = log_read( log, the_id | LOG_FLAG_READ, &elist[i], 1, &ne );
    if( sts ) break;
    if( ne == 0 ) break;
    
    nel++;
    the_id = elist[i].prev_id;
    if( the_id == 0 ) break;
  }
  if( nelist ) *nelist = nel;
  
  return 0;
}

int log_read_entry( struct log_s *log, uint64_t id, struct log_entry *entry ) {
  int ne, sts;
  sts = log_read( log, id | LOG_FLAG_READ, entry, 1, &ne );
  if( sts < 0 ) return sts;
  if( ne == 0 ) return -1;
  return 0;
}

int log_read_buf( struct log_s *log, uint64_t id, char *buf, int len, int *msglen ) {
  struct log_entry entry;
  struct log_iov iov[1];
  int sts;

  if( !log ) log = default_log();
  if( !log ) return -1;
  
  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = buf;
  iov[0].len = len;
  entry.iov = iov;
  entry.niov = 1;
  sts = log_read_entry( log, id, &entry );
  if( sts ) return sts;
  
  if( msglen ) *msglen = entry.msglen;
  return 0;
}

int log_write( struct log_s *log, struct log_entry *entry ) {
  int sts;
  char *p;
  struct _entry *e;
  struct _header *hdr;
  int i, cnt, idx;
  int msglen, nbytes, offset, blk_offset, hdrlvl, ecount;

  if( !log ) log = default_log();
  if( !log ) return -1;
  
  msglen = 0;
  for( i = 0; i < entry->niov; i++ ) {
    msglen += entry->iov[i].len;
  }
  
  entry->timestamp = time( NULL );
  entry->pid = log->pid;
  hdr = (struct _header *)log->mmf.file;
  cnt = 1 + (msglen / LOG_LBASIZE);
  if( msglen % LOG_LBASIZE ) cnt++;
    
  sts = log_lock( log );
  if( sts ) return sts;

  /* check lvlmask */
  hdrlvl = (hdr->flags & LOG_FLAG_LVLMASK) >> 4;
  if( (entry->flags & LOG_LVL_MASK) < hdrlvl ) {
    /* lvl too low - discard */
    sts = 0;
    goto done;
  }
  
  /* check enough space */
  if( ((hdr->flags & (LOG_FLAG_FIXED|LOG_FLAG_GROW)) == (LOG_FLAG_FIXED|LOG_FLAG_GROW)) &&
      (cnt > (hdr->lbacount - hdr->count)) ) {
    /* remap and update header */
    uint32_t nlbacount;
    nlbacount = ((cnt > hdr->lbacount ? cnt : hdr->lbacount) * 3) / 2;    
    printf( "remapping lbacount %d->%d\n", hdr->lbacount, nlbacount );
    mmf_remap( &log->mmf, (sizeof(struct _header) + LOG_LBASIZE * nlbacount) );
    hdr = (struct _header *)log->mmf.file;
    hdr->lbacount = nlbacount;    
  } else {
    if( cnt > (int)hdr->lbacount ) {
      sts = -1;
      goto done;
    }
  }
  
  /* get next location */
  idx = (hdr->start + hdr->count) % hdr->lbacount;

  /* check we won't overwrite the current starting block */
  if( (int)(hdr->lbacount - hdr->count) < cnt ){

    if( hdr->flags & LOG_FLAG_FIXED ) {
      /* fixed log - no space left so bail out here */
      sts = -1;
      goto done;
    }
    
    do {
      /* oh dear - we will overwrite, keep advancing the starting point until there is space */      
      e = (struct _entry *)((char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * hdr->start));
      ecount = 1 + (e->msglen / LOG_LBASIZE) + (e->msglen % LOG_LBASIZE ? 1 : 0);
      hdr->start = (hdr->start + ecount) %  hdr->lbacount;
      hdr->count -= ecount;
    } while( (int)(hdr->lbacount - hdr->count) < cnt );
  }
  hdr->count += cnt;

  /* now write data at end */
  e = (struct _entry *)((char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx));
  e->magic = LOG_MAGIC;
  e->id = ((hdr->seq & 0xffffffff) << 32) | idx;
  e->timestamp = time( NULL );
  e->pid = log->pid;
  e->flags = entry->flags;
  e->msglen = msglen;
  e->prev_id = hdr->last_id;
  e->seq = hdr->seq;
  e->ltag = log->ltag ? log->ltag : entry->ltag;
  hdr->last_id = e->id;

  entry->id = e->id;
  entry->prev_id = e->prev_id;
  entry->msglen = msglen;
  entry->seq = e->seq;
  
  /* write in 64 byte chunks */
  idx = (idx + 1) % hdr->lbacount;
  i = 0; /* index into iov */  
  offset = 0; /* offset into current iov */
  blk_offset = 0; /* offset into current block */

  /* walk the iov */
  while( i < entry->niov ) {
    /* get number of bytes left to write in current block */
    nbytes = LOG_LBASIZE - blk_offset;

    /* make sure it is not more than number of bytes left in this iov */
    if( nbytes > (entry->iov[i].len - offset) ) nbytes = entry->iov[i].len - offset;

    /* copy in */
    p = ((char *)log->mmf.file) + sizeof(struct _header) + (LOG_LBASIZE * idx) + blk_offset;
    if( entry->iov[i].buf ) memcpy( p, entry->iov[i].buf + offset, nbytes );
    offset += nbytes;
    blk_offset += nbytes;

    /* if we have hit the end of this block then advance to the next one */
    if( blk_offset == LOG_LBASIZE ) {
      idx = (idx + 1) % hdr->lbacount;
      blk_offset = 0;
    }

    /* if we have hit the end of this iov buffer then advance to the next one */
    if( offset == entry->iov[i].len ) {
      i++;
      offset = 0;
    }
  }

  hdr->seq++;
  sts = 0;
  
 done:
  if( log->flags & (LOG_SYNC|LOG_ASYNC) ) log_sync( log, log->flags & LOG_SYNC ? 1 : 0 );
  
  log_unlock( log );
  return sts;
}

int log_writev( struct log_s *log, int lvl, char *fmt, va_list args ) {
  struct log_entry entry;
  struct log_iov iov[1];
  char buf[1024];

  if( !log ) log = default_log();
  if( !log ) return -1;
  
  vsnprintf( buf, sizeof(buf) - 1, fmt, args );  
  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = buf;
  iov[0].len = (int)strlen( buf );
  entry.iov = iov;
  entry.niov = 1;
  entry.flags = lvl & (~LOG_BINARY);
  return log_write( log, &entry );  
}

int log_writef( struct log_s *log, int lvl, char *fmt, ... ) {
  va_list args;
  int sts;

  if( !log ) log = default_log();
  if( !log ) return -1;
  
  va_start( args, fmt );
  sts = log_writev( log, lvl, fmt, args );
  va_end( args );
  return sts;
}

int log_write_buf( struct log_s *log, int lvl, char *buf, int len, uint64_t *id ) {
  struct log_entry entry;
  struct log_iov iov[1];
  int sts;

  if( !log ) log = default_log();
  if( !log ) return -1;
  
  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = buf;
  iov[0].len = len;
  entry.iov = iov;
  entry.niov = 1;
  entry.flags = lvl | LOG_BINARY;
  sts = log_write( log, &entry );
  if( sts ) return sts;

  if( id ) *id = entry.id;
  return 0;
}

int log_set_lvl( struct log_s *log, int lvl ) {
  struct _header *hdr;
  
  if( !log ) log = default_log();
  if( !log ) return -1;
  
  hdr = (struct _header *)log->mmf.file;
  
  log_lock( log );
  hdr->flags = (hdr->flags & ~LOG_FLAG_LVLMASK) | ((lvl & LOG_LVL_MASK) << 4);
  log_unlock( log );
  return 0;
}

int log_sync( struct log_s *log, int sync ) {
  
  if( !log ) log = default_log();
  if( !log ) return -1;
  
  mmf_sync( &log->mmf, sync );
  return 0;
}

int log_set_cookie( struct log_s *log, char *cookie, int size ) {
  struct _header *hdr;
  
  if( !log ) log = default_log();
  if( !log ) return -1;

  hdr = (struct _header *)log->mmf.file;
  if( size > LOG_MAX_COOKIE - 1 ) size = LOG_MAX_COOKIE - 1;

  log_lock( log );
  memset( hdr->cookie, 0, LOG_MAX_COOKIE );
  memcpy( hdr->cookie, cookie, size );    
  log_unlock( log );
  
  return 0;
}


int log_truncate( struct log_s *log, uint64_t id, uint32_t flags ) {
  struct _header *hdr;
  int sts;
  uint32_t idx, ecount;
  struct _entry *e;
  char *p;

  if( !log ) log = default_log();
  if( !log ) return -1;

  hdr = (struct _header *)log->mmf.file;

  sts = -1;
  log_lock( log );

  /*
   * check if this is a valid id.
   * if it is then reduce the hdr->count to point to this index
   */

  idx = (uint32_t)(id & LOG_INDEX_MASK);
  if( idx >= hdr->lbacount ) goto done;

  /* lookup item at index */
  p = (char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx);
  e = (struct _entry *)p;
  if( e->magic != LOG_MAGIC ) goto done;
  if( e->id != id ) goto done;

  if( flags & LOG_TRUNC_END ) {
    /* delete all entries before here - just adjust start andcount */
    hdr->count = (hdr->count - (idx - hdr->start) + hdr->lbacount) % hdr->lbacount;
    hdr->start = idx;
  } else {
    /* get previous id */
    id = e->prev_id;
    if( id ) {
      idx = (uint32_t)(id & LOG_INDEX_MASK);
      if( idx >= hdr->lbacount ) goto done;
      
      p = (char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx);
      e = (struct _entry *)p;
      if( e->magic != LOG_MAGIC ) goto done;
      if( e->id != id ) goto done;
    }
    
    ecount = 1 + (e->msglen / LOG_LBASIZE) + (e->msglen % LOG_LBASIZE ? 1 : 0);
    idx = (idx + ecount) % hdr->lbacount;
    
    /* id is correct, truncate to here */
    hdr->count = (idx - hdr->start + hdr->lbacount) % hdr->lbacount;
    //hdr->seq++;
    hdr->last_id = id;
  }

  sts = 0;
 done:
  log_unlock( log );

  return sts;
}
