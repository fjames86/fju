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

#include "log.h"

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
#define LOG_MAGIC 0x109a1cde
  uint32_t version;
#define LOG_VERSION 1

  uint32_t lbacount;  /* total number of blocks */
#define LOG_LBACOUNT 16384

  uint64_t seq;       /* seqno: increments on msg write */
  uint32_t start;     /* index of first (oldest) msg */
  uint32_t count;     /* number of blocks following index containing msgs */
  uint64_t last_id;   /* most recently written msg */
  uint32_t lock_pid; 
  uint32_t lock_mode;
  uint32_t flags;
  
  uint32_t spare[51]; /* future expansion */
};

struct _entry {
  uint32_t magic;      /* msg start identifier */
  uint32_t count;      /* number of blocks, including header */
  uint64_t id;         /* msg identifier */
  uint64_t timestamp;  /* when msg written */
  uint32_t pid;        /* who wrote msg */
  uint32_t flags;      /* msg flags */
  uint32_t msglen;     /* length of msg data */
  uint64_t prev_id;    /* id of previous message */

  uint8_t spare[20];   /* future expansion */
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

int log_open( char *path, struct log_opts *opts, struct log_s *log ) {
  int sts;
  struct _header *hdr;

  if( !path ) return -1;

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
    hdr->flags = (opts && (opts->mask & LOG_OPT_FLAGS)) ? opts->flags : 0;
    
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

  return 0;

 bad:
  mmf_close( &log->mmf );
  return -1;  
}


int log_close( struct log_s *log ) {
  mmf_close( &log->mmf );
  memset( log, 0, sizeof(*log) );
  return 0;
}

int log_reset( struct log_s *log ) {
  int sts;
  struct _header *hdr;

  hdr = (struct _header *)log->mmf.file;
  if( !hdr ) return -1;
  
  sts = log_lock( log );
  if( sts ) return sts;

  hdr->start = 0;
  hdr->count = 0;
  hdr->seq = 1;
  
  log_unlock( log );
  return 0;
}

int log_prop( struct log_s *log, struct log_prop *prop ) {
  struct _header *hdr;
  int sts;

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
  
  log_unlock( log );

  return 0;
}

int log_read( struct log_s *log, uint64_t id, struct log_entry *elist, int n, int *nelist ) {
  int sts;
  struct _header *hdr;
  char *p, *q;
  struct _entry *e;
  int i, j, k, len;
  int idx, nbytes, msgcnt, offset, blk_offset;

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
    idx = (idx + e->count) % hdr->lbacount;

    /* test if we have reached the end of the log - if so there are no more messages */
    if( idx == ((hdr->start + hdr->count) % hdr->lbacount) ) goto done;
  }

  do {
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
      memcpy( elist[i].iov[j].buf + offset, p, nbytes );
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
	elist[i].iov[j].buf[offset] = '\0';
      }
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
  
  the_id = id;  
  nel = 0;
  
  if( the_id == 0 ) {
    log_lock( log );
    the_id = hdr->last_id;
    log_unlock( log );
  }  
  if( the_id == 0 ) return -1;

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
  int sts, ne;
  
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
  char *p, *q;
  struct _entry *e;
  struct _header *hdr;
  int i, j, k, len, cnt;
  int idx;
  int msglen, nbytes, offset, blk_offset;

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
    if( cnt > (int)hdr->lbacount ) goto done;
  }
  
  /* get next location */
  idx = (hdr->start + hdr->count) % hdr->lbacount;

  /* check we won't overwrite the current starting block */
  if( (int)(hdr->lbacount - hdr->count) < cnt ){

    if( hdr->flags & LOG_FLAG_FIXED ) {
      /* fixed log - no space left so bail out here */
      goto done;
    }
    
    do {
      /* oh dear - we will overwrite, keep advancing the starting point until there is space */
      e = (struct _entry *)((char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * hdr->start));
      hdr->start = (hdr->start + e->count) %  hdr->lbacount;
      hdr->count -= e->count;
    } while( (int)(hdr->lbacount - hdr->count) < cnt );
  }
  hdr->count += cnt;
  hdr->seq++;

  /* now write data at end */
  e = (struct _entry *)((char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx));
  e->magic = LOG_MAGIC;
  e->count = cnt;
  e->id = ((hdr->seq & 0xffffffff) << 32) | idx;
  e->timestamp = time( NULL );
  e->pid = log->pid;
  e->flags = entry->flags;
  e->msglen = msglen;
  e->prev_id = hdr->last_id;
  hdr->last_id = e->id;
  
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
    memcpy( p, entry->iov[i].buf + offset, nbytes );
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

 done:
  log_unlock( log );
  return 0;
}

int log_writev( struct log_s *log, int lvl, char *fmt, va_list args ) {
  struct log_entry entry;
  struct log_iov iov[1];
  char buf[1024];

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
  va_start( args, fmt );
  sts = log_writev( log, lvl, fmt, args );
  va_end( args );
  return sts;
}

int log_write_buf( struct log_s *log, int lvl, char *buf, int len, uint64_t *id ) {
  struct log_entry entry;
  struct log_iov iov[1];
  int sts;
  
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


