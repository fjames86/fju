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
#include <WinSock2.h>
#include <Windows.h>
#endif

#include "log.h"

#include <string.h>

#define LOG_LBASIZE 64 

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

  uint32_t spare[56]; /* future expansion */
};

struct _entry {
  uint32_t magic;      /* msg start identifier */
  uint32_t count;      /* number of blocks, including header */
  uint64_t id;         /* msg identifier */
  uint64_t timestamp;  /* when msg written */
  uint32_t pid;        /* who wrote msg */
  uint32_t flags;      /* msg flags */
  uint32_t msglen;     /* length of msg data */

  uint8_t spare[28];   /* future expansion */
};

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
    hdr->lbacount =(opts && (opts->mask & LOG_OPT_LBACOUNT)) ? opts->lbacount : LOG_LBACOUNT;
    hdr->seq = 1;
    hdr->start = 0;
    hdr->count = 0;
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
  
  sts = mmf_lock( &log->mmf );
  if( sts ) return sts;

  hdr->start = 0;
  hdr->count = 0;
  hdr->seq = 1;
  
  mmf_unlock( &log->mmf );
  return 0;
}

int log_prop( struct log_s *log, struct log_prop *prop ) {
  struct _header *hdr;
  int sts;

  hdr = (struct _header *)log->mmf.file;
  if( !hdr ) return -1;

  sts = mmf_lock( &log->mmf );
  if( sts ) return sts;

  prop->version = hdr->version;
  prop->seq = hdr->seq;
  prop->start = hdr->start;
  prop->count = hdr->count;
  prop->lbacount = hdr->lbacount;

  mmf_unlock( &log->mmf );

  return 0;
}

int log_read( struct log_s *log, uint64_t id, struct log_entry *elist, int n, int *nelist ) {
  int sts;
  struct _header *hdr;
  char *p, *q;
  struct _entry *e;
  int i, j, k, len;
  int idx;

  if( nelist ) *nelist = 0;
  i = 0;
  hdr = (struct _header *)log->mmf.file;

  sts = mmf_lock( &log->mmf );
  if( sts ) return sts;

  if( !id ) {
    /* id=0 implies start from beginning */
    idx = hdr->start;
  } else {
    /* id!=0 implies start from next item */
    idx = (uint32_t)(id & 0xffffffff);
    p = (char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx);
    e = (struct _entry *)p;
    if( e->magic != LOG_MAGIC ) goto done;
    idx = (idx + e->count) % hdr->lbacount;
  }
  if( idx == ((hdr->start + hdr->count) % hdr->lbacount) ) goto done;

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

    if( elist[i].msglen == 0 ) {
      /* zero length msglen, set to required length */
      elist[i].msglen = e->msglen;
    } else {
      /* copy data */
      len = e->msglen;
      if( len > elist[i].msglen ) len = elist[i].msglen;

      idx = (idx + 1) % hdr->lbacount;
      q = elist[i].msg;
      for( j = 0; j < e->count; j++ ) {
	p = (char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx);
	if( len > 0 ) {
	  k = len;
	  if( k > LOG_LBASIZE ) k = LOG_LBASIZE;
	  memcpy( q, p, k );
	  len -= k;
	  q += k;
	}
      }
      
      /* if non-binary append a null terminator */
      if( !(elist[i].flags & LOG_BINARY) ) {
	if( e->msglen < elist[i].msglen ) elist[i].msg[e->msglen] = '\0';
      }

      if( e->msglen < elist[i].msglen ) elist[i].msglen = e->msglen;


    }
    i++;
   
    idx = (idx + 1) % hdr->lbacount;
  } while( (i < n) && (idx != ((hdr->start + hdr->count) % hdr->lbacount)) );

 done:
  if( nelist ) *nelist = i;
  mmf_unlock( &log->mmf );

  return 0;
}

int log_write( struct log_s *log, struct log_entry *entry ) {
  int sts;
  char *p, *q;
  struct _entry *e;
  struct _header *hdr;
  int j, k, len, cnt;
  int idx;

  entry->timestamp = time( NULL );
  entry->pid = log->pid;
  hdr = (struct _header *)log->mmf.file;
  cnt = 1 + (entry->msglen / LOG_LBASIZE);
  if( entry->msglen % LOG_LBASIZE ) cnt++;
    
  sts = mmf_lock( &log->mmf );
  if( sts ) return sts;

  /* check enough space */
  if( cnt > hdr->lbacount ) goto done;

  /* get next location */
  idx = (hdr->start + hdr->count) % hdr->lbacount;

  /* check we won't overwrite the current starting block */
  if( (hdr->lbacount - hdr->count) < cnt ){
    do {
      /* oh dear - we will overwrite, keep advancing the starting point until there is space */
      e = (struct _entry *)((char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * hdr->start));
      hdr->start = (hdr->start + e->count) %  hdr->lbacount;
      hdr->count -= e->count;
    } while( (hdr->lbacount - hdr->count) < cnt );
  }
  hdr->count += cnt;
  hdr->seq++;

  /* now write data at end */
  e = (struct _entry *)((char *)log->mmf.file + sizeof(struct _header) + (LOG_LBASIZE * idx));
  e->magic = LOG_MAGIC;
  e->count = cnt;
  e->id = ((hdr->seq & 0xffffffff) << 32) | idx;
  e->timestamp = time( NULL );
  e->pid = getpid();
  e->flags = entry->flags;
  e->msglen = entry->msglen;
  len = e->msglen;
  q = entry->msg;
  idx = (idx + 1) % hdr->lbacount;
  for( j = 0; j < (cnt - 1); j++ ) {
    p = ((char *)log->mmf.file) + sizeof(struct _header) + (LOG_LBASIZE * idx);    
    if( len > 0 ) {
      k = len;
      if( k > LOG_LBASIZE ) k = LOG_LBASIZE;
      memcpy( p, q, k );
      q += k;
      len -= k;
    }
    idx = (idx + 1) % hdr->lbacount;
  }

  hdr->seq++;

 done:
  mmf_unlock( &log->mmf );
  return 0;
}

int log_writev( struct log_s *log, int lvl, char *fmt, va_list args ) {
  struct log_entry entry;
  char buf[1024];

  vsprintf( buf, fmt, args );  
  memset( &entry, 0, sizeof(entry) );
  entry.msg = buf;
  entry.msglen = strlen(buf);
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
