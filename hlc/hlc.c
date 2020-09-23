
#include <fju/hlc.h>
#include <fju/sec.h>
#include <string.h>

int hlc_open( char *path, struct hlc_s *hlc ) {
  int sts;
  struct log_opts opts;
  memset( &opts, 0, sizeof(opts) );
  opts.mask |= LOG_OPT_COOKIE;
  strcpy( opts.cookie, "HLC" );
  sts = log_open( path, &opts, &hlc->log );
  if( sts ) return sts;

  return 0;
}

int hlc_close( struct hlc_s *hlc ) {
  log_close( &hlc->log );
  return 0;
}

int hlc_prop( struct hlc_s *hlc, struct hlc_prop *prop ) {
  int sts;
  sts = log_prop( &hlc->log, &prop->lprop );
  if( sts ) return sts;
  
  return 0;
}

struct hlc_hdr {
  hlc_hash_t prevhash;

  uint32_t spare[11];
};

int hlc_read( struct hlc_s *hlc, uint64_t id, struct hlc_entry *elist, int n, int *nelist ) {
  int sts, i;
  struct log_entry entry;
  struct log_iov iov[2];
  struct hlc_hdr hdr;
  
  i = 0;
  do {
    if( i >= n ) break;

    memset( &entry, 0, sizeof(entry) );
    iov[0].buf = (char *)&hdr;
    iov[0].len = sizeof(hdr);
    iov[1].buf = elist[i].buf;
    iov[1].len = elist[i].len;
    entry.iov = iov;
    entry.niov = 2;

    sts = log_read_entry( &hlc->log, id, &entry );
    if( sts ) break;

    elist[i].id = entry.id;
    elist[i].seq = entry.seq;
    memcpy( elist[i].prevhash, hdr.prevhash, sizeof(hlc_hash_t) );
    if( elist[i].buf == NULL ) elist[i].len = entry.msglen - sizeof(hdr);
    else elist[i].len = iov[1].len;
    
    i++;
    id = entry.id;
  } while( i < n );

  if( nelist ) *nelist = i;
  
  return 0;
}

static void hlc_hash( hlc_hash_t hash, struct hlc_entry *entry ) {
  struct sec_buf iov[3];
  
  iov[0].buf = (char *)&entry->prevhash;
  iov[0].len = sizeof(entry->prevhash);
  iov[1].buf = (char *)&entry->seq;
  iov[1].len = sizeof(entry->seq);
  iov[2].buf = entry->buf;
  iov[2].len = entry->len;
  sha1( hash, iov, 3 );
}

int hash_entry_hash( struct hlc_s *hlc, uint64_t id, hlc_hash_t hash ) {
    struct log_entry e;
    struct log_iov iov[2];
    char *buf;
    struct hlc_hdr hdr;
    int sts;
    struct hlc_entry entry;
    
    memset( &e, 0, sizeof(e) );
    e.iov = iov;
    e.niov = 2;
    iov[0].buf = (char *)&hdr;
    iov[0].len = sizeof(hdr);
    iov[1].buf = NULL;
    iov[1].len = 0;
    sts = log_read_entry( &hlc->log, id, &e );
    if( sts ) return sts;

    buf = malloc( e.msglen - sizeof(hdr) );

    iov[1].buf = buf;
    iov[1].len = e.msglen - sizeof(hdr);
    log_read_entry( &hlc->log, id, &e );

    entry.id = e.id;
    entry.seq = e.seq;
    memcpy( entry.prevhash, hdr.prevhash, sizeof(hlc_hash_t) );
    hlc_hash( hash, &entry );
    free( buf );

    return 0;
}

int hlc_write( struct hlc_s *hlc, struct hlc_entry *entry ) {
  int sts, ne;
  struct log_entry e;
  struct log_iov iov[2];
  struct hlc_hdr hdr;

  /* read last entry */
  memset( &e, 0, sizeof(e) );
  e.iov = iov;
  e.niov = 2;
  iov[0].buf = (char *)&hdr;
  iov[0].len = sizeof(hdr);
  iov[1].buf = NULL;
  iov[1].len = 0;
  sts = log_read_end( &hlc->log, 0, &e, 1, &ne );
  if( sts == -1 ) {
      /* first entry - compute genesis hash from log tag */
      struct sec_buf iov[2];
      struct log_prop prop;
      log_prop( &hlc->log, &prop );
      iov[0].buf = (char *)&prop.tag;
      iov[0].len = sizeof(prop.tag);
      iov[1].buf = (char *)&prop.tag;
      iov[1].len = sizeof(prop.tag);      
      sha1( entry->prevhash, iov, 2 );
  } else {  
      /* compute previous hash, compare with claimed hash */
      hlc_hash_t prevhash;
      struct hlc_entry preventry;
      char *buf = malloc( e.msglen - sizeof(hdr) );
      iov[1].buf = buf;
      iov[1].len = e.msglen - sizeof(hdr);
      log_read( &hlc->log, 0, &e, 1, &ne );
      preventry.id = e.id;
      preventry.seq = e.seq;
      preventry.buf = iov[1].buf;
      preventry.len = iov[1].len;
      hlc_hash( prevhash, &preventry );
      free( buf );
//      if( memcmp( prevhash, entry->prevhash, sizeof(hlc_hash_t) ) != 0 ) return -1;
      memcpy( entry->prevhash, prevhash, sizeof(hlc_hash_t) );
  }
  hlc_hash( hdr.prevhash, entry );

  /* write entry */
  memset( &e, 0, sizeof(e) );
  e.iov = iov;
  e.niov = 2;
  iov[0].buf = (char *)&hdr;
  iov[0].len = sizeof(hdr);
  iov[1].buf = entry->buf;
  iov[1].len = entry->len;
  e.flags = LOG_BINARY;
  sts = log_write( &hlc->log, &e );
  entry->id = e.id;
  return 0;
}

