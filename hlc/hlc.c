
#include "hlc.h"
#include <sec.h>
#include <string.h>

int hlc_open( char *path, struct hlc_s *hlc ) {
  int sts;

  sts = log_open( path, NULL, &hlc->log );
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
  uint64_t hash;
  uint64_t prevhash;

  uint32_t spare[12];
};

int hlc_read( struct hlc_s *hlc, uint64_t id, struct hlc_entry *elist, int n, int *nelist ) {
  int sts, i;
  struct log_entry entry;
  struct log_iov iov[2];
  struct hlc_hdr hdr;
  int ne;
  
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

    sts = log_read( &hlc->log, id, &entry, 1, &ne );
    if( sts || ne == 0 ) break;

    elist[i].id = entry.id;
    elist[i].hash = hdr.hash;
    elist[i].prevhash = hdr.prevhash;
    if( elist[i].buf == NULL ) elist[i].len = entry.msglen - sizeof(hdr);
    else elist[i].len = iov[1].len;
    
    i++;
    id = entry.id;
  } while( i < n );

  if( nelist ) *nelist = i;
  
  return 0;
}

static uint64_t hlc_hash( struct hlc_entry *entry ) {
  uint8_t hash[SEC_SHA1_MAX_HASH];
  struct sec_buf iov[2];
  
  iov[0].buf = (char *)&entry->prevhash;
  iov[0].len = sizeof(entry->prevhash);
  iov[1].buf = entry->buf;
  iov[1].len = entry->len;
  sha1( hash, iov, 2 );
  entry->hash = *((uint64_t *)hash);  
  return entry->hash;
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
    
  hdr.prevhash = hdr.hash;
  entry->prevhash = hdr.hash;
  hdr.hash = hlc_hash( entry );
  
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

