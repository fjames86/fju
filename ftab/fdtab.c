
#include <fju/fdtab.h>
#include <string.h>

  
int fdtab_open( char *path, struct ftab_opts *opts, struct fdtab_s *fdt ) {
  int sts;
  struct ftab_prop prop;
  
  sts = ftab_open( path, opts, &fdt->ftab );
  if( sts ) return sts;

  sts = ftab_prop( &fdt->ftab, &prop );
  fdt->lbasize = prop.lbasize;
  
  return 0;
}

int fdtab_close( struct fdtab_s *fdt ) {
  ftab_close( &fdt->ftab );
  return 0;
}

#define FDT_HEADER_SIZE 8

int fdtab_alloc( struct fdtab_s *fdt, uint32_t size, uint64_t *id ) {
  int sts, nblks;
  uint64_t tid, nextid;

  nblks = size / (fdt->lbasize - FDT_HEADER_SIZE);
  if( size % fdt->lbasize != 0 ) nblks++;
  
  sts = ftab_alloc( &fdt->ftab, &tid );
  if( sts ) return sts;
  *id = tid;
  nblks--;
  
  while( nblks ) {
    sts = ftab_alloc( &fdt->ftab, &nextid );
    if( sts ) goto bad;

    ftab_write( &fdt->ftab, tid, (char *)&nextid, sizeof(nextid), 0 );
    tid = nextid;
    nblks--;
  }
  nextid = 0;
  ftab_write( &fdt->ftab, tid, (char *)&nextid, sizeof(nextid), 0 );
  
  return 0;
 bad:
  /* free all allocated blocks? */
  return -1;  
}

int fdtab_free( struct fdtab_s *fdt, uint64_t id ) {
  int sts;
  uint64_t tid;

  while( id ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, 8, 0 );
    if( sts ) break;
    ftab_free( &fdt->ftab, id );
    id = tid;
  }

  return 0;
}

int fdtab_realloc( struct fdtab_s *fdt, uint64_t id, uint32_t newsize ) {
  int sts;
  uint64_t tid;
  int nblks;

  nblks = newsize / (fdt->lbasize - FDT_HEADER_SIZE);
  if( nblks == 0 ) nblks = 1;

  while( nblks ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    if( sts ) return sts;
    if( tid == 0 ) break;
    id = tid;
    nblks--;
  }

  if( nblks > 0 ) {
    sts = fdtab_alloc( fdt, nblks * (fdt->lbasize - FDT_HEADER_SIZE), &tid );
    if( sts ) return sts;
    ftab_write( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
  }
  
  return 0;  
}

int fdtab_size( struct fdtab_s *fdt, uint64_t id ) {
  int sts, cnt;
  uint64_t tid;
  
  /* start reading from here */
  cnt = fdt->lbasize - FDT_HEADER_SIZE;
  sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
  if( sts < 0 ) return sts;
  id = tid;
  while( id ) {
    cnt += fdt->lbasize - FDT_HEADER_SIZE;
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    if( sts < 0 ) return sts;
    id = tid;
  }

  return cnt;
}

int fdtab_read( struct fdtab_s *fdt, uint64_t id, char *buf, int n, uint32_t offset ) {
  int sts, nblks, cnt, nbytes;
  uint64_t tid;
  
  /* skip this many starting blocks */
  nblks = offset / (fdt->lbasize - FDT_HEADER_SIZE);
  offset = offset % (fdt->lbasize - FDT_HEADER_SIZE);
  
  while( nblks ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    if( sts < 0 ) return sts;
    id = tid;
    nblks--;
  }

  /* start reading from here */
  cnt = 0;
  nbytes = n;
  if( nbytes > (fdt->lbasize - FDT_HEADER_SIZE) ) nbytes = fdt->lbasize - FDT_HEADER_SIZE;
  sts = ftab_read( &fdt->ftab, id, buf, nbytes, FDT_HEADER_SIZE + offset );
  if( sts < 0 ) return sts;
  buf += nbytes;
  n -= nbytes;
  cnt += nbytes;
  ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
  id = tid;
  
  while( id && n ) {
    nbytes = n;
    if( nbytes > (fdt->lbasize - FDT_HEADER_SIZE) ) nbytes = fdt->lbasize - FDT_HEADER_SIZE;
    sts = ftab_read( &fdt->ftab, id, buf, nbytes, FDT_HEADER_SIZE );
    if( sts < 0 ) return sts;
    buf += nbytes;
    n -= nbytes;
    cnt += nbytes;

    ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    id = tid;
  }

  return cnt;
}

int fdtab_write( struct fdtab_s *fdt, uint64_t id, char *buf, int n, uint32_t offset ) {
  int sts, nblks, cnt, nbytes;
  uint64_t tid;
  
  /* skip this many starting blocks */
  nblks = offset / (fdt->lbasize - FDT_HEADER_SIZE);
  offset = offset % (fdt->lbasize - FDT_HEADER_SIZE);
  
  while( nblks ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    if( sts < 0 ) return sts;
    id = tid;
    nblks--;
  }

  /* start writing from here */
  cnt = 0;
  nbytes = n;
  if( nbytes > (fdt->lbasize - FDT_HEADER_SIZE) ) nbytes = fdt->lbasize - FDT_HEADER_SIZE;
  sts = ftab_write( &fdt->ftab, id, buf, nbytes, FDT_HEADER_SIZE + offset );
  if( sts < 0 ) return sts;
  buf += nbytes;
  n -= nbytes;
  cnt += nbytes;
  ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
  id = tid;
  
  while( id && n ) {
    nbytes = n;
    if( nbytes > (fdt->lbasize - FDT_HEADER_SIZE) ) nbytes = fdt->lbasize - FDT_HEADER_SIZE;
    sts = ftab_write( &fdt->ftab, id, buf, nbytes, FDT_HEADER_SIZE );
    if( sts < 0 ) return sts;
    buf += nbytes;
    n -= nbytes;
    cnt += nbytes;

    ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    id = tid;
  }

  return cnt;
}

