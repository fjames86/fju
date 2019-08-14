
#include <fju/fdtab.h>
#include <string.h>

#define FDT_HEADER_SIZE 12

int fdtab_open( char *path, struct ftab_opts *opts, struct fdtab_s *fdt ) {
  int sts;
  struct ftab_prop prop;
  
  sts = ftab_open( path, opts, &fdt->ftab );
  if( sts ) return sts;

  sts = ftab_prop( &fdt->ftab, &prop );
  fdt->lbasize = prop.lbasize - FDT_HEADER_SIZE;
  
  return 0;
}

int fdtab_close( struct fdtab_s *fdt ) {
  ftab_close( &fdt->ftab );
  return 0;
}

int fdtab_alloc( struct fdtab_s *fdt, uint32_t size, uint64_t *id ) {
  int sts, nblks;
  uint64_t tid, nextid;

  nblks = size / fdt->lbasize;
  if( size % fdt->lbasize != 0 ) nblks++;
  if( nblks == 0 ) nblks = 1;
  
  sts = ftab_alloc( &fdt->ftab, &tid );
  if( sts ) return sts;
  *id = tid;
  ftab_write( &fdt->ftab, tid, (char *)&size, sizeof(size), 8 );
  
  nblks--;

  size = 0;
  while( nblks ) {
    sts = ftab_alloc( &fdt->ftab, &nextid );
    if( sts ) goto bad;

    ftab_write( &fdt->ftab, tid, (char *)&nextid, sizeof(nextid), 0 );
    ftab_write( &fdt->ftab, tid, (char *)&size, sizeof(size), 8 );
    tid = nextid;
    nblks--;
  }
  nextid = 0;
  ftab_write( &fdt->ftab, tid, (char *)&nextid, sizeof(nextid), 0 );
  ftab_write( &fdt->ftab, tid, (char *)&size, sizeof(size), 8 );
  
  return 0;
  
 bad:  
  /* free all allocated blocks */
  while( tid ) {
    ftab_read( &fdt->ftab, tid, (char *)&nextid, sizeof(nextid), 0 );
    ftab_free( &fdt->ftab, tid );
    tid = nextid;
  }
  
  return -1;  
}

int fdtab_free( struct fdtab_s *fdt, uint64_t id ) {
  int sts;
  uint64_t tid;

  while( id ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, 8, 0 );
    if( sts < 0 ) break;
    ftab_free( &fdt->ftab, id );
    id = tid;
  }

  return 0;
}

int fdtab_realloc( struct fdtab_s *fdt, uint64_t id, uint32_t newsize ) {
  int sts, nblks;
  uint64_t startid, tid;
  uint32_t size;

  startid = id;
  
  sts = ftab_read( &fdt->ftab, id, (char *)&size, sizeof(size), 8 );
  if( sts < 0 ) return sts;
  if( size >= newsize ) return 0;
  
  nblks = newsize / fdt->lbasize;
  if( newsize % fdt->lbasize != 0 ) nblks++;
  if( nblks == 0 ) nblks = 1;

  while( nblks ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    if( sts < 0 ) return sts;
    if( tid == 0 ) break;
    id = tid;
    nblks--;
  }

  if( nblks > 0 ) {
    sts = fdtab_alloc( fdt, nblks * fdt->lbasize, &tid );
    if( sts ) return sts;
    ftab_write( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
  }
  ftab_write( &fdt->ftab, startid, (char *)&newsize, sizeof(newsize), 8 );
  
  return 0;  
}

int fdtab_size( struct fdtab_s *fdt, uint64_t id ) {
  int sts, size;
  
  sts = ftab_read( &fdt->ftab, id, (char *)&size, sizeof(size), 8 );
  if( sts < 0 ) return sts;
  
  return size;
}

int fdtab_truncate( struct fdtab_s *fdt, uint64_t id, uint32_t size ) {
  /* walk to final block, free all blocks coming after */
  int sts, nblks, i;
  uint64_t tid, startid;

  startid = id;
			  
  nblks = size / fdt->lbasize;
  if( size % fdt->lbasize ) nblks++;
  for( i = 0; i < nblks; i++ ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    if( sts < 0 ) return sts;
    id = tid;
  }

  /* clear next pointer and set size */
  tid = 0;
  ftab_write( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
  ftab_write( &fdt->ftab, startid, (char *)&size, sizeof(size), 8 );
  
  /* free data blocks */
  fdtab_free( fdt, id );

  return 0;
}

int fdtab_read( struct fdtab_s *fdt, uint64_t id, char *buf, int n, uint32_t offset ) {
  int sts, nblks, cnt, nbytes;
  uint32_t size, foff;
  uint64_t tid;

  /* read file size */
  sts = ftab_read( &fdt->ftab, id, (char *)&size, sizeof(size), 8 );
  if( sts < 0 ) return sts;
  if( offset >= size ) return 0;
  
  /* skip this many starting blocks */
  nblks = offset / fdt->lbasize;
  offset = offset % fdt->lbasize;
  
  while( nblks ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    if( sts < 0 ) return sts;
    id = tid;
    nblks--;
  }

  /* start reading from here */
  cnt = 0;
  nbytes = n;
  foff = nblks * fdt->lbasize;
  if( nbytes > fdt->lbasize ) nbytes = fdt->lbasize;
  sts = ftab_read( &fdt->ftab, id, buf, nbytes, FDT_HEADER_SIZE + offset );
  if( sts < 0 ) return sts;
  buf += nbytes;
  n -= nbytes;
  cnt += nbytes;
  foff += nbytes;
  ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
  id = tid;
  
  while( id && n && (foff < size)) {
    nbytes = n;
    if( nbytes > fdt->lbasize ) nbytes = fdt->lbasize;
    if( nbytes > (size - foff) ) nbytes = size - foff;
    sts = ftab_read( &fdt->ftab, id, buf, nbytes, FDT_HEADER_SIZE );
    if( sts < 0 ) return sts;
    buf += nbytes;
    n -= nbytes;
    cnt += nbytes;
    foff += nbytes;
    
    ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    id = tid;
  }

  return cnt;
}

int fdtab_write( struct fdtab_s *fdt, uint64_t id, char *buf, int n, uint32_t offset ) {
  int sts, nblks, cnt, nbytes;
  uint32_t size;
  uint64_t tid;

  /* read file size, extend if necessary */
  sts = ftab_read( &fdt->ftab, id, (char *)&size, sizeof(size), 8 );
  if( sts < 0 ) return sts;
  if( size < (offset + n) ) {
    sts = fdtab_realloc( fdt, id, offset + n );
    if( sts < 0 ) return sts;
  }
  
  /* skip this many starting blocks */
  nblks = offset / fdt->lbasize;
  offset = offset % fdt->lbasize;
  
  while( nblks ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    if( sts < 0 ) return sts;
    id = tid;
    nblks--;
  }

  /* start writing from here */
  cnt = 0;
  nbytes = n;
  if( nbytes > fdt->lbasize ) nbytes = fdt->lbasize;
  sts = ftab_write( &fdt->ftab, id, buf, nbytes, FDT_HEADER_SIZE + offset );
  if( sts < 0 ) return sts;
  buf += nbytes;
  n -= nbytes;
  cnt += nbytes;
  ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
  id = tid;
  
  while( id && n ) {
    nbytes = n;
    if( nbytes > fdt->lbasize ) nbytes = fdt->lbasize;
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

