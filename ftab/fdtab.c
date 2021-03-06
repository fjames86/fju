
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

  /* write size */
  sts = ftab_write( &fdt->ftab, tid, (char *)&size, sizeof(size), 8 );
  
  nblks--;

  /* write rest of the trailing blocks */
  while( nblks ) {
    sts = ftab_alloc( &fdt->ftab, &nextid );
    if( sts ) goto bad;

    ftab_write( &fdt->ftab, tid, (char *)&nextid, sizeof(nextid), 0 );
    ftab_write( &fdt->ftab, tid, (char *)&size, sizeof(size), 8 );
    size -= fdt->lbasize;
    
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

  /* check max file size and extend if necessary */
  size = fdtab_maxsize( fdt, id );
  if( size < 0 ) return -1;

  if( newsize > size ) {
    nblks = newsize / fdt->lbasize;
    if( (newsize % fdt->lbasize) != 0 ) nblks++;
    
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
  }

  /* write new file size */
  if( newsize > fdtab_size( fdt, id ) ) {
    ftab_write( &fdt->ftab, startid, (char *)&newsize, sizeof(newsize), 8 );
  }
  
  return 0;  
}

int fdtab_size( struct fdtab_s *fdt, uint64_t id ) {
  int sts, size;
  
  sts = ftab_read( &fdt->ftab, id, (char *)&size, sizeof(size), 8 );
  if( sts < 0 ) return sts;
  
  return size;
}

int fdtab_maxsize( struct fdtab_s *fdt, uint64_t id ) {
  int sts, size, maxsize;
  uint64_t nextid;
  
  maxsize = 0;
  size = fdtab_size( fdt, id );
  if( size < 0 ) return -1;

  do {
    maxsize += fdt->lbasize;
    
    sts = ftab_read( &fdt->ftab, id, (char *)&nextid, sizeof(nextid), 0 );
    if( sts < 0 ) break;
    id = nextid;
  } while( id );
  
  return maxsize;
}

int fdtab_truncate( struct fdtab_s *fdt, uint64_t id, uint32_t size ) {
  int sts, nblks, i;
  uint64_t tid, startid, previd;

  startid = id;
  previd = 0;
  
  nblks = size / fdt->lbasize;
  if( size % fdt->lbasize ) nblks++;
  if( !nblks ) nblks = 1;
  for( i = 0; i < nblks; i++ ) {
    sts = ftab_read( &fdt->ftab, id, (char *)&tid, sizeof(tid), 0 );
    if( sts < 0 ) return sts;
    previd = id;
    id = tid;
  }

  /* clear next pointer */
  if( previd ) {
    tid = 0;
    ftab_write( &fdt->ftab, previd, (char *)&tid, sizeof(tid), 0 );  
    fdtab_free( fdt, id );
  }    

  /* set size */
  ftab_write( &fdt->ftab, startid, (char *)&size, sizeof(size), 8 );
  
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
  foff = (nblks * fdt->lbasize) + offset;
  if( nbytes > (fdt->lbasize - offset) ) nbytes = fdt->lbasize - offset;
  if( nbytes > (size - offset) ) nbytes = size - offset;
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

  /* get max file size, extend if necessary */
  if( (offset + n) > fdtab_maxsize( fdt, id ) ) {
    sts = fdtab_realloc( fdt, id, offset + n );
    if( sts < 0 ) return sts;
  }
  /* write new size */
  size = fdtab_size( fdt, id );
  if( (offset + n) > size ) {
    size = offset + n;
    ftab_write( &fdt->ftab, id, (char *)&size, sizeof(size), 8 );
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
  if( nbytes > (fdt->lbasize - offset) ) nbytes = fdt->lbasize - offset;
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

