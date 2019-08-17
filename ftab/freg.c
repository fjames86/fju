
/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
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
#endif

#include <stdlib.h>
#include <string.h>

#include <fju/freg.h>
#include <fju/fdtab.h>

struct freg_s {
  char name[FREG_MAX_NAME];
  uint64_t datap;
  uint32_t flags;
  uint32_t spare;
};


static struct {
  uint32_t ocount;
  struct fdtab_s fdt;
  uint64_t rootid;
} glob;

int freg_open( void ) {
  int sts;
  struct ftab_prop prop;
  
  if( glob.ocount > 0 ) {
    glob.ocount++;
    return 0;
  }

  sts = fdtab_open( mmf_default_path( "freg.dat", NULL ), NULL, &glob.fdt );
  if( sts ) return sts;

  /* read cookie */
  sts = ftab_prop( &glob.fdt.ftab, &prop );
  memcpy( &glob.rootid, prop.cookie, sizeof(glob.rootid) );
  
  /* if entry table not allocated, do it now */
  if( glob.rootid == 0 ) {
    sts = fdtab_alloc( &glob.fdt, 0, &glob.rootid );
    memcpy( prop.cookie, &glob.rootid, sizeof(glob.rootid) );
    ftab_set_cookie( &glob.fdt.ftab, prop.cookie );
  } else {
    sts = fdtab_read( &glob.fdt, glob.rootid, NULL, 0, 0 );
    if( sts < 0 ) {
      sts = fdtab_alloc( &glob.fdt, 0, &glob.rootid );
      memcpy( prop.cookie, &glob.rootid, sizeof(glob.rootid) );
      ftab_set_cookie( &glob.fdt.ftab, prop.cookie );
    }
  }
  
  glob.ocount = 1;
  return 0;
}

int freg_close( void ) {
  if( !glob.ocount ) return -1;
  glob.ocount--;
  if( glob.ocount ) return 0;
  fdtab_close( &glob.fdt );
  glob.ocount = 0;
  return 0;
}

int freg_list( uint64_t parentid, struct freg_entry *entry, int n ) {
  int sts, nentry, i;
  struct freg_s *e;
  char *buf;

  if( !glob.ocount ) return -1;
  if( !parentid ) parentid = glob.rootid;
  
  nentry = fdtab_size( &glob.fdt, parentid ) / sizeof(*e);
  if( nentry < 0 ) return -1;  
  if( !nentry ) return 0;  
  buf = malloc( sizeof(*e) * nentry );
  sts = fdtab_read( &glob.fdt, parentid, buf, nentry*sizeof(*e), 0 );
  if( sts < 0 ) {
    nentry = -1;
    goto done;
  }
		  
  e = (struct freg_s *)buf;
  for( i = 0; i < nentry; i++ ) {
    if( i < n ) {
      /* copy out */
      strncpy( entry[i].name, e[i].name, FREG_MAX_NAME - 1 );
      entry[i].flags = e[i].flags;
      switch( e[i].flags & FREG_TYPE_MASK ) {
      case FREG_TYPE_UINT32:
	entry[i].len = sizeof(uint32_t);
	break;
      case FREG_TYPE_UINT64:
      case FREG_TYPE_KEY:
	entry[i].len = sizeof(uint64_t);
	break;
      case FREG_TYPE_STRING:
      case FREG_TYPE_OPAQUE:
	entry[i].len = fdtab_size( &glob.fdt, e[i].datap );
	break;
      default:
	entry[i].len = 0;
	break;
      }
    }
  }

 done:
  free( buf );
  return nentry;
}

static int get_entry( uint64_t parentid, char *name, struct freg_s *e, int *idx ) {
  int sts, nentry;
  int offset, i;
  
  nentry = fdtab_size( &glob.fdt, parentid ) / sizeof(*e);
  if( nentry < 0 ) return -1;  
  if( !nentry ) return -1;

  offset = 0;
  for( i = 0; i < nentry; i++ ) {
    sts = fdtab_read( &glob.fdt, parentid, (char *)e, sizeof(*e), offset );
    if( sts < 0 ) return sts;
    if( strcmp( e->name, name ) == 0 ) {
      if( idx ) *idx = i;
      return 0;
    }
    offset += sizeof(*e);
  }

  return -1;
}

static int get_subentry( uint64_t parentid, char *path, uint64_t *id, char *name ) {
  int sts;
  char *p, *q;
  char tmpname[FREG_MAX_NAME];
  uint32_t tmpflags;
  uint64_t tmpid;
  int idx;
  
  p = path;
  if( *p == '/' ) p++;  
  while( *p ) {
    /* get next name */
    q = tmpname;
    idx = 0;
    while( *p && (*p != '/') && (idx < FREG_MAX_NAME - 1) ) {
      *q = *p;
      q++;
      p++;
      idx++;
    }
    if( idx >= (FREG_MAX_NAME - 1) ) return -1;
    *q = '\0';
    if( *p == '/' ) p++;
    if( strcmp( tmpname, "" ) == 0 ) return -1;
    if( *p == '\0' ) break;
    
    sts = freg_get( parentid, tmpname, &tmpflags, (char *)&tmpid, sizeof(tmpid), NULL );
    if( sts ) return sts;
    else if( (tmpflags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) {
      return -1;
    }

    /* continue to next level */
    parentid = tmpid;
  }

  if( name ) strncpy( name, tmpname, FREG_MAX_NAME - 1 );
  if( id ) *id = parentid;

  return 0;
}


int freg_get( uint64_t parentid, char *name, uint32_t *flags, char *buf, int len, int *lenp ) {
  /* read entries until we find one with a matching name */
  int sts, nbytes, type, size;
  struct freg_s e;
  char tmpname[FREG_MAX_NAME];
  
  if( !glob.ocount ) return -1;
  if( !parentid ) parentid = glob.rootid;
  if( strcmp( name, "" ) == 0 ) return -1;

  sts = get_subentry( parentid, name, &parentid, tmpname );
  if( sts ) return sts;
  
  sts = get_entry( parentid, tmpname, &e, NULL );
  if( sts < 0 ) return sts;
  if( flags ) *flags = e.flags;
  
  type = e.flags & FREG_TYPE_MASK;
  switch( type ) {
  case FREG_TYPE_UINT32:
    size = sizeof(uint32_t);
    if( lenp ) *lenp = size;
    
    if( len >= sizeof(uint32_t) ) {
      memcpy( buf, &e.datap, sizeof(uint32_t) );
    }
    break;
  case FREG_TYPE_UINT64:
  case FREG_TYPE_KEY:
    size = sizeof(uint64_t);
    if( lenp ) *lenp = size;

    if( len >= sizeof(uint64_t) ) {
      memcpy( buf, &e.datap, sizeof(uint64_t) );
    }
    break;
  case FREG_TYPE_OPAQUE:
  case FREG_TYPE_STRING:
    nbytes = len;
    size = fdtab_size( &glob.fdt, e.datap );
    if( size < 0 ) return -1;
    if( nbytes > size ) nbytes = size;    
    fdtab_read( &glob.fdt, e.datap, buf, nbytes, 0 );

    if( lenp ) *lenp = size;

    break;
  default:
    return -1;
  }
  
  return 0;
}

int freg_put( uint64_t parentid, char *name, uint32_t flags, char *buf, int len ) {
  /* read entries until we find one with a matching name. if we do then write to its data block otherwise add a new entry */
  int sts, idx, size;
  struct freg_s e;
  uint32_t type;
  uint64_t datap;
  char tmpname[FREG_MAX_NAME];
  
  if( !glob.ocount ) return -1;
  if( !parentid ) parentid = glob.rootid;
  if( strcmp( name, "" ) == 0 ) return -1;

  sts = get_subentry( parentid, name, &parentid, tmpname );
  if( sts ) return sts;
  
  type = flags & FREG_TYPE_MASK;
  datap = 0;
  switch( type ) {
  case FREG_TYPE_UINT32:
    if( len != sizeof(uint32_t) ) return -1;
    datap = *((uint32_t *)buf);
    break;
  case FREG_TYPE_UINT64:
  case FREG_TYPE_KEY:
    if( len != sizeof(uint64_t) ) return -1;
    datap = *((uint64_t *)buf);
    break;
  case FREG_TYPE_STRING:
  case FREG_TYPE_OPAQUE:
    break;
  default:
    return -1;
  }

  sts = get_entry( parentid, tmpname, &e, &idx );
  if( sts == 0 ) {
    /* write existing */

    /* check type - disallow type mismatch */
    if( (e.flags & FREG_TYPE_MASK) != type ) return -1;
    
    e.flags = flags;
    switch( type ) {
    case FREG_TYPE_UINT32:
    case FREG_TYPE_UINT64:
    case FREG_TYPE_KEY:
      e.datap = datap;
      fdtab_write( &glob.fdt, parentid, (char *)&e, sizeof(e), sizeof(e) * idx );
      break;
    case FREG_TYPE_OPAQUE:
    case FREG_TYPE_STRING:
      fdtab_write( &glob.fdt, parentid, (char *)&e, sizeof(e), sizeof(e) * idx );
      fdtab_truncate( &glob.fdt, e.datap, 0 );
      fdtab_write( &glob.fdt, e.datap, buf, len, 0 );
      break;
    }
    
  } else {
    /* add new */
    memset( &e, 0, sizeof(e) );
    strncpy( e.name, tmpname, FREG_MAX_NAME - 1 );
    e.flags = flags;
    switch( type ) {
    case FREG_TYPE_OPAQUE:
    case FREG_TYPE_STRING:
      sts = fdtab_alloc( &glob.fdt, len, &e.datap );
      if( sts ) return sts;
      sts = fdtab_write( &glob.fdt, e.datap, buf, len, 0 );
      break;
    case FREG_TYPE_UINT32:
    case FREG_TYPE_UINT64:
    case FREG_TYPE_KEY:
      e.datap = datap;
      break;
    }

    /* append new entry */
    size = fdtab_size( &glob.fdt, parentid );
    sts = fdtab_write( &glob.fdt, parentid, (char *)&e, sizeof(e), size );
    if( sts < 0 ) return sts;
  }

  return 0;    
}

int freg_rem( uint64_t parentid, char *name ) {  
  int sts, idx, size, type;
  struct freg_s e, tmpe, e2;
  char tmpname[FREG_MAX_NAME];
  
  if( !glob.ocount ) return -1;
  if( !parentid ) parentid = glob.rootid;
  if( strcmp( name, "" ) == 0 ) return -1;

  sts = get_subentry( parentid, name, &parentid, tmpname );
  if( sts ) return sts;
  
  sts = get_entry( parentid, tmpname, &e, &idx );
  if( sts < 0 ) return sts;

  /* if this is a subkey entry, recursively free all its children */
  type = e.flags & FREG_TYPE_MASK;
  if( type == FREG_TYPE_KEY ) {
    int nentry;
    int offset, i;
    
    nentry = fdtab_size( &glob.fdt, e.datap ) / sizeof(e);    
    offset = 0;
    for( i = 0; i < nentry; i++ ) {
      sts = fdtab_read( &glob.fdt, e.datap, (char *)&e2, sizeof(e2), 0 );
      freg_rem( e.datap, e2.name );
      offset += sizeof(e2);
    }
  }
  
  /* free data block */
  switch( type ) {
  case FREG_TYPE_OPAQUE:
  case FREG_TYPE_STRING:
  case FREG_TYPE_KEY:
    fdtab_free( &glob.fdt, e.datap );
    break;
  }
  
  /* copy final entry over top */
  size = fdtab_size( &glob.fdt, parentid );
  if( idx != ((size / sizeof(e)) - 1) ) {
    fdtab_read( &glob.fdt, parentid, (char *)&tmpe, sizeof(tmpe), size - sizeof(tmpe) );
    fdtab_write( &glob.fdt, parentid, (char *)&tmpe, sizeof(tmpe), sizeof(tmpe)*idx );
  }
  fdtab_truncate( &glob.fdt, parentid, size - sizeof(tmpe) );

  return 0;
}

int freg_subkey( uint64_t parentid, char *name, uint32_t flags, uint64_t *id ) {
  int sts;
  uint64_t tmpid;
  char tmpname[FREG_MAX_NAME];
  char *p, *q;
  int idx;
  uint32_t tmpflags;
  
  if( !glob.ocount ) return -1;
  if( !parentid ) parentid = glob.rootid;

  p = name;
  if( *p == '/' ) p++;  
  while( *p ) {
    /* get next name */
    q = tmpname;
    idx = 0;
    while( *p && (*p != '/') && (idx < FREG_MAX_NAME - 1) ) {
      *q = *p;
      q++;
      p++;
      idx++;
    }
    if( idx >= (FREG_MAX_NAME - 1) ) return -1;
    *q = '\0';
    if( *p == '/' ) p++;
    if( strcmp( tmpname, "" ) == 0 ) return -1;
    
    sts = freg_get( parentid, tmpname, &tmpflags, (char *)&tmpid, sizeof(tmpid), NULL );
    if( sts ) {
      if( !(flags & FREG_CREATE) ) return -1;
      
      sts = fdtab_alloc( &glob.fdt, 0, &tmpid );
      if( sts ) return sts;
      sts = freg_put( parentid, tmpname, FREG_TYPE_KEY, (char *)&tmpid, sizeof(tmpid) );
      if( sts ) {
	fdtab_free( &glob.fdt, tmpid );
	return sts;
      }
    } else if( (tmpflags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) {
      return -1;
    }

    /* continue to next level */
    parentid = tmpid;
  }

  if( id ) *id = parentid;
  
  return 0;
}
