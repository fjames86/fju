
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

struct entry_s {
  char name[FREG_MAX_NAME];
  uint32_t flags;
  uint32_t spare;
};


static struct {
  uint32_t ocount;
  struct freg_s freg;
} glob;

#define FREG_LBASIZE 128
static int get_subentry( struct freg_s *freg, uint64_t parentid, char *path, uint64_t *id, char *name );

int freg_open( char *path, struct freg_s *freg ) {
  int sts;
  struct ftab_prop prop;
  struct ftab_opts opts;
  int useglob;

  useglob = freg ? 0 : 1;
  if( !freg && (glob.ocount > 0) ) {
    glob.ocount++;
    return 0;
  }
  if( !useglob && !path ) return -1;
  
  if( !freg ) freg = &glob.freg;
  
  memset( &opts, 0, sizeof(opts) );
  opts.mask = FTAB_OPT_LBASIZE;
  opts.lbasize = FREG_LBASIZE;
  sts = fdtab_open( useglob ? mmf_default_path( "freg.dat", NULL ) : path, &opts, &freg->fdt );
  if( sts ) return sts;
  
  /* read cookie */
  sts = ftab_prop( &freg->fdt.ftab, &prop );
  memcpy( &freg->rootid, prop.cookie, sizeof(freg->rootid) );
  
  /* if entry table not allocated, do it now */
  if( freg->rootid == 0 ) {
      struct entry_s e;
      sts = fdtab_alloc( &freg->fdt, sizeof(e), &freg->rootid );
      memset( &e, 0, sizeof(e) );
      e.flags = FREG_TYPE_KEY;
      sts = fdtab_write( &freg->fdt, freg->rootid, (char *)&e, sizeof(e), 0 );
      
      memcpy( prop.cookie, &freg->rootid, sizeof(freg->rootid) );
      ftab_set_cookie( &freg->fdt.ftab, prop.cookie );
  } else {
    sts = fdtab_read( &freg->fdt, freg->rootid, NULL, 0, 0 );
    if( sts < 0 ) {
      sts = fdtab_alloc( &freg->fdt, 0, &freg->rootid );
      memcpy( prop.cookie, &freg->rootid, sizeof(freg->rootid) );
      ftab_set_cookie( &freg->fdt.ftab, prop.cookie );
    }
  }

  if( useglob ) glob.ocount = 1;
  return 0;
}

int freg_close( struct freg_s *freg ) {
  if( !freg ) {
    if( !glob.ocount ) return -1;
    glob.ocount--;
    if( glob.ocount ) return 0;
    fdtab_close( &glob.freg.fdt );
    glob.ocount = 0;
  } else {
    fdtab_close( &freg->fdt );
  }
  return 0;
}

int freg_entry_by_id( struct freg_s *freg, uint64_t id, struct freg_entry *entry ) {
    int sts;
    struct entry_s e;

    if( !freg ) {
      if( !glob.ocount ) return -1;
      freg = &glob.freg;
    }
    
    entry->id = id;
    sts = fdtab_read( &freg->fdt, id, (char *)&e, sizeof(e), 0 );
    if( sts < 0 ) return sts;
    strncpy( entry->name, e.name, FREG_MAX_NAME - 1 );
    entry->len = fdtab_size( &freg->fdt, id ) - sizeof(e);
    entry->flags = e.flags;

    return 0;
}

int freg_list( struct freg_s *freg, uint64_t parentid, struct freg_entry *entry, int n ) {
  int sts, nentry, i, j, idx, nn, ncnt;
    uint64_t buf[32];
    struct freg_entry etry;
    struct entry_s e;

    if( !freg ) {
      if( !glob.ocount ) return -1;
      freg = &glob.freg;
    }
    if( !parentid ) parentid = freg->rootid;

    sts = freg_entry_by_id( freg, parentid, &etry );
    if( sts ) return sts;
    if( (etry.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) return -1;
    
    nentry = etry.len / sizeof(uint64_t);
    if( nentry < 0 ) return -1;  
    if( !nentry ) return 0;
    
    idx = 0;
    ncnt = 0;
    for( i = 0; i < nentry; i += 32 ) {
      sts = fdtab_read( &freg->fdt, parentid, (char *)buf, sizeof(buf), sizeof(e) + (sizeof(uint64_t) * i) );
      if( sts < 0 ) break;

      nn = sts / sizeof(uint64_t);
      for( j = 0; j < nn; j++ ) {
	sts = freg_entry_by_id( freg, buf[j], &etry );
	if( sts ) {
	  /* invalid entry? */
	} else {
	  if( idx < n ) {
	    entry[idx] = etry;
	    idx++;
	  }
	  ncnt++;
	}
      }

      if( nn < 32 ) break;
    }

    if( ncnt != nentry ) {
      /* corruption? */
    }
    
    return ncnt;
}

int freg_next( struct freg_s *freg, uint64_t parentid, uint64_t id, struct freg_entry *entry ) {
  int sts, nentry, i, j, idx, nn;
    uint64_t buf[32];
    struct freg_entry etry;
    struct entry_s e;
    int getnext;
    
    if( !freg ) {
      if( !glob.ocount ) return -1;
      freg = &glob.freg;
    }
    if( !parentid ) parentid = freg->rootid;

    sts = freg_entry_by_id( freg, parentid, &etry );
    if( sts ) return sts;
    if( (etry.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) return -1;
    
    nentry = etry.len / sizeof(uint64_t);
    if( nentry < 0 ) return -1;  
    if( !nentry ) return 0;
    
    idx = 0;
    getnext = id ? 0 : 1;    
    for( i = 0; i < nentry; i += 32 ) {
      sts = fdtab_read( &freg->fdt, parentid, (char *)buf, sizeof(buf), sizeof(e) + (sizeof(uint64_t) * i) );
      if( sts < 0 ) break;

      nn = sts / sizeof(uint64_t);
      for( j = 0; j < nn; j++ ) {
	sts = freg_entry_by_id( freg, buf[j], &etry );
	if( sts ) continue;
	
	if( getnext ) {
	  *entry = etry;
	  return 0;
	}
	
	if( buf[j] == id ) {
	  getnext = 1;
	}
      }

      if( nn < 32 ) break;
    }

    return -1;
}

int freg_entry_by_name( struct freg_s *freg, uint64_t parentid, char *name, struct freg_entry *entry, uint64_t *parentidp ) {
  int sts, nentry, i, j, n, ncnt;
    struct freg_entry etry;
    char tmpname[FREG_MAX_NAME];
    uint64_t buf[32];
    struct entry_s e;

    if( !freg ) {
      if( !glob.ocount ) return -1;
      freg = &glob.freg;
    }
    if( !parentid ) parentid = freg->rootid;

    sts = get_subentry( freg, parentid, name, &parentid, tmpname );
    if( sts ) return sts;

    sts = freg_entry_by_id( freg, parentid, &etry );
    if( sts ) return sts;
    if( (etry.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) return -1;
    
    nentry = etry.len / sizeof(uint64_t);
    if( nentry < 0 ) return -1;  
    if( !nentry ) return -1;

    ncnt = 0;
    for( i = 0; i < nentry; i += 32 ) {
      sts = fdtab_read( &freg->fdt, parentid, (char *)buf, sizeof(buf), sizeof(e) + (sizeof(uint64_t) * i) );
      if( sts < 0 ) break;      
      n = sts / sizeof(uint64_t);
      
      for( j = 0; j < n; j++ ) {
	sts = freg_entry_by_id( freg, buf[j], &etry );
	if( sts ) {
	  /* invalid item? */
	} else if( strcmp( etry.name, tmpname ) == 0 ) {
	  if( entry ) *entry = etry;
	  if( parentidp ) *parentidp = parentid;
	  return 0;
	} else {
	  ncnt++;
	}
      }
	
      if( n < 32 ) break;
    }

    if( ncnt != nentry ) {
      /* invalid population count? */
    }
    
    return -1;
}

int freg_get( struct freg_s *freg, uint64_t id, uint32_t *flags, char *buf, int len, int *lenp ) {
    int sts, nbytes, size;
    struct entry_s e;

    if( !freg ) {
      if( !glob.ocount ) return -1;
      freg = &glob.freg;
    }

    sts = fdtab_read( &freg->fdt, id, (char *)&e, sizeof(e), 0 );
    if( sts < sizeof(e) ) return sts;
    
    if( flags ) *flags = e.flags;

    nbytes = len;
    size = fdtab_size( &freg->fdt, id ) - sizeof(e);
    if( size < nbytes ) nbytes = size;
    sts = fdtab_read( &freg->fdt, id, buf, nbytes, sizeof(e) );
    if( sts < 0 ) return sts;
    if( lenp ) *lenp = size;

    return 0;
}

int freg_put( struct freg_s *freg, uint64_t parentid, char *name, uint32_t flags, char *buf, int len, uint64_t *id ) {
    int sts, nentry, i, j, n;
    struct freg_entry entry;
    struct entry_s e;
    char tmpname[FREG_MAX_NAME];
    uint64_t tmpbuf[32];
    uint64_t tid;

    if( !freg ) {
      if( !glob.ocount ) return -1;
      freg = &glob.freg;
    }
    if( !parentid ) parentid = freg->rootid;

    switch( flags & FREG_TYPE_MASK ) {
    case FREG_TYPE_UINT32:
	if( len != sizeof(uint32_t) ) return -1;
	break;
    case FREG_TYPE_UINT64:
	if( len != sizeof(uint64_t) ) return -1;	
	break;
    case FREG_TYPE_KEY:
	/* forbid calling with type key, keys are added using freg_subkey */
	return -1;
    }

    sts = get_subentry( freg, parentid, name, &parentid, tmpname );
    if( sts ) return sts;
    
    sts = freg_entry_by_id( freg, parentid, &entry );
    if( sts ) return sts;
    if( (entry.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) return -1;
    
    nentry = entry.len / sizeof(uint64_t);
    for( i = 0; i < nentry; i += 32 ) {
	sts = fdtab_read( &freg->fdt, parentid, (char *)tmpbuf, sizeof(tmpbuf), sizeof(e) + (sizeof(uint64_t) * i) );
	if( sts < 0 ) break;
	n = sts / sizeof(uint64_t);

	for( j = 0; j < n; j++ ) {
	  sts = freg_entry_by_id( freg, tmpbuf[j], &entry );
	  if( sts ) {
	    /* invalid item? */
	  } else if( strcmp( entry.name, tmpname ) == 0 ) {
	    /* check type match */
	    if( (flags & FREG_TYPE_MASK) != (entry.flags & FREG_TYPE_MASK) ) return -1;

	    /* update existing data*/
	    if( flags != entry.flags ) {
	      memset( &e, 0, sizeof(e) );
	      strcpy( e.name, entry.name );
	      e.flags = flags;
	      sts = fdtab_write( &freg->fdt, entry.id, (char *)&e, sizeof(e), 0 );
	    }
	    if( len < entry.len ) sts = fdtab_truncate( &freg->fdt, entry.id, sizeof(e) + len );
	    sts = fdtab_write( &freg->fdt, entry.id, buf, len, sizeof(e) );
	    
	    if( id ) *id = entry.id;
	    return 0;
	  }
	}
	
	if( n < 32 ) break;
    }
    
    /* add new entry */
    memset( &e, 0, sizeof(e) );
    strncpy( e.name, tmpname, FREG_MAX_NAME - 1 );
    e.flags = flags;
    sts = fdtab_alloc( &freg->fdt, sizeof(e) + len, &tid );
    if( sts ) return sts;
    sts = fdtab_write( &freg->fdt, tid, (char *)&e, sizeof(e), 0 );
    if( sts < 0 ) return sts;
    sts = fdtab_write( &freg->fdt, tid, buf, len, sizeof(e) );
    if( sts < 0 ) return sts;
    
    /* append id to parent */
    sts = fdtab_write( &freg->fdt, parentid, (char *)&tid, sizeof(uint64_t), sizeof(e) + (nentry * sizeof(uint64_t)) );
    if( sts < 0 ) return sts;
    
    if( id ) *id = tid;
    
    return 0;
}

int freg_set( struct freg_s *freg, uint64_t id, char *name, uint32_t *flags, char *buf, int len ) {
    int sts;
    struct entry_s e;

    if( !freg ) {
      if( !glob.ocount ) return -1;
      freg = &glob.freg;
    }

    if( flags && len ) {
	switch( (*flags) & FREG_TYPE_MASK ) {
	case FREG_TYPE_UINT32:
	    if( len != sizeof(uint32_t) ) return -1;
	    break;
	case FREG_TYPE_UINT64:
	    if( len != sizeof(uint64_t) ) return -1;
	    break;
	case FREG_TYPE_KEY:
	    return -1;
	}
    }

    sts = fdtab_read( &freg->fdt, id, (char *)&e, sizeof(e), 0 );
    if( sts < 0 ) return sts;

    if( flags ) {
	e.flags = ((*flags) & ~FREG_TYPE_MASK) | (e.flags & FREG_TYPE_MASK);
    }
    if( name ) strncpy( e.name, name, FREG_MAX_NAME - 1 );
    if( buf ) {
	if( len < (fdtab_size( &freg->fdt, id ) - sizeof(e)) ) sts = fdtab_truncate( &freg->fdt, id, sizeof(e) + len );
	sts = fdtab_write( &freg->fdt, id, buf, len, sizeof(e) );
    }
    if( flags || name || buf ) {
      sts = fdtab_write( &freg->fdt, id, (char *)&e, sizeof(e), 0 );
    }
    
    return 0;
}

int freg_rem( struct freg_s *freg, uint64_t parentid, uint64_t id ) {
    int sts, nentry, i, j, n;
    struct freg_entry parent, entry;
    uint64_t buf[32];
    struct entry_s e;
    uint64_t tmpid;

    if( !freg ) {
      if( !glob.ocount ) return -1;
      freg = &glob.freg;
    }
    if( !parentid ) parentid = freg->rootid;

    sts = freg_entry_by_id( freg, parentid, &parent );
    if( sts ) return sts;
    if( (parent.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) return -1;
    nentry = parent.len / sizeof(uint64_t);
    if( !nentry ) return -1;
    
    sts = freg_entry_by_id( freg, id, &entry );
    if( sts ) return sts;

    if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
      /* recursively delete all child items first */
      while( entry.len > 0 ) {
	if( entry.len < sizeof(uint64_t) ) break;
	
	sts = fdtab_read( &freg->fdt, id, (char *)&tmpid, sizeof(tmpid), sizeof(e) );
	if( sts < sizeof(tmpid) ) break;
	freg_rem( freg, id, tmpid );
	
	sts = freg_entry_by_id( freg, id, &entry );
	if( sts < 0 ) break;
      }
    }

    /* free data block */
    fdtab_free( &freg->fdt, id );

    /* delete from parent list */
    i = 0;
    do {
	sts = fdtab_read( &freg->fdt, parentid, (char *)buf, sizeof(buf), sizeof(e) + (sizeof(uint64_t) * i) );
	if( sts < 0 ) break;

	n = sts / sizeof(uint64_t);
	for( j = 0; j < n; j++ ) {
	    if( buf[j] == id ) {
		/* copy final item over top of this item */
		if( i != (nentry - 1) ) {
		  sts = fdtab_read( &freg->fdt, parentid, (char *)&id, sizeof(id), sizeof(e) + (sizeof(id) * (nentry - 1)) );
		  sts = fdtab_write( &freg->fdt, parentid, (char *)&id, sizeof(id), sizeof(e) + (sizeof(id) * i) );
		}
		sts = fdtab_truncate( &freg->fdt, parentid, sizeof(e) + (sizeof(uint64_t) * (nentry - 1)) );
		return 0;
	    }

	    i++;
	}
    } while( i < nentry );

    return -1;
}

static int get_subentry( struct freg_s *freg, uint64_t parentid, char *path, uint64_t *id, char *name ) {
  int sts;
  char *p, *q;
  char tmpname[FREG_MAX_NAME];
  int idx;
  struct freg_entry e;
  
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
    
    sts = freg_entry_by_name( freg, parentid, tmpname, &e, NULL );
    if( sts ) return sts;
    else if( (e.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) {
      return -1;
    }

    /* continue to next level */
    parentid = e.id;
  }

  if( name ) strncpy( name, tmpname, FREG_MAX_NAME - 1 );
  if( id ) *id = parentid;

  return 0;
}

int freg_subkey( struct freg_s *freg, uint64_t parentid, char *name, uint32_t flags, uint64_t *id ) {
  int sts;
  uint64_t tmpid;
  char tmpname[FREG_MAX_NAME];
  char *p, *q;
  int idx;
  struct freg_entry entry;
  struct entry_s e;

  if( !freg ) {
    if( !glob.ocount ) return -1;
    freg = &glob.freg;
  }
  if( !parentid ) parentid = freg->rootid;

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
    if( (flags & FREG_VALUEPATH) && (*p == '\0') ) break;
    
    sts = freg_entry_by_name( freg, parentid, tmpname, &entry, NULL );
    if( sts ) {
	/* subkey didn't exist so create it */
	if( !(flags & FREG_CREATE) ) return -1;

	/* create new key entry */
	sts = fdtab_alloc( &freg->fdt, 0, &tmpid );
	if( sts ) return sts;
	memset( &e, 0, sizeof(e) );
	strncpy( e.name, tmpname, FREG_MAX_NAME - 1 );
	e.flags = FREG_TYPE_KEY;
	sts = fdtab_write( &freg->fdt, tmpid, (char *)&e, sizeof(e), 0 );

	/* append id to parent */
	sts = freg_entry_by_id( freg, parentid, &entry );
	sts = fdtab_write( &freg->fdt, parentid, (char *)&tmpid, sizeof(tmpid), sizeof(e) + entry.len );

	parentid = tmpid;
    } else if( (entry.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) {
	return -1;
    } else {    
	/* continue to next level */
	parentid = entry.id;
    }
  }

  if( id ) *id = parentid;
  
  return 0;
}

int freg_get_by_name( struct freg_s *freg, uint64_t parentid, char *name, uint32_t flags, char *buf, int len, int *lenp ) {
  int sts;
  struct freg_entry entry;

  sts = freg_entry_by_name( freg, parentid, name, &entry, NULL );
  if( sts ) return sts;
  if( (entry.flags & FREG_TYPE_MASK) != (flags & FREG_TYPE_MASK) ) return -1;
  return freg_get( freg, entry.id, NULL, buf, len, lenp );
}

int freg_ensure( struct freg_s *freg, uint64_t parentid, char *path, uint32_t flags, char *buf, int len, uint64_t *id ) {
  int sts;
  struct freg_entry entry;
  uint64_t pid;
  
  sts = freg_entry_by_name( freg, parentid, path, &entry, &pid );
  if( sts ) {    
    sts = freg_subkey( freg, 0, path, FREG_CREATE|FREG_VALUEPATH, NULL );
    if( sts ) return sts;
    sts = freg_put( freg, 0, path, flags, buf, len, id );
    if( sts ) return sts;
  } else if( (entry.flags & FREG_TYPE_MASK) != (flags & FREG_TYPE_MASK) ) {
    sts = freg_rem( freg, pid, entry.id );
    if( sts ) return sts;
    sts = freg_put( freg, 0, path, flags, buf, len, id );
    if( sts ) return sts;
  }

  return 0;
}

