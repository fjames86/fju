
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
#include <Winsock2.h>
#include <Windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <time.h>

#include <fju/mmf.h>
#include <fju/freg.h>
#include <fju/sec.h>
#include <fju/ftab.h>
#include <fju/rpc.h>

#ifdef WIN32
#define PRIu64 "llu"
#define PRIx64 "llx"
#endif

static struct {
  char *path;
  struct freg_s *freg;  
  struct freg_s fregs;
  int quiet;
  int hexmode;
} glob;

static void usage( char *fmt, ... ) {
  if( !glob.quiet ) {
    fprintf( stderr, "Usage: [-p path] [-q] CMD args...\n"
	     "Where options:\n"
	     "     -p path   Set path to database file\n"
	     "     -q        Quiet mode, don't print to stderr on failure\n"
	     "     -x        Hex mode, read and print all numbers in hex rather than decimal\n"
	     "\n" 
	     "Where CMD:\n"
	     "               [list] [path]\n"
	     "               [get] path|id\n"
	     "               put path|id u32|u64|string|opaque|key [value]\n"
	     "               set path|id [name=NAME] [flags=FLAGS]\n" 
	     "               rem path\n"
	     "               dump [path]\n"
	     "               populate [count]\n"
	     "               reset\n"
	     "               prop\n"
	     "               time path|id u32|u64|string|opaque\n"
	     "\n"
	     );
    
    if( fmt ) {
      va_list args;
      fprintf( stderr, "Error: " );
      va_start( args, fmt );
      vfprintf( stderr, fmt, args );
      va_end( args );
      fprintf( stderr, "\n" );
    }
  }
  exit( 1 );
}

static void argval_split( char *instr, char *argname, char **argval ) {
    char *p;

    p = strchr( instr, '=' );
    if( p ) *argval = p + 1;
    else *argval = "";

    p = instr;
    while( *p != '0' && *p != '=' ) {
        *argname = *p;
        p++;
        argname++;
    }
    *argname = '\0';
}

static void cmd_get( int argc, char **argv, int i );
static void cmd_dump( uint64_t parentid, char *path );
static void cmd_populate( uint64_t parentid, int depth, int breadth, int *count );
static void cmd_put( int argc, char **argv, int i );

static void cmd_list( int argc, char **argv, int i ) {
  struct freg_entry *elist;
  int sts, n, j;
  uint64_t id;
  uint32_t u32;
  uint64_t u64;
  char *buf;
  int len;
  int printkey;
  
  id = 0;

  if( i < argc ) {
    sts = freg_subkey( glob.freg, 0, argv[i], 0, &id );
      if( sts ) {
	sts = freg_entry_by_name( glob.freg, 0, argv[i], NULL, NULL );
	  if( sts ) usage( "Unknown path \"%s\"", argv[i] );
	  cmd_get( argc, argv, i );
	  return;
      }
  }
    
  sts = freg_list( glob.freg, id, NULL, 0 );
  if( sts < 0 ) usage( "Failed to list" );
  elist = malloc( sizeof(*elist) * sts );
  n = freg_list( glob.freg, id, elist, sts );
  if( n < sts ) n = sts;
  printf( "%-16s %-8s %-24s %-6s %-8s\n", "ID", "Type", "Name", "Flags", "Len" );
  printkey = 1;
 again:
  for( i = 0; i < n; i++ ) {
    if( printkey && ((elist[i].flags & FREG_TYPE_MASK) != FREG_TYPE_KEY) ) continue;
    else if( !printkey && ((elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_KEY) ) continue;
    
    printf( "%016"PRIx64" %-8s %-24s %-6x %-8u ",
	    elist[i].id,
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_UINT32 ? "u32" :
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_UINT64 ? "u64" :
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_STRING ? "string" :
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_OPAQUE ? "opaque" :
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ? "key" :
	    "unknown",
	    elist[i].name,
	    elist[i].flags & ~FREG_TYPE_MASK,
	    elist[i].len );
    switch( elist[i].flags & FREG_TYPE_MASK ) {
    case FREG_TYPE_UINT32:
      sts = freg_get( glob.freg, elist[i].id, NULL, (char *)&u32, sizeof(u32), NULL );
      if( glob.hexmode ) printf( "%x", u32 );
      else printf( "%u", u32 );
      break;
    case FREG_TYPE_UINT64:
      sts = freg_get( glob.freg, elist[i].id, NULL, (char *)&u64, sizeof(u64), NULL );
      if( glob.hexmode ) printf( "%"PRIx64"", u64 );
      else printf( "%"PRIu64"", u64 );
      break;
    case FREG_TYPE_KEY:
      break;
    case FREG_TYPE_STRING:
      sts = freg_get( glob.freg, elist[i].id, NULL, NULL, 0, &len );
      buf = malloc( len );
      sts = freg_get( glob.freg, elist[i].id, NULL, buf, len, NULL );
      printf( "%s", buf );
      break;
    case FREG_TYPE_OPAQUE:
      sts = freg_get( glob.freg, elist[i].id, NULL, NULL, 0, &len );
      buf = malloc( len );
      sts = freg_get( glob.freg, elist[i].id, NULL, buf, len, NULL );
      for( j = 0; j < len; j++ ) {
	printf( "%02x", (uint32_t)(uint8_t)buf[j] );
      }
      break;      
    }
    
    printf( "\n" );
  }
  if( printkey ) {
    printkey = 0;
    goto again;
  }
  printf( "\n" );
  free( elist );  
}

static void cmd_get( int argc, char **argv, int ii ) {
  uint32_t flags;
  char *buf;
  int len, i, sts;
  uint64_t id;
  char *term;
  
  id = strtoull( argv[ii], &term, 16 );
  if( *term ) {
      struct freg_entry e;
      sts = freg_entry_by_name( glob.freg, 0, argv[ii], &e, NULL );
      if( sts ) usage( "Failed to get entry \"%s\"", argv[ii] );
      id = e.id;
  }
  
  sts = freg_get( glob.freg, id, &flags, NULL, 0, &len );
  if( sts ) usage( "Failed to get" );
  buf = malloc( len );
  sts = freg_get( glob.freg, id, &flags, buf, len, NULL );
  if( sts ) usage( "Failed to get data" );
  if( !glob.quiet ) {
      switch( flags & FREG_TYPE_MASK ) {
      case FREG_TYPE_UINT32:
	  if( glob.hexmode ) printf( "%x\n", *(uint32_t *)buf );
	  else printf( "%u\n", *(uint32_t *)buf );
	  break;
      case FREG_TYPE_UINT64:
	  if( glob.hexmode ) printf( "%"PRIx64"\n", *(uint64_t *)buf );
	  else printf( "%"PRIu64"\n", *(uint64_t *)buf );
	  break;
      case FREG_TYPE_KEY:
	  for( i = 0; i < len / sizeof(uint64_t); i++ ) {
	      uint64_t id = ((uint64_t *)buf)[i];
	      printf( "%"PRIx64" ", id );
	  }
	  break;
      case FREG_TYPE_STRING:
	  printf( "%s\n", buf );
	  break;
      case FREG_TYPE_OPAQUE:
	  for( i = 0; i < len; i++ ) {
	      printf( "%02x", (uint32_t)(uint8_t)buf[i] );
	  }
	  printf( "\n" );
	  break;
      }
  }
  free( buf );
}

int main( int argc, char **argv ) {
  int sts, i;
  uint64_t id;

  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-p" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      glob.path = argv[i];
      glob.freg = &glob.fregs;
    } else if( strcmp( argv[i], "-q" ) == 0 ) {
      glob.quiet = 1;
    } else if( strcmp( argv[i], "-x" ) == 0 ) {
      glob.hexmode = 1;
    } else break;
    i++;
  }

  sts = freg_open( glob.path, glob.freg );
  if( sts ) usage( "Failed to open" );
    
  if( i >= argc ) {
    cmd_list( argc, argv, i );
  } else if( (strcmp( argv[i], "-h" ) == 0) || (strcmp( argv[i], "--help" ) == 0) || (strcmp( argv[i], "help") == 0) ) {
    usage( NULL );
  } else if( strcmp( argv[i], "list" ) == 0 ) {
    i++;
    cmd_list( argc, argv, i );
  } else if( strcmp( argv[i], "get" ) == 0 ) {
    i++;
    if( i >= argc ) usage( NULL );
    cmd_get( argc, argv, i );
  } else if( strcmp( argv[i], "put" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      cmd_put( argc, argv, i );
  } else if( strcmp( argv[i], "rem" ) == 0 ) {
      struct freg_entry e;
      uint64_t parentid;
      
      i++;
      while( i < argc ) {
	  sts = freg_entry_by_name( glob.freg, 0, argv[i], &e, &parentid );
	  if( sts ) printf( "Failed to find entry \"%s\"\n", argv[i] );
	  if( !sts ) {
	      sts = freg_rem( glob.freg, parentid, e.id );
	      if( sts ) printf( "Failed to remove value\n" );
	  }
	  i++;
      }
  } else if( strcmp( argv[i], "dump" ) == 0 ) {
    uint64_t parentid = 0;
    char *path = "";
    
    i++;
    if( i < argc ) {
      sts = freg_subkey( glob.freg, 0, argv[i], 0, &parentid );
      if( sts ) usage( "Unknown key \"%s\"", argv[i] );
    }
    cmd_dump( parentid, path );
  } else if( strcmp( argv[i], "populate" ) == 0 ) {
    int count = 100;
    i++;
    if( i < argc ) {
      count = strtoul( argv[i], NULL, 10 );
    }
    while( count ) {
      cmd_populate( 0, 8, 8, &count );
    }
  } else if( strcmp( argv[i], "reset" ) == 0 ) {
    struct ftab_s ftab;
    char cookie[FTAB_MAX_COOKIE];
    ftab_open( glob.path ? glob.path : mmf_default_path( "freg.dat", NULL ), NULL, &ftab );
    ftab_reset( &ftab );
    memset( cookie, 0, sizeof(cookie) );
    ftab_set_cookie( &ftab, cookie );
    ftab_close( &ftab );
  } else if( strcmp( argv[i], "time" ) == 0 ) {
      uint64_t start, end;
      int j, niter;
      i++;
      if( i >= argc ) usage( NULL );

      niter = 1000000;
      
      start = rpc_now();
      for( j = 0; j < niter; j++ ) {
	cmd_get( argc, argv, i );
      }
      end = rpc_now();
      printf( "%d cmd_get %dms %.3fus per call\n", niter, (int)(end - start), (float)(1000 * (end - start))/(float)niter );
      
      start = rpc_now();
      for( j = 0; j < niter; j++ ) {
	cmd_put( argc, argv, i );
      }
      end = rpc_now();
      printf( "%d cmd_put %dms %.3fus per call\n", niter, (int)(end - start), (float)(1000 * (end - start))/(float)niter );
  } else if( strcmp( argv[i], "prop" ) == 0 ) {
    struct ftab_s ftab;
    struct ftab_prop prop;
    ftab_open( glob.path ? glob.path : mmf_default_path(  "freg.dat", NULL ), NULL, &ftab );
    ftab_prop( &ftab, &prop );
    ftab_close( &ftab );

    printf( "Inodes %u/%u (%u%%) Data %ukb/%ukb Root %"PRIx64" LBASize %u\n",
	    prop.count, prop.max, (100*prop.count)/prop.max,
	    (prop.lbasize*prop.count) / 1024,
	    (prop.lbasize*prop.max) / 1024,
	    *((uint64_t *)prop.cookie),
	    prop.lbasize );
  } else if( strcmp( argv[i], "set" ) == 0 ) {
    char *namep = NULL;
    char *term;
    uint32_t flags, *flagsp = NULL;
    char argname[64], *argval;
    
    i++;
    if( i >= argc ) usage( NULL );
    id = strtoull( argv[i], &term, 16 );
    if( *term ) {
      struct freg_entry entry;
      sts = freg_entry_by_name( glob.freg, 0, argv[i], &entry, NULL );
      if( sts ) usage( "Failed to find entry" );
      id = entry.id;
    }
    i++;
    while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "name" ) == 0 ) {
	namep = argval;
      } else if( strcmp( argname, "flags" ) == 0 ) {
	flags = strtoul( argval, NULL, 16 );
	flagsp = &flags;
      } else usage( NULL );
      i++;
    }
    
    sts = freg_set( glob.freg, id, namep, flagsp, NULL, 0 );
    if( sts ) usage( "Failed to set" );
  } else cmd_list( argc, argv, i );
  
  freg_close( glob.freg );

  return 0;
}

static void cmd_get2( uint64_t parentid, char *path, char *name ) {
  uint32_t flags;
  char *buf;
  int i, sts;
  struct freg_entry e;

  sts = freg_entry_by_name( glob.freg, parentid, name, &e, NULL );
  if( sts ) return;
  buf = malloc( e.len );
  sts = freg_get( glob.freg, e.id, &flags, buf, e.len, NULL );
  if( sts ) goto done;
  switch( flags & FREG_TYPE_MASK ) {
  case FREG_TYPE_UINT32:
    if( glob.hexmode ) printf( "%s/%s u32 %x\n", path, name, *(uint32_t *)buf );
    else printf( "%s/%s u32 %u\n", path, name, *(uint32_t *)buf );
    break;
  case FREG_TYPE_UINT64:
    if( glob.hexmode ) printf( "%s/%s u64 %"PRIx64"\n", path, name, *(uint64_t *)buf );
    else printf( "%s/%s u64 %"PRIu64"\n", path, name, *(uint64_t *)buf );
    break;
  case FREG_TYPE_KEY:
    printf( "%s/%s key\n", path, name );
    break;
  case FREG_TYPE_STRING:
    printf( "%s/%s str %s\n", path, name, buf );
    break;
  case FREG_TYPE_OPAQUE:
    printf( "%s/%s opaque ", path, name );
    for( i = 0; i < e.len; i++ ) {
      printf( "%02x", (uint32_t)(uint8_t)buf[i] );
    }
    printf( "\n" );
    break;
  }
 done:
  free( buf );
}

static void cmd_dump( uint64_t parentid, char *path ) {
  int i;
  struct freg_entry *elist;
  int nelist, n;
  char *ppath;

  nelist = freg_list( glob.freg, parentid, NULL, 0 );
  if( nelist <= 0 ) return;
  elist = malloc( sizeof(*elist) * nelist );
  n = freg_list( glob.freg, parentid, elist, nelist );
  if( n < nelist ) nelist = n;
  
  /* print data first */
  for( i = 0; i < nelist; i++ ) {
    switch( elist[i].flags & FREG_TYPE_MASK ) {
    case FREG_TYPE_KEY:
      break;
    default:
      cmd_get2( parentid, path, elist[i].name );
      break;	
    }
  }
  /* recurse into subkeys second */
  for( i = 0; i < nelist; i++ ) {
    switch( elist[i].flags & FREG_TYPE_MASK ) {
    case FREG_TYPE_KEY:
      /* recurse */
      ppath = malloc( strlen( path ) + strlen( elist[i].name ) + 2 );
      sprintf( ppath, "%s/%s", path, elist[i].name );
      printf( "%s key\n", ppath );
      cmd_dump( elist[i].id, ppath );
      free( ppath );
      break;
    default:
      break;
    }
  }  

  free( elist );
}

static void cmd_populate( uint64_t parentid, int depth, int breadth, int *count ) {
  char name[FREG_MAX_NAME];
  uint32_t u32;
  uint64_t id;
  int sts, i;

  for( i = 0; i < depth; i++ ) {
    if( !(*count) ) return;
    u32 = sec_rand_uint32();
    sprintf( name, "%u", u32 );    
    sts = freg_subkey( glob.freg, parentid, name, FREG_CREATE, &id );
    if( sts ) {
      *count = 0;
      return;
    }
    (*count)--;
    cmd_populate( id, depth - 1, breadth, count );
  }

  for( i = 0; i < breadth; i++ ) {
    if( !(*count) ) return;
    u32 = sec_rand_uint32();
    sprintf( name, "%u", u32 );    
    if( u32 % 2 ) {
      sts = freg_put( glob.freg, parentid, name, FREG_TYPE_UINT32, (char *)&u32, sizeof(u32), NULL );
      if( sts ) {
	*count = 0;
	return;
      }
    } else {
      sts = freg_put( glob.freg, parentid, name, FREG_TYPE_STRING, name, strlen( name ) + 1, NULL );
      if( sts ) {
	*count = 0;
	return;
      }
    }
    
    (*count)--;
  }
  
}

static void cmd_put( int argc, char **argv, int i ) {
    char *path;
    uint32_t flags, u32;
    uint64_t u64, id, setid;
    char *buf;
    int len, sts, j;
    char tmpstr[32];
    char *term;
    
    path = argv[i];
    setid = strtoull( path, &term, 16 );
    if( *term ) setid = 0;
    
    i++;

    flags = FREG_TYPE_KEY;
    if( i < argc ) {
      if( strcmp( argv[i], "u32" ) == 0 ) {
	flags = FREG_TYPE_UINT32;
      } else if( strcmp( argv[i], "u64" ) == 0 ) {
	flags = FREG_TYPE_UINT64;
      } else if( (strcmp( argv[i], "string" ) == 0) || (strcmp( argv[i], "str" ) == 0) ) {
	flags = FREG_TYPE_STRING;
      } else if( (strcmp( argv[i], "opaque" ) == 0) ) {
	flags = FREG_TYPE_OPAQUE;
      } else if( strcmp( argv[i], "key" ) == 0 ) {
	flags = FREG_TYPE_KEY;
      } else usage( NULL );    
      
      i++;
    }
    
    buf = NULL;
    len = 0;
    u32 = 0;
    u64 = 0;
    switch( flags & FREG_TYPE_MASK ) {
    case FREG_TYPE_KEY:
      sts = freg_subkey( glob.freg, 0, path, FREG_CREATE, &id );
      if( sts ) usage( "Failed to create subkey" );      
      break;
    case FREG_TYPE_UINT32:
      if( i < argc ) {
	char *term;
	u32 = strtoul( argv[i], &term, glob.hexmode ? 16 : 10 );
	if( *term ) usage( "Failed to parse u32" );
      }
      buf = (char *)&u32;
      len = sizeof(u32);
      break;
    case FREG_TYPE_UINT64:
      if( i < argc ) {
	char *term;
	u64 = strtoull( argv[i], &term, glob.hexmode ? 16 : 10 );
	if( *term ) usage( "Failed to parse u64 \"%s\"", argv[i] );
      }
      buf = (char *)&u64;
      len = sizeof(u64);
      break;
    case FREG_TYPE_STRING:
      if( i < argc ) {
	buf = NULL;
	while( i < argc ) {
	  if( buf ) buf = realloc( buf, len + strlen( argv[i] ) );
	  else buf = malloc( strlen( argv[i] ) );
	  memcpy( buf + len, argv[i], strlen( argv[i] ) );
	  len += strlen( argv[i] );

	  i++;
	  if( i < argc ) {
	    buf = realloc( buf, len + 1 );
	    memcpy( buf + len, " ", 1 );
	    len += 1;
	  } else {
	    buf = realloc( buf, len + 1 );
	    memcpy( buf + len, "", 1 );
	    len += 1;
	  }
	}	
      } else {
	buf = NULL;
	len = 0;
      }
      break;
    case FREG_TYPE_OPAQUE:
      len = 0;
      buf = malloc( 4096 );
      while( i < argc ) {	
	for( j = 0; j < strlen( argv[i] ) / 2; j++ ) {
	  tmpstr[0] = argv[i][2*j];
	  tmpstr[1] = argv[i][2*j + 1];
	  tmpstr[2] = '\0';
	  buf[len] = strtoul( tmpstr, NULL, 16 );
	  len++;
	}
	i++;
      }
      break;
    default:
      usage( NULL );
      break;
    }

    if( (flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) {
      if( setid ) sts = freg_set( glob.freg, setid, NULL, NULL, buf, len );
      else sts = freg_put( glob.freg, 0, path, flags, buf, len, NULL );
      if( sts ) usage( "Failed to put" );
    }

    if( ((flags & FREG_TYPE_MASK) == FREG_TYPE_STRING) && buf ) free( buf );
}

