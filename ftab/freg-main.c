
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

#ifdef WIN32
#define PRIu64 "llu"
#define PRIx64 "llx"
#endif

static void usage( char *fmt, ... ) {
    printf( "Usage: CMD args...\n"
	    "Where CMD:\n"
	    "               [list] [path]\n"
	    "               [get] path\n"
	    "               put path u32|u64|string|opaque|key [value]\n"
	    "               rem path\n"
	    "               dump [path]\n" 
	    "\n"
    );

    if( fmt ) {
        va_list args;
        printf( "Error: " );
        va_start( args, fmt );
        vprintf( fmt, args );
        va_end( args );
        printf( "\n" );
    }
    exit( 1 );
}

static void cmd_get( char *path );
static void cmd_dump( uint64_t parentid, char *path );

static void cmd_list( int argc, char **argv, int i ) {
  struct freg_entry *elist;
  int sts, n, j;
  uint64_t id;
  uint32_t u32;
  uint64_t u64;
  char *buf;
  int len;
    
  id = 0;

  if( i < argc ) {
    sts = freg_subkey( 0, argv[i], 0, &id );
    if( sts ) {
      sts = freg_get( 0, argv[i], NULL, NULL, 0, NULL );
      if( sts ) usage( "Unknown path \"%s\"", argv[i] );
      cmd_get( argv[i] );
      return;
    }
  }
    
  sts = freg_list( id, NULL, 0 );
  if( sts < 0 ) usage( "Failed to list" );
  elist = malloc( sizeof(*elist) * sts );
  n = freg_list( id, elist, sts );
  if( n < sts ) n = sts;
  printf( "%-8s %-32s %-8s\n", "Type", "Name", "Len" );
  for( i = 0; i < n; i++ ) {
    printf( "%-8s %-32s %-8u ",
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_UINT32 ? "u32" :
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_UINT64 ? "u64" :
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_STRING ? "string" :
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_OPAQUE ? "opaque" :
	    (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ? "key" :
	    "unknown",
	    elist[i].name,
	    elist[i].len );
    switch( elist[i].flags & FREG_TYPE_MASK ) {
    case FREG_TYPE_UINT32:
      sts = freg_get( id, elist[i].name, NULL, (char *)&u32, sizeof(u32), NULL );
      printf( "%u (%x)", u32, u32 );
      break;
    case FREG_TYPE_UINT64:
      sts = freg_get( id, elist[i].name, NULL, (char *)&u64, sizeof(u64), NULL );
      printf( "%"PRIu64" (%"PRIx64")", u64, u64 );
      break;
    case FREG_TYPE_KEY:
      sts = freg_get( id, elist[i].name, NULL, (char *)&u64, sizeof(u64), NULL );
      printf( "%"PRIx64"", u64 );      
      break;
    case FREG_TYPE_STRING:
      sts = freg_get( id, elist[i].name, NULL, NULL, 0, &len );
      buf = malloc( len );
      sts = freg_get( id, elist[i].name, NULL, buf, len, NULL );
      printf( "%s", buf );
      break;
    case FREG_TYPE_OPAQUE:
      sts = freg_get( id, elist[i].name, NULL, NULL, 0, &len );
      buf = malloc( len );
      sts = freg_get( id, elist[i].name, NULL, buf, len, NULL );
      for( j = 0; j < len; j++ ) {
	printf( "%02x", (uint32_t)(uint8_t)buf[j] );
      }
      break;      
    }
    
    printf( "\n" );
  }
  printf( "\n" );
  free( elist );  
}

static void cmd_get( char *path ) {
  uint32_t flags;
  char *buf;
  int len, i, sts;
    
  sts = freg_get( 0, path, &flags, NULL, 0, &len );
  if( sts ) usage( "Failed to get" );
  buf = malloc( len );
  sts = freg_get( 0, path, &flags, buf, len, NULL );
  if( sts ) usage( "Failed to get data" );
  switch( flags & FREG_TYPE_MASK ) {
  case FREG_TYPE_UINT32:
    printf( "%u %x\n", *(uint32_t *)buf, *(uint32_t *)buf );
    break;
  case FREG_TYPE_UINT64:
    printf( "%"PRIu64" %"PRIx64"\n", *(uint64_t *)buf, *(uint64_t *)buf );
    break;
  case FREG_TYPE_KEY:
    printf( "%"PRIx64"\n", *(uint64_t *)buf );
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
  free( buf );
}

int main( int argc, char **argv ) {
  int sts, i, j;
  uint64_t id;
  char tmpstr[32];
  
  sts = freg_open();
  if( sts ) usage( "Failed to open" );

  i = 1;
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
    cmd_get( argv[i] );
  } else if( strcmp( argv[i], "put" ) == 0 ) {
    char *path;
    uint32_t flags, u32;
    uint64_t u64;
    char *buf;
    int len;
    
    i++;
    if( i >= argc ) usage( NULL );
    path = argv[i];
    i++;
    if( i >= argc ) usage( NULL );
    flags = 0;
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
    
    buf = NULL;
    len = 0;
    u32 = 0;
    u64 = 0;
    switch( flags & FREG_TYPE_MASK ) {
    case FREG_TYPE_KEY:
      sts = freg_subkey( 0, path, FREG_CREATE, &id );
      if( sts ) usage( "Failed to create subkey" );      
      break;
    case FREG_TYPE_UINT32:
      if( i < argc ) {
	char *term;
	u32 = strtoul( argv[i], &term, 10 );
	if( *term ) {
	  u32 = strtoul( argv[i], &term, 16 );
	  if( *term ) usage( "Failed to parse u32" );
	}
      }
      buf = (char *)&u32;
      len = sizeof(u32);
      break;
    case FREG_TYPE_UINT64:
      if( i < argc ) {
	char *term;
	u64 = strtoull( argv[i], &term, 10 );
	if( *term ) {
	  u64 = strtoull( argv[i], &term, 16 );
	  if( *term ) usage( "Failed to parse u64 \"%s\"", argv[i] );
	}
      }
      buf = (char *)&u64;
      len = sizeof(u64);
      break;
    case FREG_TYPE_STRING:
      if( i < argc ) {
	buf = argv[i];
	len = strlen( argv[i] ) + 1;
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
      sts = freg_put( 0, path, flags, buf, len );
      if( sts ) usage( "Failed to put" );
    }
  } else if( strcmp( argv[i], "rem" ) == 0 ) {
    i++;
    if( i >= argc ) usage( NULL );
    sts = freg_rem( 0, argv[i] );
    if( sts ) usage( "Failed to remove value" );
  } else if( strcmp( argv[i], "dump" ) == 0 ) {
    uint64_t parentid = 0;
    char *path = "";
    
    i++;
    if( i < argc ) {
      sts = freg_subkey( 0, argv[i], 0, &parentid );
      if( sts ) usage( "Unknown key \"%s\"", argv[i] );
    }
    cmd_dump( parentid, path );
  } else cmd_list( argc, argv, i );
  
  freg_close();

  return 0;
}

static void cmd_get2( uint64_t parentid, char *path, char *name ) {
  uint32_t flags;
  char *buf;
  int len, i, sts;
    
  sts = freg_get( parentid, name, &flags, NULL, 0, &len );
  if( sts ) return;
  buf = malloc( len );
  sts = freg_get( parentid, name, &flags, buf, len, NULL );
  if( sts ) goto done;
  switch( flags & FREG_TYPE_MASK ) {
  case FREG_TYPE_UINT32:
    printf( "%s/%s u32 %u\n", path, name, *(uint32_t *)buf );
    break;
  case FREG_TYPE_UINT64:
    printf( "%s/%s u64 %"PRIu64"\n", path, name, *(uint64_t *)buf );
    break;
  case FREG_TYPE_KEY:
    printf( "%s/%s key\n", path, name );
    break;
  case FREG_TYPE_STRING:
    printf( "%s/%s str %s\n", path, name, buf );
    break;
  case FREG_TYPE_OPAQUE:
    printf( "%s/%s opaque ", path, name );
    for( i = 0; i < len; i++ ) {
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
  uint64_t u64;
  struct freg_entry *elist;
  int nelist, n;
  char *ppath;
  
  nelist = freg_list( parentid, NULL, 0 );
  if( nelist <= 0 ) return;
  elist = malloc( sizeof(*elist) * nelist );
  n = freg_list( parentid, elist, nelist );
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
      freg_get( parentid, elist[i].name, NULL, (char *)&u64, sizeof(u64), NULL );
      
      ppath = malloc( strlen( path ) + strlen( elist[i].name ) + 2 );
      sprintf( ppath, "%s/%s", path, elist[i].name );
      printf( "%s key\n", ppath );
      cmd_dump( u64, ppath );
      free( ppath );
      break;
    default:
      break;
    }
  }  

  free( elist );
}
