
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
	    "               list [path]\n"
	    "               get path name\n"
	    "               put path name u32|u64|string|key value\n"
	    "               rem path name\n" 
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
    exit( 0 );
}

int main( int argc, char **argv ) {
  int sts, i;
  uint64_t id;
  
  sts = freg_open();
  if( sts ) usage( "Failed to open" );

  i = 1;
  if( (i >= argc) || (strcmp( argv[i], "list" ) == 0) ) {
    struct freg_entry *elist;
    int n;
    id = 0;
    
    i++;
    if( i < argc ) {
      sts = freg_subkey( 0, argv[i], 0, &id );
      if( sts ) usage( "Unknown path \"%s\"", argv[i] );
    }
    sts = freg_list( id, NULL, 0 );
    if( sts < 0 ) usage( "Failed to list" );
    elist = malloc( sizeof(*elist) * sts );
    n = freg_list( id, elist, sts );
    if( n < sts ) n = sts;
    printf( "%-64s %-8s %-8s\n", "Name", "Type", "Len" );
    for( i = 0; i < n; i++ ) {
      printf( "%-64s %-8s %-8u\n",
	      elist[i].name,
	      (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_UINT32 ? "u32" :
	      (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_UINT64 ? "u64" :
	      (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_STRING ? "string" :
	      (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_OPAQUE ? "opaque" :
	      (elist[i].flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ? "key" :
	      "unknown",
	      elist[i].len );
    }
    printf( "\n" );
    free( elist );
  } else if( strcmp( argv[i], "get" ) == 0 ) {
    uint32_t flags;
    char *buf;
    int len;
    
    i++;
    if( i >= argc ) usage( NULL );
    sts = freg_subkey( 0, argv[i], 0, &id );
    if( sts ) usage( "Failed to get key" );
    i++;
    if( i >= argc ) usage( NULL );
    sts = freg_get( id, argv[i], &flags, NULL, 0, &len );
    if( sts ) usage( "Failed to get" );
    printf( "xx len=%d\n", len );
    buf = malloc( len );
    sts = freg_get( id, argv[i], &flags, buf, len, NULL );
    if( sts ) usage( "Failed to get2" );
    switch( flags & FREG_TYPE_MASK ) {
    case FREG_TYPE_UINT32:
      printf( "%u\n", *(uint32_t *)buf );
      break;
    case FREG_TYPE_UINT64:
      printf( "%"PRIu64" (%"PRIx64")\n", *(uint64_t *)buf, *(uint64_t *)buf );
      break;
    case FREG_TYPE_KEY:
      printf( "%"PRIx64"\n", *(uint64_t *)buf );
      break;
    case FREG_TYPE_STRING:
      printf( "%s\n", buf );
      break;
    case FREG_TYPE_OPAQUE:
      for( i = 0; i < len; i++ ) {
	if( i && (i % 16 == 0) ) printf( "\n" );
	printf( "%02x ", (uint32_t)(uint8_t)buf[i] );
      }
      printf( "\n" );
      break;
    }
    free( buf );
    
  } else if( strcmp( argv[i], "put" ) == 0 ) {
    char *path, *name = NULL;
    uint32_t flags, u32;
    uint64_t u64;
    char *buf;
    int len;
    
    i++;
    if( i >= argc ) usage( NULL );
    path = argv[i];
    i++;
    if( i < argc ) {
      name = argv[i];
      i++;
      if( i >= argc ) usage( NULL );

      flags = 0;
      if( strcmp( argv[i], "u32" ) == 0 ) {
	flags = FREG_TYPE_UINT32;
      } else if( strcmp( argv[i], "u64" ) == 0 ) {
	flags = FREG_TYPE_UINT64;
      } else if( strcmp( argv[i], "string" ) == 0 ) {
	flags = FREG_TYPE_STRING;
      } else if( strcmp( argv[i], "key" ) == 0 ) {
	flags = FREG_TYPE_KEY;
      } else usage( NULL );
      
      i++;
      if( i >= argc ) usage( NULL );
    }
    
    sts = freg_subkey( 0, path, FREG_CREATE, &id );
    if( sts ) usage( "Failed to create subkey" );

    if( name ) {
      buf = NULL;
      len = 0;
      switch( flags & FREG_TYPE_MASK ) {
      case FREG_TYPE_UINT32:
	u32 = strtoul( argv[i], NULL, 10 );
	buf = (char *)&u32;
	len = sizeof(u32);
	break;
      case FREG_TYPE_UINT64:
	u64 = strtoull( argv[i], NULL, 10 );
	buf = (char *)&u64;
	len = sizeof(u64);
	break;
      case FREG_TYPE_STRING:
	buf = argv[i];
	len = strlen( argv[i] );
	break;
      case FREG_TYPE_KEY:
	u64 = strtoull( argv[i], NULL, 16 );
	buf = (char *)&u64;
	len = sizeof(u64);	
	break;
      default:
	usage( NULL );
      }
      
      freg_put( id, name, flags, buf, len );
    }
    
  } else if( strcmp( argv[i], "rem" ) == 0 ) {
    i++;
    if( i >= argc ) usage( NULL );
    sts = freg_subkey( 0, argv[i], 0, &id );
    if( sts ) usage( "Failed to get subkey" );
    i++;
    if( i >= argc ) usage( NULL );
    sts = freg_rem( id, argv[i] );
    if( sts ) usage( "Failed to remove value" );
  } else usage( NULL );
  
  freg_close();

  return 0;
}

