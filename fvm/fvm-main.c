
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#define strcasecmp _stricmp
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <fju/fvm.h>
#include <fju/rpc.h>
#include <fju/freg.h>
#include <fju/sec.h>
#include <fju/dmb.h>
#include <fju/hostreg.h>

#include "fvm-private.h"

static void usage( char *fmt, ... ) {
  va_list args;

  if( fmt == NULL ) {
    printf( "fvm OPTIONS -m file|modname ... module/proc args...\n"
	    "\n"
	    "\n OPTIONS:\n"
	    "   -v            Verbose\n"
	    "\n" );
  }
  
  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
   
  exit( 1 );
}

static uint8_t argbuf[FVM_MAX_STACK];

int fvm_main( int argc, char **argv ) {
  int i, sts, nargs, j;
  char mname[64], pname[64];
  int procid;
  struct xdr_s argxdr, resxdr;
  struct fvm_module *module;
  int verbose = 0;
  uint64_t start, end;
  uint64_t siginfo;
  int rawargs = 0;

  freg_open( NULL, NULL );
  
  xdr_init( &argxdr, argbuf, sizeof(argbuf) );
  
  memset( mname, 0, sizeof(mname) );
  memset( pname, 0, sizeof(pname) );

  i = 1;
  if( i >= argc ) usage( NULL );

  while( i < argc ) {
    if( strcmp( argv[i], "-h" ) == 0 ) {
      usage( NULL );
    } else if( strcmp( argv[i], "--help" ) == 0 ) {
      usage( NULL );
    } else if( strcmp( argv[i], "-v" ) == 0 ) {
      verbose = 1;
      fvm_setdebug( 1 );
    } else if( strcmp( argv[i], "-m" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      sts = fvm_module_load_file( argv[i], 0, &module );
      if( sts ) {
	char path[256];
	char *textp;
	int lenp;

	sprintf( path, "/fju/fvm/modules/%s/path", argv[i] );
	sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_STRING, path, sizeof(path), &lenp );
	if( !sts ) {
	  sts = fvm_module_load_file( path, 0, &module );
	  if( sts ) usage( "Failed to load module from path %s", path );
	} else {
	  sprintf( path, "/fju/fvm/modules/%s/text", argv[i] );
	  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_OPAQUE, NULL, 0, &lenp );
	  if( sts ) usage( "Failed to load module %s from file or freg", argv[i] );
	  
	  textp = malloc( lenp );
	  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_OPAQUE, textp, lenp , &lenp );
	  if( sts ) usage( "Unexpected failure to load text segment" );
	  sts = fvm_module_load( textp, lenp, 0, &module );
	  if( sts ) usage( "Failed to load from freg module %s", path );
	  free( textp );
	}
      }

      if( !mname[0] ) strcpy( mname, module->name );
      if( !pname[0] ) strcpy( pname, module->procs[0].name );
    } else if( strcmp( argv[i], "--args" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      
      xdr_reset( &argxdr );
      sts = base64_decode( (char *)argxdr.buf, argxdr.count, argv[i] );
      if( sts < 0 ) usage( "Bad base64" );
      if( sts % 4 ) usage( "Base64 length %d not a multiple of 4", sts );
      argxdr.offset = sts;
      rawargs = 1;
    } else break;
    
    i++;
  }

  /* parse module/proc */
  if( i < argc ) {
    char *p, *q;
    
    p = strchr( argv[i], '/' );
    if( !p ) usage( "Expected modname/procname not %s", argv[i] );
    
    p = argv[i];
    q = mname;
    while( *p != '0' && *p != '/' ) {
      *q = *p;
      p++;
      q++;
    }
    *q = '\0';
    strncpy( pname, p + 1, FVM_MAX_NAME - 1 );

    i++;
  }

  if( !mname[0] || !pname[0] ) usage( NULL );

  module = fvm_module_by_name( mname );
  if( !module ) usage( "Unknown module %s", mname );
  procid = fvm_procid_by_name( module, pname );
  if( procid < 0 ) usage( "Unknown proc %s", pname );

  siginfo = module->procs[procid].siginfo;

  if( rawargs == 0 ) {
    nargs = 0;
    for( j = 0; j < FVM_SIGINFO_NARGS( siginfo ); j++ ) {
      if( !FVM_SIGINFO_ISVAR( siginfo, j ) ) {
	if( (j != (FVM_SIGINFO_NARGS( siginfo ) - 1)) &&
	    (FVM_SIGINFO_VARTYPE( siginfo, j + 1) == FVM_VARTYPE_OPAQUE) ) {
	} else {
	  nargs++;
	}
      }
    }
    
    if( (i + nargs) != argc ) usage( "Expected %d args, got %d", nargs, argc - i );
    
    for( j = 0; j < FVM_SIGINFO_NARGS( siginfo ); j++ ) {
      if( FVM_SIGINFO_ISVAR( siginfo, j ) ) continue;
      
      switch( FVM_SIGINFO_VARTYPE( siginfo, j ) ) {
      case FVM_VARTYPE_INT:
	if( ((j + 1) < FVM_SIGINFO_NARGS( siginfo )) &&
	    (FVM_SIGINFO_VARTYPE( siginfo, j + 1 ) == FVM_VARTYPE_OPAQUE ) ) {
	  /* do nothing - next arg is opaque */
	} else {
	  uint32_t u32;
	  char *term;
	  
	  u32 = strtol( argv[i], &term, 0 );
	  if( *term ) {
	    u32 = strtoll( argv[i], &term, 16 );
	    if( *term ) usage( "Failed to parse int %s", argv[i] );
	  }
	  xdr_encode_uint32( &argxdr, u32 );
	  i++;
	}
	break;
      case FVM_VARTYPE_STRING:
	xdr_encode_string( &argxdr, argv[i] );
	i++;
	break;
      case FVM_VARTYPE_OPAQUE:
	sts = base64_decode( (char *)argxdr.buf + argxdr.offset + 4,
			     argxdr.count - argxdr.offset - 4,
			     argv[i] );
	if( sts < 0 ) usage( "Failed to parse base64 %s", argv[i] );
	xdr_encode_uint32( &argxdr, sts );
	argxdr.offset += sts;
	if( sts % 4 ) argxdr.offset += 4 - (sts % 4);
	
	i++;
	break;
      }
      
    }
  }
  
  freg_open( NULL, NULL );
  hostreg_open();
  dmb_open();
  
  xdr_init( &resxdr, argbuf, sizeof(argbuf) );
  argxdr.count = argxdr.offset;
  argxdr.offset = 0;
  start = rpc_now();
  sts = fvm_run( module, procid, &argxdr, &resxdr );
  if( sts ) usage( "Failed to run %s/%s", mname, pname );
  end = rpc_now();
  if( verbose ) printf( "Runtime: %ums\n", (uint32_t)(end - start) );
  
  {
    int vartype, isvar;
    uint64_t siginfo;
    uint32_t u32;
    char str[4096];
    char opaque[2048];

    siginfo = module->procs[procid].siginfo;

    nargs = FVM_SIGINFO_NARGS(siginfo);
    for( i = 0; i < nargs; i++ ) {
      vartype = FVM_SIGINFO_VARTYPE(siginfo,i);
      isvar = FVM_SIGINFO_ISVAR(siginfo,i);
      if( isvar ) {
	/* var type */
	switch( vartype ) {
	case VAR_TYPE_U32:
	  if( (i < (nargs - 1)) && (FVM_SIGINFO_VARTYPE(siginfo, i + 1) == VAR_TYPE_OPAQUE) ) {
	  } else {
	    sts = xdr_decode_uint32( &resxdr, &u32 );
	    if( sts ) usage( "XDR error decoding u32" );
	    printf( "[%d] %d %u 0x%08x\n", i, u32, u32, u32 );
	  }
	  break;
	case VAR_TYPE_STRING:
	  sts = xdr_decode_string( &resxdr, str, sizeof(str) );
	  if( sts ) usage( "XDR error decoding string" );
	  printf( "[%d] %s\n", i, str );
	  break;
	case VAR_TYPE_OPAQUE:
	  u32 = sizeof(opaque);
	  sts = xdr_decode_opaque( &resxdr, (uint8_t *)opaque, (int *)&u32 );
	  if( sts ) usage( "XDR error decoding opaque" );
	  base64_encode( opaque, u32, str );
	  printf( "[%d] Len=%u %s\n", i, u32, str );
	  break;
	}
      }
    }
  }

  
  return 0;
}

