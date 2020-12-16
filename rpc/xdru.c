
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

#include <fju/rpc.h>
#include <fju/sec.h>

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "Usage: xdru encode [u32=*] [u64=*] [str=*] [opaque=*] [fixed=*]\n"
	  "            decode base64 [u32] [u64] [str] [opaque] [fixed=count]\n"
	  );

  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
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

int main( int argc, char **argv ) {
  static char buf[32*1024];
  static char bufstr[64*1024];
  
  int sts, i, len;
  struct xdr_s xdr;
  uint32_t u32;
  uint64_t u64;
  char str[1024], *term;
  char argname[64], *argval;
  
  if( argc < 2 ) usage( NULL );
  
  i = 1;
  if( strcmp( argv[i], "encode" ) == 0 ) {
    xdr_init( &xdr, (uint8_t *)buf, sizeof(buf) );    
    i++;
    while( i < argc ) {
      argval_split( argv[i], argname, &argval );
      if( strcmp( argname, "u32" ) == 0 ) {
	u32 = strtoul( argval, &term, 0 );
	if( *term ) usage( "Failed to parse u32" );
	xdr_encode_uint32( &xdr, u32 );
      } else if( strcmp( argname, "u64" ) == 0 ) {
	u64 = strtoull( argval, &term, 0 );
	if( *term ) usage( "Failed to parse u64" );
	xdr_encode_uint64( &xdr, u64 );
      } else if( strcmp( argname, "str" ) == 0 ) {
	xdr_encode_string( &xdr, argval );
      } else if( strcmp( argname, "opaque" ) == 0 ) {
	sts = base64_decode( (char *)(xdr.buf + xdr.offset + 4), xdr.count - xdr.offset - 4, argval );
	if( sts < 0 ) usage( "Failed to parse base64 opaque %s", argval );
	
	xdr_encode_uint32( &xdr, sts );
	xdr.offset += sts;
	if( sts % 4 ) xdr.offset += 4 - (sts % 4);
      } else if( strcmp( argname, "fixed" ) == 0 ) {
	sts = base64_decode( (char *)(xdr.buf + xdr.offset), xdr.count - xdr.offset, argval );
	if( sts < 0 ) usage( "Failed to parse base64 fixed" );
	if( sts % 4 ) usage( "Fixed length %d not a multiple of 4", sts );
	xdr.offset += sts;
      } else usage( NULL );
      i++;
    }

    base64_encode( buf, xdr.offset, bufstr );
    printf( "%s\n", bufstr );
  } else if( strcmp( argv[i], "decode" ) == 0 ) {
    i++;    
    sts = -1;
    if( i < argc ) {
      sts = base64_decode( buf, sizeof(buf), argv[i] );
    }
    
    if( sts < 0 ) {
      /* failed to decode the stinrg passed - try reading from stdin? */
      char *tmpstr = malloc( 32*1024 );
      sts = fju_readstdin( tmpstr, 32*1024 );
      sts = base64_decode( buf, sizeof(buf), tmpstr );
      if( sts < 0 ) usage( "Failed to decode base64" );
      free( tmpstr );      
    } else {
      i++;
    }
    if( sts % 4 ) usage( "XDR buffer not a multiple of 4" );
    xdr_init( &xdr, (uint8_t *)buf, sts );
    while( i < argc ) {
      argval_split( argv[i], argname, &argval );      
      if( strcmp( argname, "u32" ) == 0 ) {
	sts = xdr_decode_uint32( &xdr, &u32 );
	if( sts ) usage( "XDR Error decoding uint32" );
	printf( "%u ", u32 );
      } else if( strcmp( argname, "x32" ) == 0 ) {
	sts = xdr_decode_uint32( &xdr, &u32 );
	if( sts ) usage( "XDR Error decoding uint32" );
	printf( "%x ", u32 );	
      } else if( strcmp( argname, "u64" ) == 0 ) {
	sts = xdr_decode_uint64( &xdr, &u64 );
	if( sts ) usage( "XDR Error decoding uint64" );
	printf( "%"PRIu64" ", u64 );
      } else if( strcmp( argname, "x64" ) == 0 ) {
	sts = xdr_decode_uint64( &xdr, &u64 );
	if( sts ) usage( "XDR Error decoding uint64" );
	printf( "%"PRIx64" ", u64 );	
      } else if( strcmp( argname, "str" ) == 0 ) {
	sts = xdr_decode_string( &xdr, str, sizeof(str) );
	if( sts ) usage( "XDR Error decoding string" );
	printf( "%s ", str );
      } else if( strcmp( argname, "opaque" ) == 0 ) {
	sts = xdr_decode_uint32( &xdr, (uint32_t *)&len );
	if( sts ) usage( "XDR Error decoding opaque" );
	if( xdr.count - xdr.offset < len ) usage( "XDR Error count=%u offset=%u len=%u", xdr.count, xdr.offset, len );
	base64_encode( (char *)(xdr.buf + xdr.offset), len, bufstr );
	printf( "%s ", bufstr );
	xdr.offset += len;
	if( len % 4 ) xdr.offset += 4 - (len % 4);
      } else if( strcmp( argname, "fixed" ) == 0 ) {
	if( !argval ) usage( "Need fixed length" );
	len = strtoul( argval, &term, 0 );
	if( *term ) usage( "Failed to parse fixed len" );
	if( len % 4 ) usage( "Fixed len %d must be multiple of 4", len );
	base64_encode( (char *)(xdr.buf + xdr.offset), len, bufstr );
	printf( "%s ", bufstr );
	xdr.offset += len;
      } else usage( NULL );
      i++;
    }
    printf( "\n" );
    
    if( xdr.offset < xdr.count ) {
      base64_encode( (char *)(xdr.buf + xdr.offset), xdr.count - xdr.offset, bufstr );
      printf( "Remaining %u: %s\n", xdr.count - xdr.offset, bufstr );
    }
  } else usage( NULL );
  
  return 0;
}


