
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

#include <fju/rpc.h>
#include <fju/sec.h>

static void decodeformat( struct xdr_s *xdr, char *fmt );

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "Usage: xdru encode [u32=*] [u64=*] [str=*] [opaque=*] [fixed=*]\n"
	  "            decode format [base64]\n"
	  "    Decode format:\n"
	  "        u|x      u32\n"
	  "        i        i32\n"
	  "        U|X      u64\n"
	  "        I        i64\n"
	  "        s        string\n"
	  "        b        boolean\n"
	  "        o        opaque\n"
	  "        f(nnn)   fixed\n"
	  "        O(...)   optional\n"
	  "        A(...)   array\n"
	  "        L(...)   list\n" 
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
    char *fmt;
    i++;
    if( i >= argc ) usage( NULL );
    fmt = argv[i];
    i++;
    sts = -1;
    if( i < argc ) {
      sts = base64_decode( buf, sizeof(buf), argv[i] );
      if( sts < 0 ) usage( "Bad base64 string %s", argv[i] );
    } else {
      char *tmpstr = malloc( 32*1024 );
      sts = fju_readstdin( tmpstr, 32*1024 );
      sts = base64_decode( buf, sizeof(buf), tmpstr );
      if( sts < 0 ) usage( "Failed to decode base64" );
      free( tmpstr );      
    }

    xdr_init( &xdr, buf, sts );
    decodeformat( &xdr, fmt );
    printf( "\n" );
    
    if( xdr.offset < xdr.count ) {
      base64_encode( (char *)(xdr.buf + xdr.offset), xdr.count - xdr.offset, bufstr );
      printf( "Remaining: %s\n", bufstr );
    }
  } else usage( NULL );
  
  return 0;
}

static int printed = 0;

static char *decodevalue( struct xdr_s *xdr, char *fmt ) {
  int sts, i, b;
  uint32_t u32;
  int32_t i32;
  uint64_t u64;
  int64_t i64;
  char *str, *p, *bufp;
  int lenp;
  
  switch( *fmt ) {
  case 'u':
  case 'x':
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) usage( "XDR error decode u32" );
    if( *fmt == 'u' ) printf( "%s%u", printed ? " " : "", u32 );
    else printf( "%s%x", printed ? " " : "", u32 );
    fmt++;
    printed = 1;
    break;
  case 'i':
    sts = xdr_decode_int32( xdr, &i32 );
    if( sts ) usage( "XDR error decode i32" );
    printf( "%s%d", printed ? " " : "", i32 );
    fmt++;
    printed = 1;    
    break;
  case 'U':
  case 'X':
    sts = xdr_decode_uint64( xdr, &u64 );
    if( sts ) usage( "XDR error decode u64" );
    if( *fmt == 'U' ) printf( "%s%"PRIu64"", xdr->offset > 0 ? " " : "", u64 );
    else printf( "%s%"PRIx64"", printed ? " " : "", u64 );
    fmt++;
    printed = 1;    
    break;
  case 'I':
    sts = xdr_decode_int64( xdr, &i64 );
    if( sts ) usage( "XDR error decode i64" );
    printf( "%s%"PRIi64"", printed ? " " : "", i64 );
    fmt++;
    printed = 1;    
    break;
  case 's':
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) usage( "XDR error decode string len" );
    xdr->offset -= 4;
    str = malloc( u32 + 1 );
    sts = xdr_decode_string( xdr, str, u32 + 1 );
    if( sts ) usage( "XDR error decode string" );
    printf( "%s%s", printed ? " " : "", str );
    free( str );
    fmt++;
    printed = 1;    
    break;
  case 'b':
    sts = xdr_decode_boolean( xdr, &i32 );
    if( sts ) usage( "XDR error decode bool" );
    printf( "%s%s", printed ? " " : "", i32 ? "True" : "False" );
    fmt++;
    printed = 1;    
    break;
  case 'o':
    sts = xdr_decode_opaque_ref( xdr, &bufp, &lenp );
    if( sts ) usage( "XDR error decode opaque" );
    str = malloc( (4 * lenp) / 3 + 5 );
    base64_encode( bufp, lenp, str );
    printf( "%s%s", printed ? " " : "", str );
    free( str );
    fmt++;
    printed = 1;    
    break;
  case 'f':
    /* f(nnn) : nnn must parse as an int */
    fmt++;
    if( *fmt != '(' ) usage( "Bad format: expect ( after f" );
    u32 = strtoul( fmt, &str, 0 );
    if( *str != ')' ) usage( "Bad format: expect ) after fixed num" );
    fmt = str + 1;
    bufp = malloc( u32 );
    sts = xdr_decode_fixed( xdr, bufp, u32 );
    if( sts ) usage( "XDR error decode fixed" );
    str = malloc( 4*(u32 / 3) + 5 );
    base64_encode( bufp, u32, str );
    printf( "%s%s", xdr->offset > 0 ? " " : "", str );
    free( str );
    free( bufp );
    printed = 1;    
    break;
  case 'A':
    /* A(...) */
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) usage( "XDR error decode array" );
    fmt++;
    if( *fmt != '(' ) usage( "Bad format: expect ( after A" );
    fmt++;
    p = fmt;
    printf( "[" );
    printed = 0;    
    for( i = 0; i < u32; i++ ) {
      if( i > 0 ) printf( "," );
      fmt = p;
      while( *fmt && (*fmt != ')') ) {
	fmt = decodevalue( xdr, fmt );
      }
      if( !fmt ) usage( "Bad format: expect ) after A" );      
    }
    printf( "]" );
    printed = 1;    
    fmt++;
    break;
  case 'L':
    /* L(...) */
    fmt++;
    if( *fmt != '(' ) usage( "Bad format: expect ( after L" );
    fmt++;
    p = fmt;
    printf( "[" );
    printed = 0;    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) usage( "XDR error decode list" );
    i = 0;
    while( b ) {
      if( i > 0 ) printf( "," );
      fmt = p;
      while( *fmt && (*fmt != ')') ) {
	fmt = decodevalue( xdr, fmt );
      }
      if( !fmt ) usage( "Bad format: expect ) after L" );
      sts = xdr_decode_boolean( xdr, &b );
      if( sts ) usage( "XDR error decode list" );
      i++;
    }
    printed = 1;    
    printf( "]" );
    fmt++;    
    break;
  case 'O':
    /* O(...) */
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) usage( "XDR error decode optional" );
    fmt++;    
    if( *fmt != '(' ) usage( "Bad format: expect ( after O" );
    fmt++;
    printf( "{" );
    printed = 0;    
    if( b ) {
      while( *fmt && (*fmt != ')') ) {
	fmt = decodevalue( xdr, fmt );
      }
      if( !fmt ) usage( "Bad format: expect ) after O" );
    } else {
      u32 = 1;
      while( *fmt ) {
	if( *fmt == ')' ) u32--;
	else if( *fmt == '(' ) u32++;
	
	if( !u32 ) break;
	fmt++;
      }
      if( !fmt ) usage( "Bad format: expect ) after O" );      
    }
    printed = 1;    
    printf( "}" );
    fmt++;
    break;
  default:
    usage( "Bad format character %c", *fmt );
    break;
  }

  return fmt;
}


static void decodeformat( struct xdr_s *xdr, char *fmt ) {
  while( *fmt ) {
    fmt = decodevalue( xdr, fmt );
  }
}
