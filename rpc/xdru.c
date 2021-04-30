
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
	  "        v        void\n"
	  "        {...}    struct\n" 
	  "        O(...)   optional\n"
	  "        A(...)   array\n"
	  "        L(...)   list\n"
	  "        V(nnn:...;nnn:...;t:...)  union\n"
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

int xdr_main( int argc, char **argv ) {
  static char buf[32*1024];
  static char bufstr[64*1024];
  
  int sts, i;
  struct xdr_s xdr;
  uint32_t u32;
  uint64_t u64;
  char *term;
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

    xdr_init( &xdr, (uint8_t *)buf, sts );
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
static void decodeerror( struct xdr_s *xdr, char *msg ) {
  printf( "XDR decode error %s offset=%u", msg, xdr->offset );
  exit( 1 );
}

static char *decodevalue( struct xdr_s *xdr, char *fmt ) {
  int sts, i, b;
  uint32_t u32;
  int32_t i32;
  uint64_t u64;
  int64_t i64;
  char *str, *p, *bufp;
  int lenp;
  
  switch( *fmt ) {
  case 'v':
    fmt++;
    break;
  case 'u':
  case 'x':
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) decodeerror( xdr, "u32" );
    if( *fmt == 'u' ) printf( "%s%u", printed ? ", " : "", u32 );
    else printf( "%s%x", printed ? ", " : "", u32 );
    fmt++;
    printed = 1;
    break;
  case 'i':
    sts = xdr_decode_int32( xdr, &i32 );
    if( sts ) decodeerror( xdr, "i32" );
    printf( "%s%d", printed ? ", " : "", i32 );
    fmt++;
    printed = 1;    
    break;
  case 'U':
  case 'X':
    sts = xdr_decode_uint64( xdr, &u64 );
    if( sts ) decodeerror( xdr, "u64" );
    if( *fmt == 'U' ) printf( "%s%"PRIu64"", printed ? ", " : "", u64 );
    else printf( "%s%"PRIx64"", printed ? ", " : "", u64 );
    fmt++;
    printed = 1;    
    break;
  case 'I':
    sts = xdr_decode_int64( xdr, &i64 );
    if( sts ) decodeerror( xdr, "i64" );
    printf( "%s%"PRIi64"", printed ? ", " : "", i64 );
    fmt++;
    printed = 1;    
    break;
  case 's':
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) decodeerror( xdr, "string" );
    xdr->offset -= 4;
    str = malloc( u32 + 1 );
    sts = xdr_decode_string( xdr, str, u32 + 1 );
    if( sts ) decodeerror( xdr, "string" );
    printf( "%s\"%s\"", printed ? ", " : "", str );
    free( str );
    fmt++;
    printed = 1;    
    break;
  case 'b':
    sts = xdr_decode_boolean( xdr, &i32 );
    if( sts ) decodeerror( xdr, "boolean" );
    printf( "%s%s", printed ? ", " : "", i32 ? "True" : "False" );
    fmt++;
    printed = 1;    
    break;
  case 'o':
    sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&bufp, &lenp );
    if( sts ) decodeerror( xdr, "opaque" );
    str = malloc( (4 * lenp) / 3 + 5 );
    base64_encode( bufp, lenp, str );
    printf( "%s%s", printed ? ", " : "", str );
    free( str );
    fmt++;
    printed = 1;    
    break;
  case 'f':
    /* f(nnn) : nnn must parse as an int */
    fmt++;
    if( *fmt != '(' ) usage( "Bad format: expect ( after f" );
    fmt++;
    u32 = strtoul( fmt, &str, 0 );
    if( *str != ')' ) usage( "Bad format: expect ) after fixed num" );
    fmt = str + 1;
    bufp = malloc( u32 );
    sts = xdr_decode_fixed( xdr, (uint8_t *)bufp, u32 );
    if( sts ) decodeerror( xdr, "fixed" );
    str = malloc( 4*(u32 / 3) + 5 );
    base64_encode( bufp, u32, str );
    printf( "%s%s", printed ? ", " : "", str );
    free( str );
    free( bufp );
    printed = 1;    
    break;
  case 'A':
    /* A(...) */
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) decodeerror( xdr, "fixed" );
    fmt++;
    if( *fmt != '(' ) usage( "Bad format: expect ( after A" );
    fmt++;
    p = fmt;
    printf( "%s[", printed ? ", " : "" );
    printed = 0;    
    for( i = 0; i < u32; i++ ) {
      fmt = p;
      fmt = decodevalue( xdr, fmt );
      if( *fmt != ')' ) usage( "Bad format: expect ) after A" );      
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
    printf( "%s[", printed ? ", " : "" );
    printed = 0;    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) decodeerror( xdr, "list" );
    i = 0;
    while( b ) {
      fmt = p;
      fmt = decodevalue( xdr, fmt );
      if( *fmt != ')' ) usage( "Bad format: expect ) after L" );
      sts = xdr_decode_boolean( xdr, &b );
      if( sts ) decodeerror( xdr, "list" );
      i++;
    }
    printed = 1;    
    printf( "]" );
    fmt++;    
    break;
  case 'O':
    /* O(...) */
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) decodeerror( xdr, "optional" );
    fmt++;    
    if( *fmt != '(' ) usage( "Bad format: expect ( after O" );
    fmt++;
    if( b ) {
      fmt = decodevalue( xdr, fmt );
      if( *fmt != ')' ) usage( "Bad format: expect ) after O value" );
    } else {
      printf( "%s{}", printed ? ", " : "" );
      u32 = 1;
      while( *fmt ) {
	if( *fmt == ')' ) u32--;
	else if( *fmt == '(' ) u32++;
	
	if( !u32 ) break;
	fmt++;
      }
      if( !fmt ) usage( "Bad format: expect ) after O" );      
    }
    fmt++;
    break;
  case 'V':
    /* V(int:...;int:...;...) */
    fmt++;
    if( *fmt !=  '(' ) usage( "Bad format: expect ( after U" );
    fmt++;
    sts = xdr_decode_int32( xdr, &i32 );
    if( sts ) decodeerror( xdr, "union" );

    printf( "%s{ %d: ", printed ? ", " : "", i32 );
    printed = 0;
    
    while( *fmt && *fmt != ')' ) {
      if( *fmt == 't' ) {
	/* default final case */
	fmt++;
	if( *fmt != ':' ) usage( "Bad format: expect : after t" );
	fmt++;
	fmt = decodevalue( xdr, fmt );
	if( *fmt == '\0' ) usage( "Bad format: unexpected end of string" );
	if( *fmt != ')' ) usage( "Bad format: expect ) after final clause" );
	fmt++;
	break;
      } else {
	char *term;
	int32_t x = strtol( fmt, &term, 0 );
	if( *term != ':' ) usage( "Bad format: expect : after number" );
	fmt = term + 1;	
	if( x == i32 ) {
	  fmt = decodevalue( xdr, fmt );
	  if( *fmt == '\0' ) usage( "Bad format: unexpected end of string" );
	  else if( *fmt == ';' ) {
	    /* advance to end */
	    u32 = 1; /* paren counter - when hits zero we have hit end */
	    while( 1 ) {
	      if( *fmt == '\0' ) usage( "Bad format: unexpected end of string" );
	      if( *fmt == '(' ) u32++;
	      if( *fmt == ')' ) u32--;
	      if( u32 == 0 ) break;
	      fmt++;
	    }
	  } else if( *fmt != ')' ) usage( "Bad format: expect ; or ) after union clause" );
	  fmt++;
	  break;
	} else {
	  /* not a match - advance to next arm */
	  fmt++;
	  u32 = 0;
	  while( 1 ) {
	    if( *fmt == '\0' ) usage( "Bad format: unexpected end of string" );
	    if( *fmt == '(' ) u32++;
	    if( *fmt == ')' ) {
	      if( u32 == 0 ) break;
	      else u32--;
	    }
	    if( *fmt == ';' && (u32 == 0) ) break;
	    fmt++;
	  }
	  fmt++;
	}
      }
    }

    printf( "%s}", printed ? " " : "" );
    printed = 1;
    break;
  case '{':
    /* { ... } is a struct */
    fmt++;
    printf( "{" );
    printed = 0;
    while( *fmt && (*fmt != '}') ) {
      fmt = decodevalue( xdr, fmt );
    }
    if( !fmt ) usage( "Bad format: unexpected end of string after {" );
    printf( "}, " );
    printed = 1;
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
    printed = 0;
    fmt = decodevalue( xdr, fmt );
    printf( "\n" );
  }
}

#if 0

struct xstruct {
  int len;
  struct xvalue *vals;
};
struct xunion {
  int32_t tag;
  struct xvalue *val;
};
struct xarray {
  int len;
  struct xvalue *vals;
};

typedef enum {
    XVAL_U32,
    XVAL_I32,
    XVAL_U64,
    XVAL_I64,
    XVAL_STRING,
    XVAL_BOOLEAN,
    XVAL_OPAQUE,
    XVAL_FIXED,
    XVAL_ARRAY,
    XVAL_LIST,
    XVAL_OPTIONAL,
    XVAL_STRUCT,
} xvalue_t;

struct xvalue {
  xvalue_t type;
  union {
    uint32_t u32;
    int32_t i32;
    uint64_t u64;
    int64_t u64;
    int b;
    char *str;
    struct {
      int len;
      char *buf;
    } opaque;
    struct xstruct strct;
    struct xunion unin;
    struct xarray arr;
  } u;
};


/* 
 * need a function that takes as its input a format string and a value string and returns as its value 
 * an xvalue structure
 */

struct parse_s {
  char *fmt;
  char *valstr;  
};

static void skipwhitespace( struct parse_s *parse ) {
  while( *parse->valstr == ' ' ) {
    parse->valstr++;
  }    
}

static struct xvalue *parsevalue( struct parse_s *parse ) {
  /* 
   * walk the format string, building up an xvalue from the contents of hte valstring
   * contents of valstr is expected to match the format string
   */
  struct xvalue *xval;

  skipwhitespace( parse );
  
  switch( parse->fmt[0] ) {
  case '\0':
    /* end of format string */
    return NULL;
    break;
  case 'u':
    /* u32 decimal */
    {
      uint32_t u32;
      u32 = strtoul( parse->valstr, &term, 10 );
      parse->valstr = term;
      parse->fmt++;
      
      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_U32;
      xval->u.u32 = u32;
      return xval;
    }
    break;
  case 'x':
    /* u32 hex */
    {
      uint32_t u32;
      u32 = strtoul( parse->valstr, &term, 16 );
      parse->valstr = term;
      parse->fmt++;
      
      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_U32;
      xval->u.u32 = u32;
      return xval;
    }    
    break;
  case 'd':
    /* i32 */
    {
      int32_t i32;
      i32 = strtol( parse->valstr, &term, 10 );
      parse->valstr = term;
      parse->fmt++;
      
      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_I32;
      xval->u.i32 = i32;
      return xval;
    }    
    break;
  case 'U':
    {
      uint64_t u64;
      u64 = strtoull( parse->valstr, &term, 10 );
      parse->valstr = term;
      parse->fmt++;
      
      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_U64;
      xval->u.u64 = u64;
      return xval;
    }    
    break;
  case 'X':
    {
      uint64_t u64;
      u64 = strtoull( parse->valstr, &term, 16 );
      parse->valstr = term;
      parse->fmt++;
      
      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_U64;
      xval->u.u64 = u64;
      return xval;
    }        
    break;
  case 'D':
    {
      uint64_t i64;
      i64 = strtoll( parse->valstr, &term, 10 );
      parse->valstr = term;
      parse->fmt++;
      
      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_I64;
      xval->u.u64 = i64;
      return xval;
    }        
    break;
  case 's':
    {
      char *s, *p;
      int slen, sl;

      slen = 32;
      sl = 0;
      s = malloc( slen );
      
      parse->valstr++;
      p = parse->valstr;
      while( *p != '"' ) {
	if( *p == '\0' ) usage( "Unexpected end of string" );
	if( *p == '\\' ) {
	  p++;
	}
	
	s[sl] = *p;
	sl++;
	p++;	
	if( sl >= (slen - 1) ) {
	  slen = (3*slen) / 2;
	  s = realloc( slen );
	}
      }
      s[sl] = '\0';

      parse->valstr = p + 1;
      parse->fmt++;

      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_STRING;
      xval->u.str = s;
      return xval;
    }
    break;
  case 'b':
    {
      char tmp[6];
      int b;

      b = 0;
      memset( tmp, 0, 6 );
      memcpy( tmp, parse->valstr, 4 );
      if( strcasecmp( tmp, "true" ) ) {
	b = 1;
	parse->valstr += 4;
      } else {
	memcpy( tmp, parse->valstr, 5 );
	if( strcasecmp( tmp, "false" ) == 0 ) {
	  b = 0;
	  parse->valstr += 5;
	} else usage( "Bad boolean value" );
      }
      parse->fmt++;
      
      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_BOOLEAN;
      xval->u.b = b;
      return xval;
    }
    break;
  case 'o':
    {
      /* find end of base64 string */
      int i;
      char *p;
      p = parse->valstr;
      while( (*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || (*p == '/') || (*p == '+') || (*p == '=') ) {
	p++;
	i++;
      }
      p = malloc( i + 1 );
      memcpy( p, parse->valstr, i );
      p[i] = '\0';
      len = (3*i) / 4 + 5;
      buf = malloc( len );
      len = base64_decode( buf, len, p );
      if( len < 0 ) usage( "Bad base64 string" );
      free( p );
      
      fmt++;
      parse->valstr += i;

      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_OPAQUE;
      xval->u.opaque.len = len;
      xval->u.opaque.buf = buf;
      return xval;
    }
    break;
  case 'f':
    {
      uint32_t len, ln;
      char *term;
      int i;
      char *p;
      
      len = strtoul( parse->fmt, &term, 10 );
      if( *term != ')' ) usage( "Bad foramt string: expect ) after f" );
      parse->fmt = term + 1;

      p = parse->valstr;
      while( (*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || (*p == '/') || (*p == '+') || (*p == '=') ) {
	p++;
	i++;
      }
      p = malloc( i + 1 );
      memcpy( p, parse->valstr, i );
      p[i] = '\0';
      ln = (3*i) / 4 + 5;
      buf = malloc( ln );
      ln = base64_decode( buf, ln, p );
      if( ln < 0 ) usage( "Bad base64 string" );
      if( ln != len ) usage( "fixed length %d does not match format length %d", ln, len );
      free( p );
      
      parse->valstr += i;
      xval = malloc( sizeof(*xval) );
      xval->type = XVAL_FIXED;
      xval->u.opaque.len = len;
      xval->u.opaque.buf = buf;
      return xval;
    }
    break;
  case 'O':
    {
      parse->fmt++;
      if( *parse->fmt != '(' ) usage( "Bad format string: expect ( after O" );
      /* if value is {} then skip the format until closing ) otherwise extract the value and compare against format */
    }
    break;
  case 'A':
    break;
  case 'L':
    break;
  case '{':
    break;
  default:
    usage( "Bad format string" );
    break;
  }
}


static void encodevalue( struct xdr_s *xdr, char *fmt, struct xvalue *xval ) {
  
}


#endif
