
#include "fjui.h"

#define CMD_ENCODE 1
#define CMD_DECODE 2

static void xdr_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "xdr_args" );
	SetWindowPos( h, HWND_TOP, 60, 5, width - 225, 25, 0 );
	h = fjui_get_hwnd( "xdr_b64" );
	SetWindowPos( h, HWND_TOP, 60, 35, width - 65, 25, 0 );
	h = fjui_get_hwnd( "xdr_encode" );
	SetWindowPos( h, HWND_TOP, width - 160, 5, 75, 25, 0 );
	h = fjui_get_hwnd( "xdr_decode" );
	SetWindowPos( h, HWND_TOP, width - 80, 5, 75, 25, 0 );

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


static void xdr_command_encode( HWND hwnd, HWND hargs, HWND hb64 ) {
	struct xdr_s xdr;
	int sts, i;
	char argstr[256];
	char b64buf[256];
	char b64str[512];
	char *p, *term, *argval, argname[64];
	uint32_t u32;
	uint64_t u64;

	GetWindowTextA( hargs, argstr, sizeof(argstr) );

	xdr_init( &xdr, (uint8_t *)b64buf, sizeof(b64buf) );
	p = strtok( argstr, " " );
	if( !p ) p = argstr;
	while( p ) {
		argval_split( p, argname, &argval );
		if( strcmp( argname,"u32" ) == 0 ) {
			u32 = strtoul( argval, &term, 0 );
			if( *term ) {
				MessageBoxA( hwnd, "Failed to parse u32", "Error", MB_OK|MB_ICONERROR );
				return;
			}
			xdr_encode_uint32( &xdr, u32 );
		} else if( strcmp( argname,"u64" ) == 0 ) {
			u64 = strtoull( argval, &term, 0 );
			if( *term ) {
				MessageBoxA( hwnd, "Failed to parse u64", "Error", MB_OK|MB_ICONERROR );
				return;
			}
			xdr_encode_uint64( &xdr, u64 );
		} else if(strcmp( argname,"str" ) == 0) {
			xdr_encode_string( &xdr, argval );
		} else if(strcmp( argname,"opaque" ) == 0) {
			sts = base64_decode( (char *)(xdr.buf + xdr.offset + 4),xdr.count - xdr.offset - 4,argval );
			if(sts < 0) {
				MessageBoxA( hwnd, "Failed to parse base64 opaque", "Error", MB_OK|MB_ICONERROR );
				return;
			}

			xdr_encode_uint32( &xdr,sts );
			xdr.offset += sts;
			if(sts % 4) xdr.offset += 4 - (sts % 4);
		} else if(strcmp( argname,"fixed" ) == 0) {
			sts = base64_decode( (char *)(xdr.buf + xdr.offset),xdr.count - xdr.offset,argval );
			if(sts < 0) {
				MessageBoxA( hwnd, "Failed to parse base64 fixed", "Error", MB_OK|MB_ICONERROR );
				return;
			}
			if(sts % 4)  {
				MessageBoxA( hwnd, "Fixed length not a multiple of 4", "Error", MB_OK|MB_ICONERROR );
				return;
			}
			xdr.offset += sts;
		} else {
			MessageBoxA( hwnd, "Error", "Bad arg", MB_OK|MB_ICONERROR );
			return;
		}

		p = strtok( NULL, " " );
	}
	
	base64_encode( xdr.buf, xdr.offset, b64str );
	SetWindowTextA( hb64, b64str );
}

static int printed = 0;
static char *decodeerror( HWND hwnd, char *fmt, ... ) {
	char errmsg[256];
	va_list args;

	if( fmt ) {
		va_start(args,fmt);
		vsprintf( errmsg, fmt, args );
		va_end(args);
	} else {
		strcpy( errmsg, "XDR decode error" );
	}

	MessageBoxA( hwnd, errmsg, "Error", MB_OK|MB_ICONERROR );
	return NULL;
}

static char *decodevalue( HWND hwnd, struct xdr_s *xdr, char *fmt, char *outstr ) {
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
    if( sts ) return decodeerror( hwnd, "Failed to decode u32" );
    if( *fmt == 'u' ) sprintf( outstr + strlen(outstr), "%s%u", printed ? ", " : "", u32 );
    else sprintf( outstr + strlen(outstr), "%s%x", printed ? ", " : "", u32 );
    fmt++;
    printed = 1;
    break;
  case 'i':
    sts = xdr_decode_int32( xdr, &i32 );
    if( sts ) return decodeerror( hwnd, "Failed to decode i32" );
    sprintf( outstr + strlen(outstr), "%s%d", printed ? ", " : "", i32 );
    fmt++;
    printed = 1;    
    break;
  case 'U':
  case 'X':
    sts = xdr_decode_uint64( xdr, &u64 );
    if( sts ) return decodeerror( hwnd, "Failed to decode u64" );
    if( *fmt == 'U' ) sprintf( outstr + strlen(outstr), "%s%"PRIu64"", printed ? ", " : "", u64 );
    else sprintf( outstr + strlen(outstr), "%s%"PRIx64"", printed ? ", " : "", u64 );
    fmt++;
    printed = 1;    
    break;
  case 'I':
    sts = xdr_decode_int64( xdr, &i64 );
    if( sts ) return decodeerror( hwnd, "Failed to decode i64" );
    sprintf( outstr + strlen(outstr), "%s%"PRIi64"", printed ? ", " : "", i64 );
    fmt++;
    printed = 1;    
    break;
  case 's':
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) return decodeerror( hwnd, "Failed to decode string" );
    xdr->offset -= 4;
    str = malloc( u32 + 1 );
    sts = xdr_decode_string( xdr, str, u32 + 1 );
    if( sts ) return decodeerror( hwnd, "Failed to decode string" );
    sprintf( outstr + strlen(outstr), "%s\"%s\"", printed ? ", " : "", str );
    free( str );
    fmt++;
    printed = 1;    
    break;
  case 'b':
    sts = xdr_decode_boolean( xdr, &i32 );
    if( sts ) return decodeerror( hwnd, "Failed to decode boolean" );
    sprintf( outstr + strlen(outstr), "%s%s", printed ? ", " : "", i32 ? "True" : "False" );
    fmt++;
    printed = 1;    
    break;
  case 'o':
    sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&bufp, &lenp );
    if( sts ) return decodeerror( hwnd, "Failed to decode opaque" );
    str = malloc( (4 * lenp) / 3 + 5 );
    base64_encode( bufp, lenp, str );
    sprintf( outstr + strlen(outstr), "%s%s", printed ? ", " : "", str );
    free( str );
    fmt++;
    printed = 1;    
    break;
  case 'f':
    /* f(nnn) : nnn must parse as an int */
    fmt++;
    if( *fmt != '(' ) return decodeerror( hwnd, "Bad format: expect ( after f" );
    fmt++;
    u32 = strtoul( fmt, &str, 0 );
    if( *str != ')' ) return decodeerror( hwnd, "Bad format: expect ) after fixed num" );
    fmt = str + 1;
    bufp = malloc( u32 );
    sts = xdr_decode_fixed( xdr, (uint8_t *)bufp, u32 );
    if( sts ) return decodeerror( hwnd, "Failed to decode fixed" );
    str = malloc( 4*(u32 / 3) + 5 );
    base64_encode( bufp, u32, str );
    sprintf( outstr + strlen(outstr), "%s%s", printed ? ", " : "", str );
    free( str );
    free( bufp );
    printed = 1;    
    break;
  case 'A':
    /* A(...) */
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) return decodeerror( hwnd, "Failed to decode fixed" );
    fmt++;
    if( *fmt != '(' ) return decodeerror( hwnd, "Bad format: expect ( after A" );
    fmt++;
    p = fmt;
    sprintf( outstr + strlen(outstr), "%s[", printed ? ", " : "" );
    printed = 0;    
    for( i = 0; i < u32; i++ ) {
      fmt = p;
      fmt = decodevalue( hwnd, xdr, fmt, outstr );
	  if( !fmt ) return NULL;
      if( *fmt != ')' ) return decodeerror( hwnd, "Bad format: expect ) after A" );      
    }
    sprintf( outstr + strlen(outstr), "]" );
    printed = 1;    
    fmt++;
    break;
  case 'L':
    /* L(...) */
    fmt++;
    if( *fmt != '(' ) return decodeerror( hwnd, "Bad format: expect ( after L" );
    fmt++;
    p = fmt;
    sprintf( outstr + strlen(outstr), "%s[", printed ? ", " : "" );
    printed = 0;    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) return decodeerror( hwnd, "Failed to decode list" );
    i = 0;
    while( b ) {
      fmt = p;
      fmt = decodevalue( hwnd, xdr, fmt, outstr );
	  if( !fmt ) return NULL;
      if( *fmt != ')' ) return decodeerror( hwnd, "Bad format: expect ) after L" );
      sts = xdr_decode_boolean( xdr, &b );
      if( sts ) return decodeerror( hwnd, "Failed to decode list" );
      i++;
    }
    printed = 1;    
    sprintf( outstr + strlen(outstr), "]" );
    fmt++;    
    break;
  case 'O':
    /* O(...) */
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) return decodeerror( hwnd, "Failed to decode optional" );
    fmt++;    
    if( *fmt != '(' ) return decodeerror( hwnd, "Bad format: expect ( after O" );
    fmt++;
    if( b ) {
      fmt = decodevalue( hwnd, xdr, fmt, outstr );
	  if( !fmt ) return NULL;
      if( *fmt != ')' ) return decodeerror( hwnd, "Bad format: expect ) after O value" );
    } else {
      sprintf( outstr + strlen(outstr), "%s{}", printed ? ", " : "" );
      u32 = 1;
      while( *fmt ) {
	if( *fmt == ')' ) u32--;
	else if( *fmt == '(' ) u32++;
	
	if( !u32 ) break;
	fmt++;
      }
      if( !fmt ) return decodeerror( hwnd, "Bad format: expect ) after O" );      
    }
    fmt++;
    break;
  case 'V':
    /* V(int:...;int:...;...) */
    fmt++;
    if( *fmt !=  '(' ) return decodeerror( hwnd, "Bad format: expect ( after U" );
    fmt++;
    sts = xdr_decode_int32( xdr, &i32 );
    if( sts ) return decodeerror( hwnd, "Failed to decode union" );

    sprintf( outstr + strlen(outstr), "%s{ %d: ", printed ? ", " : "", i32 );
    printed = 0;
    
    while( *fmt && *fmt != ')' ) {
      if( *fmt == 't' ) {
	/* default final case */
	fmt++;
	if( *fmt != ':' ) return decodeerror( hwnd, "Bad format: expect : after t" );
	fmt++;
	fmt = decodevalue( hwnd, xdr, fmt, outstr );
	if( !fmt ) return NULL;
	if( *fmt == '\0' ) return decodeerror( hwnd, "Bad format: unexpected end of string" );
	if( *fmt != ')' ) return decodeerror( hwnd, "Bad format: expect ) after final clause" );
	fmt++;
	break;
      } else {
	char *term;
	int32_t x = strtol( fmt, &term, 0 );
	if( *term != ':' ) return decodeerror( hwnd, "Bad format: expect : after number" );
	fmt = term + 1;	
	if( x == i32 ) {
	  fmt = decodevalue( hwnd, xdr, fmt, outstr );
	  if( !fmt ) return NULL;
	  if( *fmt == '\0' ) return decodeerror( hwnd, "Bad format: unexpected end of string" );
	  else if( *fmt == ';' ) {
	    /* advance to end */
	    u32 = 1; /* paren counter - when hits zero we have hit end */
	    while( 1 ) {
	      if( *fmt == '\0' ) return decodeerror( hwnd, "Bad format: unexpected end of string" );
	      if( *fmt == '(' ) u32++;
	      if( *fmt == ')' ) u32--;
	      if( u32 == 0 ) break;
	      fmt++;
	    }
	  } else if( *fmt != ')' ) return decodeerror( hwnd, "Bad format: expect ; or ) after union clause" );
	  fmt++;
	  break;
	} else {
	  /* not a match - advance to next arm */
	  fmt++;
	  u32 = 0;
	  while( 1 ) {
	    if( *fmt == '\0' ) return decodeerror( hwnd, "Bad format: unexpected end of string" );
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

    sprintf( outstr + strlen(outstr), "%s}", printed ? " " : "" );
    printed = 1;
    break;
  case '{':
    /* { ... } is a struct */
    fmt++;
    sprintf( outstr + strlen(outstr), "{" );
    printed = 0;
    while( *fmt && (*fmt != '}') ) {
      fmt = decodevalue( hwnd, xdr, fmt, outstr );
	  if( !fmt ) return NULL;
    }
    if( !fmt ) return decodeerror( hwnd, "Bad format: unexpected end of string after {" );
    sprintf( outstr + strlen(outstr), "}" );
    printed = 1;
    fmt++;
    break;
  default:
    return decodeerror( hwnd, "Bad format character %c", *fmt );
    break;
  }

  return fmt;
}

static void xdr_command_decode( HWND hwnd, HWND hargs, HWND hb64 ) {
	struct xdr_s xdr;
	char fmtstr[256];
	char b64buf[256], b64str[512];
	int len;

	GetWindowTextA( hb64, b64str, sizeof(b64str) );
	len = base64_decode( b64buf, sizeof(b64buf), b64str );
	if( len < 0 ) {
		MessageBoxA( hwnd, "Failed to decode base64", "Error", MB_OK|MB_ICONERROR );
		return;
	}
	xdr_init( &xdr, b64buf, len );

	GetWindowTextA( hargs, fmtstr, sizeof(fmtstr) );

	strcpy( b64str, "" );
	if( decodevalue( hwnd, &xdr, fmtstr, b64str ) ) {
		SetWindowTextA( hb64, b64str );
	}

	
}


static void xdr_command( HWND hwnd, int id, int cmd ) {
	HWND hargs, hb64;

	hargs = fjui_get_hwnd( "xdr_args" );
	hb64 = fjui_get_hwnd( "xdr_b64" );

	switch( id ) {
	case CMD_ENCODE:
	xdr_command_encode( hwnd, hargs, hb64 );
	break;
	case CMD_DECODE:
	xdr_command_decode( hwnd, hargs, hb64 );
	break;
	}
}

static LRESULT CALLBACK xdr_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;
	LVCOLUMNA lvc;

	switch( msg ) {
	case WM_CREATE:
	  h = CreateWindowA( WC_STATICA, "Args:", WS_VISIBLE|WS_CHILD, 5, 5, 50, 25, hwnd, 0, NULL, 0 );
	  fjui_set_font( h );
	  h = CreateWindowA( WC_STATICA, "Base64:", WS_VISIBLE|WS_CHILD, 5, 35, 50, 25, hwnd, 0, NULL, 0 );
	  fjui_set_font( h );
	  
	  h = CreateWindowA( WC_EDITA, "args", WS_VISIBLE|WS_CHILD|WS_BORDER|ES_AUTOHSCROLL, 0,0,0,0, hwnd, 0, NULL, 0 );
	  fjui_set_font( h );
	  fjui_hwnd_register( "xdr_args", h );
	  
	  h = CreateWindowA( WC_EDITA, "base64", WS_VISIBLE|WS_CHILD|WS_BORDER|ES_AUTOHSCROLL, 0,0,0,0, hwnd, 0, NULL, 0 );
	  fjui_set_font( h );
	  fjui_hwnd_register( "xdr_b64", h );

	  h = CreateWindowA( WC_BUTTON, "Encode", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON, 0, 0,0,0, hwnd, CMD_ENCODE, NULL, 0 );
	  fjui_set_font( h );
	  fjui_hwnd_register( "xdr_encode", h );

	  h = CreateWindowA( WC_BUTTON, "Decode", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON, 0, 0,0,0, hwnd, CMD_DECODE, NULL, 0 );
	  fjui_set_font( h );
	  fjui_hwnd_register( "xdr_decode", h );
		break;	
	case WM_COMMAND:
		xdr_command( hwnd, LOWORD( wparam ), HIWORD( wparam ) );
		break;
	case WM_SIZE:
		xdr_size(hwnd, LOWORD(lparam), HIWORD(lparam) );	  
		break;
	case WM_NOTIFY:
		break;
	case WM_CTLCOLORSTATIC:
		SetBkMode((HDC)wparam, TRANSPARENT );
		return (BOOL)GetStockObject( NULL_PEN );
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}

void fjui_xdr_register( void ) {
	WNDCLASSEXW cls;

	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = xdr_cb;
	cls.hInstance = fjui_hinstance();
	cls.lpszClassName = L"FJUIXDR";
	cls.hbrBackground = GetSysColorBrush( COLOR_3DFACE );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );

	RegisterClassExW( &cls );  
}
