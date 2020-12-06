
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <stdint.h>
#include <inttypes.h>

static void usage( char *fmt, ... ) {
  va_list args;
  
  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  } else {
    printf( "Usage: fvmc [-o output] [-I includepath] filename\n");
  }
   
  exit( 1 );  
}

static void compile_file( char *path );
static void addincludepath( char *path );

int main( int argc, char **argv ) {
  int i;
  char *outpath = NULL;
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-o" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      outpath = argv[i];
    } else if( strcmp( argv[i], "-h" ) == 0 ) {
      usage( NULL );
    } else if( strcmp( argv[i], "-I" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      addincludepath( argv[i] );
    } else {
      compile_file( argv[i] );
      break;
    }
    i++;
  }

  return 0;
}

/* ----------- lexing ------------------- */

  
typedef enum {
    TOK_U32,       /* [-][0-9] | 0x[0-9A-Fa-f] */
    TOK_U64,       /* [-][0-9] | 0x[0-9A-Fa-f]L */
    TOK_STRING,    /* "..." */
    TOK_NAME,      /* [a-zA-Z0-9_] */
    TOK_SEMICOLON, /* ; */
    TOK_COLON,     /* : */
    TOK_COMMA,     /* , */
    TOK_PERIOD,    /* . */
    TOK_OPAREN,    /* ( */
    TOK_CPAREN,    /* ) */
    TOK_OARRAY,    /* [ */
    TOK_CARRAY,    /* ] */
    TOK_PLUS,      /* + */
    TOK_MINUS,     /* - */
    TOK_MUL,       /* * */
    TOK_DIV,       /* / */
    TOK_MOD,       /* % */
    TOK_EQ,        /* = */
    TOK_NEQ,       /* <> */
    TOK_GT,        /* > */
    TOK_GTE,       /* >= */
    TOK_LT,        /* < */
    TOK_LTE,       /* <= */
    TOK_AND,       /* & */
    TOK_OR,        /* | */
    TOK_XOR,       /* ^ */
    TOK_NOT,       /* ! */
    TOK_TILDE,     /* ~ */
    TOK_ANDAND,    /* && */
    TOK_OROR,      /* || */
} tok_t;

static struct {
  tok_t type;
  char *name;
} toknames[] = { { TOK_U32, "u32" }, { TOK_U64, "U64" }, { TOK_STRING, "String" }, { TOK_NAME, "Name" }, {TOK_SEMICOLON, "Semicolon" },
		 { TOK_COLON, "colon" }, { TOK_COMMA, "comma" }, { TOK_PERIOD, "period" }, { TOK_OPAREN, "oparen" }, { TOK_CPAREN, "cparen" },
		 { TOK_OARRAY, "oarray" }, { TOK_CARRAY, "carray" }, { TOK_PLUS, "plus" }, { TOK_MINUS, "minus" }, { TOK_MUL, "mul" },
		 { TOK_DIV, "div" }, { TOK_MOD, "mod" }, { TOK_EQ, "eq" }, { TOK_NEQ, "neq" }, { TOK_GT, "gt" }, { TOK_GTE, "gte" },
		 { TOK_LT, "lt" }, { TOK_LTE, "lte" }, { TOK_AND, "and" }, { TOK_OR, "or" }, { TOK_NOT, "not" }, { TOK_TILDE, "tilde" }, { TOK_ANDAND, "andand" },
		 { TOK_OROR, "oror" }, { 0, NULL } };
static char *gettokname( tok_t type ) {
  int i;
  for( i = 0; toknames[i].name; i++ ) {
    if( toknames[i].type == type ) return toknames[i].name;
  }
  return "Unknown";
}

static void skipwhitespace( FILE *f ) {
  static int whitespacechars[] = { ' ', '\n', '\r', '\t', '\0' };
  int c;
  int *w, found;
  
  do {
    c = fgetc( f );
    if( c == EOF ) return;

    if( c == '{' ) {
      /* skip comment */
      do {
	c = fgetc( f );
	if( c == EOF ) break;
	if( c == '}' ) break;
      } while( 1 );
      found = 1;
    } else {    
      found = 0;
      for( w = whitespacechars; *w; w++ ) {
	if( c == *w ) {
	  found = 1;
	  break;
	}
      }
      if( !found ) ungetc( c, f );
    }
    
  } while( found );
}

struct token {
  char *val;
  int len;
  tok_t type;
  uint32_t u32;
  uint64_t u64;
};

static int nexttok( FILE *f, struct token *tok ) {
  int c, c2, i;
  char *p;
  
  skipwhitespace( f );
  
  c = fgetc( f );
  if( c == EOF ) return -1;

  if( c == '"' ) {
    /* read until matching " */
    tok->val = malloc( 32 );
    tok->len = 32;
    
    p = tok->val;
    i = 0;
    while( 1 ) {
      if( i >= tok->len - 1) {
	tok->len = (3 * tok->len) / 2;
	tok->val = realloc( tok->val, tok->len );
      }
      
      c = fgetc( f );
      if( c == '"' ) break;
      if( c == '\\' ) {
	/* escape character */
	c = fgetc( f );
	if( c == '\\' ) {
	  *p = '\\';
	  p++;
	  i++;
	} else if( c == 'a' ) {
	  *p = '\a';
	  p++;
	  i++;
	} else if( c == 'b' ) {
	  *p = '\b';
	  p++;
	  i++;
	} else if( c == 'f' ) {
	  *p = '\f';
	  p++;
	  i++;
	} else if( c == 'n' ) {
	  *p = '\n';
	  p++;
	  i++;
	} else if( c == 'r' ) {
	  *p = '\r';
	  p++;
	  i++;
	} else if( c == 't' ) {
	  *p = '\t';
	  p++;
	  i++;
	} else if( c == 'v' ) {
	  *p = '\v';
	  p++;
	  i++;
	} else if( c == 'o' ) {
	  /* octal char */
	  uint8_t x = 0;
	  c = fgetc( f );
	  if( c < '0' || c > '7' ) usage( "Bad octal char %c", c );
	  x = c - '0';
	  
	  c = fgetc( f );
	  if( c < '0' || c > '7' ) usage( "Bad octal char %c", c );	  
	  x = (x << 3) | (c - '0');

	  c = fgetc( f );
	  if( c < '0' || c > '7' ) usage( "Bad octal char %c", c );	  	
	  x = (x << 3) | (c - '0');
	  *p = x;
	  p++;
	  i++;
	} else if( c == 'x' ) {
	  /* parse hex char */
	  uint8_t x;
	  c = fgetc( f );
	  if( c >= '0' && c <= '9' ) x = c - '0';
	  else if( c >= 'a' && c <= 'f' ) x = 10 + c - 'a';
	  else if( c >= 'A' && c <= 'F' ) x = 10 - c - 'A';
	  else usage( "Bad hex char %c", c );
	  
	  c = fgetc( f );
	  if( c >= '0' && c <= '9' ) x = (x << 4) | (c - '0');
	  else if( c >= 'a' && c <= 'f' ) x = (x << 4) | (10 + c - 'a');
	  else if( c >= 'A' && c <= 'F' ) x = (x << 4) | (10 + c - 'A');
	  else usage( "Bad hex char %c", c );
	  *p = x;
	  p++;
	  i++;
	} else {
	  usage( "Bad escape character %c", c );
	}
      } else {
	*p = c;
	p++;
	i++;
      }
      
    }

    *p = '\0';
    tok->type = TOK_STRING;
  } else if( c == '&' ) {
    /* check for && */
    c2 = fgetc( f );
    if( c2 == '&' ) {
      tok->type = TOK_ANDAND;
    } else {
      ungetc( c2, f );
      tok->type = TOK_AND;
    }
  } else if( c == '|' ) {
    /* check for || */
    c2 = fgetc( f );
    if( c2 == '|' ) {
      tok->type = TOK_OROR;
    } else {
      tok->type = TOK_OR;
      ungetc( c2, f );
    }
  } else if( c == '=' ) {
    tok->type = TOK_EQ;    
  } else if( c == '<' ) {
    /* check for <= or <> */
    c2 = fgetc( f );
    if( c2 == '=' ) {
      tok->type = TOK_LTE;
    } else if( c2 == '>' ) {
      tok->type = TOK_NEQ;
    } else {
      tok->type = TOK_LT;
      ungetc( c2, f );
    }
  } else if( c == '>' ) {
    /* check for >= */
    c2 = fgetc( f );
    if( c2 == '=' ) {
      tok->type = TOK_GTE;
    } else {
      tok->type = TOK_GT;
      ungetc( c2, f );
    }
  } else if( c == '+' ) {
    tok->type = TOK_PLUS;
  } else if( c == '-' ) {
    c2 = fgetc( f );
    if( c2 >= '0' && c2 <= '9' ) {
      /* parse negative integer */
      char nstr[32];
      char *p;
      p = nstr;
      *p = '-';
      p++;
      *p = c2;
      p++;

      do {
	c2 = fgetc( f );
	if( c2 >= '0' && c <= '9' ) {
	  *p = c2;
	  p++;
	} else {
	  *p = '\0';
	  ungetc( c2, f );
	  break;
	}
      } while( 1 );

      tok->u32 = strtol( nstr, NULL, 0 );
      tok->type = TOK_U32;
    } else {
      tok->type = TOK_MINUS;
      ungetc( c2, f );
    }
  } else if( c == '*' ) {
    tok->type = TOK_MUL;
  } else if( c == '/' ) {
    tok->type = TOK_DIV;
  } else if( c == '%' ) {
    tok->type = TOK_MUL;
  } else if( c == '~' ) {
    tok->type = TOK_TILDE;
  } else if( c == ';' ) {
    tok->type = TOK_SEMICOLON;
  } else if( c == ':' ) {
    tok->type = TOK_COLON;
  } else if( c == ',' ) {
    tok->type = TOK_COMMA;
  } else if( c == '(' ) {
    tok->type = TOK_OPAREN;
  } else if( c == ')' ) {
    tok->type = TOK_CPAREN;
  } else if( c == '[' ) {
    tok->type = TOK_OARRAY;
  } else if( c == ']' ) {
    tok->type = TOK_CARRAY;
  } else if( c == '!' ) {
    tok->type = TOK_NOT;
  } else if( c >= '0' && c <= '9' ) {
    /* parse as integer */
    c2 = fgetc( f );
    if( c == '0' && c2 == 'x' ) {
      /* parse as hex */
      char nstr[32];
      char *p;
      p = nstr;
      while( 1 ) {
	c = fgetc( f );
	if( (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') ) {
	  *p = c;
	  p++;
	} else {
	  ungetc( c, f );
	  break;
	}
      }
      *p = '\0';
      tok->u32 = strtoul( nstr, NULL, 16 );
      tok->type = TOK_U32;
    } else {
      /* parse as decimal */
      char nstr[32];
      char *p;
      p = nstr;
      *p = c;
      p++;
      if( c2 >= '0' && c2 <= '9' ) {
	*p = c2;
	p++;
	while( 1 ) {
	  c = fgetc( f );
	  if( (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') ) {
	    *p = c;
	    p++;
	  } else {
	    ungetc( c, f );
	    break;
	  }
	}
	*p = '\0';
	tok->u32 = strtol( nstr, NULL, 10 );
	tok->type = TOK_U32;
      } else {
	ungetc( c2, f );
	*p = '\0';
	tok->type = TOK_U32;
	tok->u32 = strtol( nstr, NULL, 0 );
      }
    }
  } else {
    /* read until terminal */
    tok->val = malloc( 32 );
    tok->len = 32;
    p = tok->val;
    *p = c;
    p++;
    i = 1;
    while( 1 ) {
      if( i >= tok->len - 1 ) {
	tok->len = (3 * tok->len) / 2;
	tok->val = realloc( tok->val, tok->len );
      }
      
      c = fgetc( f );
      if( (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_') ) {
	/* continue reading */
	*p = c;
	p++;
	i++;
      } else {
	ungetc( c, f );
	break;
      }
    }
    *p = '\0';
    tok->type = TOK_NAME;
  }

  return 0;
}

/* ---------- file parsing -------------------------- */

struct includepath {
  struct includepath *next;
  char *path;
};

static struct {
  int pass;
  struct includepath *includepaths;
} glob;

static void addincludepath( char *path ) {
  struct includepath *p;
  p = malloc( sizeof(*p) );
  p->next = glob.includepaths;
  p->path = path;
  glob.includepaths = p;
}

static void parsefile( char *path ) {
  FILE *f;
  struct includepath *p;
  char ppath[256];
  int sts;
  struct token tok;
  
  f = fopen( path, "r" );
  if( !f ) {
    p = glob.includepaths;
    while( p ) {
#ifdef WIN32
      sprintf( ppath, "%s\\%s", p->path, path );
#else
      sprintf( ppath, "%s/%s", p->path, path );
#endif
      f = fopen( ppath, "r" );
      if( f ) break;
      p = p->next;
    }
    if( !f ) usage( "No file \"%s\"", path );
  }

  while( 1 ) {
    memset( &tok, 0, sizeof(tok) );
    sts = nexttok( f, &tok );
    if( sts ) break;

    switch( tok.type ) {
    case TOK_U32:
      printf( "U32: %u\n", tok.u32 );
      break;
    case TOK_STRING:
      printf( "STRING: %s\n", tok.val );
      break;
    case TOK_NAME:
      printf( "NAME: %s\n", tok.val );
      break;
    default:
      printf( "%s\n", gettokname( tok.type ) );
      break;
    }
    if( tok.val ) free( tok.val );
  }
  
  
  fclose( f );
}

static void compile_file( char *path ) {
  parsefile( path );
  glob.pass = 1;
  parsefile( path );
}
