
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>

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

static void skipwhitespace( FILE *f ) {
  static int whitespaceshars[] = { ' ', '\n', '\r', '\t', '\0' };
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
	if( c == w ) {
	  found = 1;
	  break;
	}
      }
      if( !found ) ungetc( f, c );
    }
    
  } while( found );
}

static int nexttok( FILE *f, struct token *tok ) {
  skipwhitespace( f );
  
  c = fgetc( f );
  if( c == EOF ) return -1;

  if( c == '"' ) {
    /* read until matching " */
    tok->val = malloc( 32 );
    tok->len = 32;
    
    p = tok->val;
    *p = c;
    p++;
    i = 1;
    while( 1 ) {
      c = fgetc( f );
      if( c == '"' ) break;
      if( c == '\\' ) {
	/* escape character */
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
      ungetc( f, c2 );
      tok->type = TOK_AND;
    }
  } else if( c == '|' ) {
    /* check for || */
    c2 = fgetc( f );
    if( c2 == '|' ) {
      tok->type = TOK_OROR;
    } else {
      tok->type = TOK_OR;
      ungetc( f, c2 );
    }
  } else if( c == '<' ) {
    /* check for <= or <> */
    c2 = fgetc( f );
    if( c2 == '=' ) {
      tok->type = TOK_LTE;
    } else if( c2 == '>' ) {
      tok->type = TOK_NEQ;
    } else {
      tok->type = TOK_LT;
      ungetc( f, c2 );
    }
  } else if( c == '>' ) {
    /* check for >= */
    c2 = fgetc( f );
    if( c2 == '=' ) {
      tok->type = TOK_GTE;
    } else {
      tok->type = TOK_GT;
      ungetc( f, c2 );
    }
  } else if( c == '+' ) {
    tok->type = TOK_PLUS;
  } else if( c == '-' ) {
    c2 = fgetc( f );
    if( c2 >= '0' && c2 <= '9' ) {
      /* parse negative integer */
      /* TODO */
    } else {
      tok->type = TOK_MINUS;
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
    if( c2 == 'x' ) {
      /* parse as hex */
    } else {
    }
  } else {
    /* read until terminal */
    tok->val = malloc( 32 );
    p = tok->val;
    *p = c;
    p++;
    while( 1 ) {
      c = fgetc( f );
      if( (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_') ) {
	/* continue reading */
      } else {
	ungetc( f, c );
	break;
      }
    }

    tok->type = TOK_NAME;
  }
  
}

/* ---------- file parsing -------------------------- */

struct includepath {
  struct inclduepath *next;
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
  glob.includeepaths = p;
}

static void parsefile( char *path ) {
  FILE *f;
  struct includepath *p;
  char ppath[256];
  
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

  
  
  fclose( f );
}

static void compile_file( *path ) {
  parsefile( path );
  glob.pass = 1;
  parsefile( path );
}
