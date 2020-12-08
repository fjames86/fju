
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <stdint.h>
#include <inttypes.h>

#include "fvmc2.h"

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

static void compile_file( char *path, char *outpath );
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
      compile_file( argv[i], outpath ? outpath : "out.fvm" );
      break;
    }
    i++;
  }

  return 0;
}

/* ----------- lexing ------------------- */

  
typedef enum {
    TOK_U32,       /* [-][0-9] | 0x[0-9A-Fa-f] */
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
    TOK_SHR,       /* >> */
    TOK_SHL,       /* << */
} tok_t;

static struct {
  tok_t type;
  char *name;
} toknames[] = { { TOK_U32, "u32" }, { TOK_STRING, "String" }, { TOK_NAME, "Name" }, {TOK_SEMICOLON, "Semicolon" },
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
    } else if( c == '#' ) {
      /* skip single line comment */
      do {
	c = fgetc( f );
	if( c == '\n' ) break;
      } while( 1 );	  
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
};

static int getnexttok( FILE *f, struct token *tok ) {
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
	} else if( c == '"' ) {
	  *p = '"';
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
    } else if( c2 == '<' ) {
      tok->type = TOK_SHL;
    } else {
      tok->type = TOK_LT;
      ungetc( c2, f );
    }
  } else if( c == '>' ) {
    /* check for >= */
    c2 = fgetc( f );
    if( c2 == '=' ) {
      tok->type = TOK_GTE;
    } else if( c2 == '>' ) {
      tok->type = TOK_SHR;
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
  } else if( c == '.' ) {
    tok->type = TOK_PERIOD;
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
      if( (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_') || (c == ':') ) {
	/* continue reading */
	*p = c;
	p++;
	i++;

	/* allow final character to be a colon to cope with label identifiers */
	if( c == ':' ) break;
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

static void emitdata( void *data, int len );
static struct var *getglobal( char *name );
static struct label *getlabel( char *prefix );
static struct proc *getproc( char *name );
static struct param *getparam( struct proc *proc, char *name );
static struct var *getlocal( struct proc *proc, char *name );
static struct constvar *getconst( char *name );
static struct constval *getconstval( char *name );
static int getvarbyname( char *name, struct var **localvar, struct var **globalvar, struct param **param, struct constvar **constvar, struct constval **constval );

#define MAXPROC 32 
#define MAXPARAM 8
#define MAXNAME 64

struct includepath {
  struct includepath *next;
  char *path;
};

/* parsed information */

/* global or local variable */
struct var {
  struct var *next;
  char name[MAXNAME];
  
  var_t type;
  uint32_t arraylen;
  uint32_t address; /* address if global, stack offset if local */
  uint32_t offset;
  uint32_t size; /* size of the variable. u32 is 4 */
};

/* procedure parameter */
struct param {
  char name[MAXNAME];
  var_t type;
  int isvar;
  uint32_t size;    /* size is 4 for everything */
  uint32_t offset;  /* stack offset */
};

struct proc {
  struct proc *next;
  char name[MAXNAME];
  uint32_t address;
  struct param params[MAXPARAM];
  int nparams;
  struct var *locals;
  uint32_t localsize; /* total size of all locals */
  uint32_t siginfo; /* signature */
};

struct label {
  struct label *next;
  char name[MAXNAME];
  uint32_t address;
};

struct export {
  struct export *next;
  char name[MAXNAME];
};

struct constvar {
  struct constvar *next;
  char name[MAXNAME];
  var_t type;
  uint32_t address;
  char *val;
  int len;
};

struct constval {
  struct constval *next;
  char name[MAXNAME];
  var_t type;
  char *val;
  int len;
};

/*
 * stack: 
 *  - parameter 7    ;; parameters pushed from right to left 
 *  - parameter 6    ;;
 *  - ...
 *  - parameter 0    ;; offset 4
 *  - return address       
 *  - local var 1 
 *  - local var 2 
 *  - ... 
 *  - local var n 
 */


static struct {
  int pass;
  struct includepath *includepaths;

  char progname[MAXNAME];
  uint32_t progid, versid;

  uint32_t labelidx;
  struct label *labels;
  struct var *globals;
  struct proc *procs;
  struct export *exports;
  struct constvar *consts;
  struct constval *constvals;
  
  struct token tok;
  uint32_t pc;
  uint32_t datasize;
  uint32_t textsize;

  FILE *outfile;
  struct proc *currentproc;
  uint32_t stackoffset;  /* offset relative to last local var. */
} glob;

static void getlabelname( char *prefix, char *lname ) {
  sprintf( lname, "%s-%u", prefix ? prefix : "L", glob.labelidx );
  glob.labelidx++;
}

static struct label *getlabel( char *lname ) {
  struct label *l;
  l = glob.labels;
  while( l ) {
    if( strcasecmp( l->name, lname ) == 0 ) return l;
    l = l->next;
  }
  return NULL;
}
static struct label *addlabel( char *lname ) {
  struct label *l;

  if( glob.pass == 2 ) {
    l = getlabel( lname );
    printf( ";; PC=%04x SP=%04u %s:\n", l ? l->address : 0, glob.stackoffset, lname );
    return l;
  }

  l = getlabel( lname );
  if( l ) usage( "Label name %s already exists", lname );

  l = malloc( sizeof(*l) );
  strcpy( l->name, lname );
  l->address = glob.pc;
  l->next = glob.labels;
  glob.labels = l;
  return l;
}

static struct var *getglobal( char *name ) {
  struct var *v;
  v = glob.globals;
  while( v ) {
    if( strcasecmp( v->name, name ) == 0 ) return v;
    v = v->next;
  }
  return NULL;
}
static struct var *addglobal( char *name, var_t type, uint32_t arraylen ) {
  struct var *v;
  
  if( glob.pass == 2 ) return getglobal( name );

  v = getglobal( name );
  if( v ) usage( "Global %s already exists", name );
    
  v = malloc( sizeof(*v) );
  strcpy( v->name, name );
  v->type = type;
  v->arraylen = arraylen;
  v->address = glob.globals ? glob.globals->address + glob.globals->size : 0;
  v->size = 4;
  if( arraylen ) {
    if( type == VAR_TYPE_STRING || type == VAR_TYPE_OPAQUE ) {
      v->size = arraylen;
      if( arraylen % 4 ) v->size += 4 - (arraylen % 4);
    } else {
      v->size *= arraylen;
    }
  }
  v->next = glob.globals;
  glob.globals = v;

  printf( ";; Adding global %s type=%u arraylen=%u address=%u\n", name, type, arraylen, v->address );
  
  return v;
}
static struct proc *getproc( char *name ) {
  struct proc *p;
  p = glob.procs;
  while( p ) {
    if( strcasecmp( p->name, name ) == 0 ) return p;
    p = p->next;
  }
  return NULL;
}
static struct proc *addproc( char *name, struct param *params, int nparams, uint32_t siginfo ) {
  struct proc *p;
  
  if( glob.pass == 2 ) usage( "assert" );

  printf( ";; Add procedure %s\n", name );
  
  p = getproc( name );
  if( p ) usage( "Proc %s already exists", name );

  p = malloc( sizeof(*p) );
  memset( p, 0, sizeof(*p) );
  strcpy( p->name, name );
  p->address = glob.pc;
  memcpy( p->params, params, sizeof(*params) * nparams );
  p->nparams = nparams;
  p->siginfo = siginfo;
  
  p->next = glob.procs;
  glob.procs = p;
  return p;
}
static struct param *getparam( struct proc *proc, char *name ) {
  int i;
  for( i = 0; i < proc->nparams; i++ ) {
    if( strcasecmp( proc->params[i].name, name ) == 0 ) return &proc->params[i];
  }
  return NULL;
}

static struct var *getlocal( struct proc *proc, char *name ) {
  struct var *v;
  v = proc->locals;
  while( v ) {
    if( strcasecmp( v->name, name ) == 0 ) return v;
    v = v->next;
  }
  return NULL;
}

static struct var *addlocal( struct proc *proc, char *name, var_t type, uint32_t arraylen ) {
  struct var *v, **vp, *v2;
  int i;

  if( glob.pass == 2 ) return getlocal( proc, name );
  
  v = getglobal( name );
  if( v ) printf( ";; Warning: local variable %s shadows existing global\n", name );
  
  v = getlocal( proc, name );
  if( v ) usage( "Local variable with name %s already exists", name );

  printf( ";; add local %s type=%u arraylen=%u\n", name, type, arraylen );
  
  vp = NULL;
  if( proc->locals ) {
    v = proc->locals;
    while( v ) {
      if( v->next == NULL ) {
	vp = &v->next;
	break;
      }
      v = v->next;
    }
  } else {
    vp = &proc->locals;
  }
  if( !vp ) return NULL;
  
  /* every time we add a new local we must push the others back in the stack frame i.e. increase their stack offset. this includes params */
  
  v = malloc( sizeof(*v) );
  strcpy( v->name, name );
  v->type = type;
  v->arraylen = arraylen;
  v->size = 4;
  if( arraylen ) {
    if( type == VAR_TYPE_STRING || type == VAR_TYPE_OPAQUE ) {
      v->size = arraylen;
      if( arraylen % 4 ) v->size += 4 - (arraylen % 4);
    } else {
      v->size *= arraylen;
    }    
  }

  /* this local is the shallowest variable on stack so can be accessed at depth of its size */
  v->offset = v->size;

  /* push all other params + locals further back in the stack */
#if 0
  for( i = 0; i < proc->nparams; i++ ) {
    proc->params[i].offset += v->size;
  }
#endif
  v2 = proc->locals;
  while( v2 ) {
    v2->offset += v->size;
    v2 = v2->next;
  }
  
  /* put on end */
  v->next = NULL;
  *vp = v;
  proc->localsize += v->size;
  
  return v;
}

static void addexport( char *name ) {
  struct export *e, **ep;

  if( glob.pass == 2 ) return;

  e = glob.exports;
  while( e ) {
    if( strcasecmp( e->name, name ) == 0 ) usage( "Export %s already exists", name );;
    e = e->next;    
  }

  printf( ";; Adding export %s\n", name );
    
  ep = NULL;
  if( glob.exports ) {
    e = glob.exports;
    while( e ) {
      if( !e->next ) {
	ep = &e->next;
	break;
      }
      e = e->next;
    }
  } else {
    ep = &glob.exports;
  }
  if( !ep ) usage( "cannot happen" );
  
  e = malloc( sizeof(*e) );
  memset( e, 0, sizeof(*e) );
  strncpy( e->name, name, sizeof(e->name) - 1 );
  *ep = e;
}

static struct constvar *getconst( char *name ) {
  struct constvar *v;
  v = glob.consts;
  while( v ) {
    if( strcasecmp( v->name, name ) == 0 ) return v;
    v = v->next;
  }
  return NULL;
}

static struct constvar *addconst( char *name, var_t type, char *val, int len ) {
  struct constvar *v;

  if( glob.pass == 2 ) {
    emitdata( val, len );
    return getconst( name );
  }
  
  v = getconst( name );
  if( v ) usage( "Const name %s already exists", name );

  printf( ";; Adding const %s\n", name );
  v = malloc( sizeof(*v) );
  strncpy( v->name, name, MAXNAME - 1 );
  v->type = type;
  v->val = val;
  v->len = len;
  v->address = glob.pc;
  v->next = glob.consts;
  glob.consts = v;

  glob.pc += len;
  
  return v;
}

static struct constval *getconstval( char *name ) {
  struct constval *v;
  v = glob.constvals;
  while( v ) {
    if( strcasecmp( v->name, name ) == 0 ) return v;
    v = v->next;
  }
  return NULL;
}

static struct constval *addconstval( char *name, var_t type, char *val, int len ) {
  struct constval *v;

  if( glob.pass == 2 ) return getconstval( name );
  
  v = getconstval( name );
  if( v ) usage( "Const name %s already exists", name );

  v = malloc( sizeof(*v) );
  strncpy( v->name, name, MAXNAME - 1 );
  v->type = type;
  v->val = val;
  v->len = len;
  v->next = glob.constvals;
  glob.constvals = v;

  return v;
}


static int getvarbyname( char *name, struct var **localvar, struct var **globalvar, struct param **param, struct constvar **constvar, struct constval **constval ) {
  struct var *v;
  struct param *p;
  struct constvar *cv;
  struct constval *cl;
  
  if( localvar ) *localvar = NULL;
  if( globalvar ) *globalvar = NULL;
  if( param ) *param = NULL;
  if( constvar ) *constvar = NULL;
  if( constval ) *constval = NULL;
  
  v = getlocal( glob.currentproc, name );
  if( v ) {
    if( localvar ) *localvar = v;
    return 1;
  }

  v = getglobal( name );
  if( v ) {
    if( globalvar ) *globalvar = v;
    return 1;
  }

  p = getparam( glob.currentproc, name );
  if( p ) {
    if( param ) *param = p;
    return 1;
  }

  cv = getconst( name );
  if( cv ) {
    if( constvar ) *constvar = cv;
    return 1;
  }

  cl = getconstval( name );
  if( cl ) {
    if( constval ) *constval = cl;
    return 1;
  }

  return 0;
}



static void addincludepath( char *path ) {
  struct includepath *p;
  p = malloc( sizeof(*p) );
  p->next = glob.includepaths;
  p->path = path;
  glob.includepaths = p;
}

static struct token *nexttok( FILE *f ) {
  int sts;
  
  if( glob.tok.val ) free( glob.tok.val );

  memset( &glob.tok, 0, sizeof(glob.tok) );
  sts = getnexttok( f, &glob.tok );
  if( sts ) return NULL;
  return &glob.tok;
}

static int accepttok( FILE *f, tok_t type ) {
  struct token *tok;
  if( glob.tok.type == type ) {
    tok = nexttok( f );
    if( !tok ) {
      glob.tok.type = TOK_PERIOD;
    }
    return 1;
  }
  return 0;
}

static void expecttok( FILE *f, tok_t type ) {
  if( accepttok( f, type ) ) return;
  usage( "Unexpected symbol %s (%s) - expected %s", gettokname( glob.tok.type ), glob.tok.val ? glob.tok.val : "", gettokname( type ) );
}

static int acceptkeyword( FILE *f, char *name ) {
  struct token *tok;
  if( glob.tok.type == TOK_NAME && (strcasecmp( glob.tok.val, name ) == 0) ) {
    tok = nexttok( f );
    if( !tok ) printf( ";; Unexpected end of file\n" );
    return 1;
  }
  return 0;
}

static void expectkeyword( FILE *f, char *name ) {
  if( !acceptkeyword( f, name ) ) usage( "Unexpected symbol %s - expected %s", glob.tok.val ? glob.tok.val : gettokname( glob.tok.type ), name );
}

/* ---------------- */

struct opinfo {  
  op_t op;
  char *name;
  uint32_t pcdata;
  int32_t stackadjust;
};

static struct opinfo opcodeinfo[] =
  {
   { OP_NOP, "NOP", 0, 0 },
   { OP_LDI32, "LDI32", 4, 4 },
   { OP_LEA, "LEA", 2, 4 },
   { OP_ADDSP, "ADDSP", 2, 0 }, /* opcode adjusts sp directly */
   { OP_SUBSP, "SUBSP", 2, 0 }, /* ditto */
   { OP_CALL, "CALL", 2, 0 }, /* ( -- retaddr ) */
   { OP_RET, "RET", 0, 0 }, /* ( retaddr -- ) */
   { OP_LEASP, "LEASP", 2, 4 }, /* ( -- address ) */
   { OP_LDSP, "LDSP", 2, 4 }, /* ( -- value ) */
   { OP_STSP, "STSP", 2, -4 }, /* (value -- )*/
   { OP_BR, "BR", 2, -4 },  /* (test --) */
   { OP_EQ, "EQ", 0, -4 }, /* (a b -- test) */
   { OP_NEQ, "NEQ", 0, -4 },
   { OP_GT, "GT", 0, -4 },
   { OP_GTE, "GTE", 0, -4 },
   { OP_LT, "LT", 0, -4 },
   { OP_LTE, "LTE", 0, -4 },
   { OP_JMP, "JMP", 2, 0 },
   { OP_ADD, "ADD", 0, -4 },
   { OP_SUB, "SUB", 0, -4 },
   { OP_MUL, "MUL", 0, -4 },
   { OP_DIV, "DIV", 0, -4 },      
   { OP_MOD, "MOD", 0, -4 },      
   { OP_AND, "AND", 0, -4 },
   { OP_OR, "OR", 0, -4 },
   { OP_XOR, "XOR", 0, -4 }, /* (a b -- a^b )*/
   { OP_NOT, "NOT", 0, 0 },
   { OP_SHL, "SHL", 0, -4 }, /* (value shift -- value) */
   { OP_SHR, "SHR", 0, -4 }, /* (value shift -- value) */
   { OP_LD, "LD", 0, 0 },  /* ( address -- value ) */
   { OP_ST, "ST", 0, -8 },  /* (address value -- ) */
   { OP_SYSCALL, "SYSCALL", 2, 0 },
   { 0, NULL, 0, 0 }
  };
static struct opinfo *getopinfo( op_t op ) {
  int i;
  for( i = 0; opcodeinfo[i].name; i++ ) {
    if( opcodeinfo[i].op == op ) return &opcodeinfo[i];
  }
  return NULL;
}

static void emitopcode( op_t op, void *data, int len ) {
  uint8_t u8;
  struct opinfo *info;

  info = getopinfo( op );
  if( !info ) usage( "Unknown opcode" );
  if( len != info->pcdata ) usage( "opcode %s data mismatch %u != %u", info->name, len, info->pcdata );
  
  if( glob.pass == 2 ) {
    if( len == 0 ) printf( ";; PC=%04x SP=%04u Emitopcode: %s\n", glob.pc, glob.stackoffset, info->name );  
    else if( len == 2 ) {
      uint16_t u16 = *((uint16_t *)data);
      printf( ";; PC=%04x SP=%04u Emitopcode: %s\t%u (%d) 0x%x\n", glob.pc, glob.stackoffset, info->name, (uint32_t)u16, (int32_t)(int16_t)u16, (uint32_t)u16 );
    } else if( len == 4 ) {
      uint32_t u32 = *((uint32_t *)data);
      printf( ";; PC=%04x SP=%04u Emitopcode: %s\t%u (%d) 0x%x\n", glob.pc, glob.stackoffset, info->name, u32, u32, u32 );
    }
  }
  
  u8 = op;
  if( glob.pass == 2 ) fwrite( &u8, 1, 1, glob.outfile );
  glob.pc += 1 + info->pcdata;
  glob.stackoffset += info->stackadjust;
  if( glob.pass == 2 ) fwrite( data, 1, info->pcdata, glob.outfile );
}


static void emit_ldi32( uint32_t val ) {
  emitopcode( OP_LDI32, &val, 4 );
}
static void emit_lea( int16_t offset ) {
  emitopcode( OP_LEA, &offset, 2 );
}
static void emit_addsp( int16_t offset ) {
  emitopcode( OP_ADDSP, &offset, 2 );
}
static void emit_subsp( int16_t offset ) {
  emitopcode( OP_SUBSP, &offset, 2 );
}
static void emit_call( uint16_t addr ) {
  emitopcode( OP_CALL, &addr, 2 );
}
static void emit_ret( void ) {
  emitopcode( OP_RET, NULL, 0 );
}
static void emit_leasp( uint16_t u ) {
  emitopcode( OP_LEASP, &u, 2 );
}
static void emit_ldsp( uint16_t u ) {
  emitopcode( OP_LDSP, &u, 2 );
}
static void emit_stsp( uint16_t u ) {
  emitopcode( OP_STSP, &u, 2 );
}
static void emit_br( uint16_t u ) {
  emitopcode( OP_BR, &u, 2 );
}
static void emit_eq( void ) {
  emitopcode( OP_EQ, NULL, 0 );
}
static void emit_neq( void ) {
  emitopcode( OP_NEQ, NULL, 0 );
}
static void emit_gt( void ) {
  emitopcode( OP_GT, NULL, 0 );
}
static void emit_gte( void ) {
  emitopcode( OP_GTE, NULL, 0 );
}
static void emit_lt( void ) {
  emitopcode( OP_LT, NULL, 0 );
}
static void emit_lte( void ) {
  emitopcode( OP_LTE, NULL, 0 );
}
static void emit_jmp( uint16_t addr ) {
  emitopcode( OP_JMP, &addr, 2 );
}
static void emit_add( void ) {
  emitopcode( OP_ADD, NULL, 0 );
}
static void emit_sub( void ) {
  emitopcode( OP_SUB, NULL, 0 );
}
static void emit_mul( void ) {
  emitopcode( OP_MUL, NULL, 0 );
}
static void emit_div( void ) {
  emitopcode( OP_DIV, NULL, 0 );
}
static void emit_mod( void ) {
  emitopcode( OP_MOD, NULL, 0 );
}
static void emit_and( void ) {
  emitopcode( OP_AND, NULL, 0 );
}
static void emit_or( void ) {
  emitopcode( OP_OR, NULL, 0 );
}
static void emit_xor( void ) {
  emitopcode( OP_XOR, NULL, 0 );
}
static void emit_not( void ) {
  emitopcode( OP_NOT, NULL, 0 );
}
static void emit_shl( void ) {
  emitopcode( OP_SHL, NULL, 0 );
}
static void emit_shr( void ) {
  emitopcode( OP_SHR, NULL, 0 );
}
static void emit_ld( void ) {
  emitopcode( OP_LD, NULL, 0 );
}
static void emit_st( void ) {
  emitopcode( OP_ST, NULL, 0 );
}
static void emit_syscall( uint16_t u ) {
  emitopcode( OP_SYSCALL, &u, 2 );
}


static void emitdata( void *data, int len ) {
  if( glob.pass == 2 ) {
    printf( ";; PC=%04x SP=%04u Const data len %x\n", glob.pc, glob.stackoffset, len );
    fwrite( data, 1, len, glob.outfile );
  }
  glob.pc += len;
}

/*
 * encode param info into a uint32: bits 0-23: 3 bits per param encoding type and isvar flag
 * bits 24-27: param count
 * bits 28-31: unused 
 */

#if 0
(defun encode-params (&rest params)
	   (let ((p 0))
	     (do ((i 0 (1+ i))
		  (pars params (cdr pars)))
		 ((or (= i 8) (null pars)))
	       (destructuring-bind (type &optional isvar) (car pars)
		 (setf p (logior p
				 (ash (logior (ecase type
						(:u32 0) (:string 1) (:opaque 2))
					      (if isvar 4 0))
				      (* 3 i))))))
	     (setf p (logior p (ash (length params) 24)))
	     p))
#endif

struct syscall {
  char *name;
  uint16_t id;
  uint32_t siginfo;
};

static struct syscall syscalls[] =
  {
   { "Log", 1, 0x02000010 }, /* Log(flags : u32, str : string); */ 
   { NULL, 0, 0 }
  };

static struct syscall *getsyscall( char *name ) {
  int i;
  for( i = 0; syscalls[i].name; i++ ) {
    if( strcasecmp( syscalls[i].name, name ) == 0 ) return &syscalls[i];
  }
  return NULL;
}
  
  

/* --------------- */

static void parsevartype( FILE *f, var_t *type, uint32_t *arraylen ) {
  if( glob.tok.type != TOK_NAME ) usage( "Unexpect symbol type %s - expected u32|string|opaque", gettokname( glob.tok.type ) );
  if( strcasecmp( glob.tok.val, "u32" ) == 0 ) *type = VAR_TYPE_U32;
  else if( strcasecmp( glob.tok.val, "string" ) == 0 ) *type = VAR_TYPE_STRING;
  else if( strcasecmp( glob.tok.val, "opaque" ) == 0 ) *type = VAR_TYPE_OPAQUE;
  expecttok( f, TOK_NAME );
  if( accepttok( f, TOK_OARRAY ) ) {
    if( glob.tok.type != TOK_U32 ) usage( "Expected array length not %s", gettokname( glob.tok.type ) );
    *arraylen = glob.tok.u32;
    expecttok( f, TOK_U32 );
    expecttok( f, TOK_CARRAY );
  } else {
    *arraylen = 0;
  }
}

static void parseexpr( FILE *f ) {
  uint16_t addr;
  tok_t optype;
  
  /*
   * exprs: 
   * varname : load value of varname 
   * u32 : constant 
   * string : constant 
   * (expr)
   * ~expr
   * !expr
   * expr [+ - * / % & ^ | >> <<] expr
   */
  if( accepttok( f, TOK_OPAREN ) ) {
    parseexpr( f );
    expecttok( f, TOK_CPAREN );
  } else if( accepttok( f, TOK_TILDE ) || accepttok( f, TOK_NOT ) ) {
    parseexpr( f );
    emit_not();
  } else if( glob.tok.type == TOK_U32 ) {
    emit_ldi32( glob.tok.u32 );
    expecttok( f, TOK_U32 );
  } else if( glob.tok.type == TOK_STRING ) {
    char lname[MAXNAME];
    struct label *l;
    uint16_t startaddr;

    getlabelname( "STRING", lname );
    if( glob.pass == 1 ) l = NULL;
    else {
      l = getlabel( lname );
      if( !l ) usage( "Failed to get label" );
    }
    addr = l ? l->address : 0;
    emit_jmp( addr );
    startaddr = glob.pc;
    emitdata( glob.tok.val, strlen( glob.tok.val ) + 1 );
    addlabel( lname );

    emit_lea( startaddr - glob.pc );

    expecttok( f, TOK_STRING );
  } else if( glob.tok.type == TOK_NAME ) {
    struct var *v;
    struct param *p;
    
    v = getlocal( glob.currentproc, glob.tok.val );
    if( v ) {
      emit_ldsp( v->offset + glob.stackoffset );
    } else {
      v = getglobal( glob.tok.val );
      if( v ) {
	emit_ldi32( v->address );
	emit_ld(); 	
      } else {
	p = getparam( glob.currentproc, glob.tok.val );
	if( !p ) return;
	emit_ldsp( p->offset + glob.currentproc->localsize + glob.stackoffset );
	if( p->isvar ) {
	  emit_ld();
	}
      }
    }
    
    expecttok( f, TOK_NAME );
  } else usage( "Bad expr %s (%s)", gettokname( glob.tok.type ), glob.tok.val ? glob.tok.val : "" );
  
  optype = glob.tok.type;
  if( glob.tok.type == TOK_SEMICOLON ) {
    /* done */ 
  } else {
    /* binary operator */
    switch( optype ) {
    case TOK_PLUS:
      expecttok( f, optype );
      parseexpr( f );      
      emit_add();
      break;
    case TOK_MINUS:
      expecttok( f, optype );
      parseexpr( f );      
      emit_sub();
      break;
    case TOK_MUL:
      expecttok( f, optype );
      parseexpr( f );            
      emit_mul();
      break;
    case TOK_DIV:
      expecttok( f, optype );
      parseexpr( f );         
      emit_div();
      break;
    case TOK_MOD:
      expecttok( f, optype );
      parseexpr( f );            
      emit_mod();
      break;
    case TOK_AND:
      expecttok( f, optype );
      parseexpr( f );            
      emit_and();
      break;
    case TOK_OR:
      expecttok( f, optype );
      parseexpr( f );            
      emit_or();
      break;
    case TOK_XOR:
      expecttok( f, optype );
      parseexpr( f );            
      emit_xor();
      break;
    case TOK_SHL:
      expecttok( f, optype );
      parseexpr( f );            
      emit_shl();
      break;
    case TOK_SHR:
      expecttok( f, optype );
      parseexpr( f );            
      emit_shr();
      break;
    case TOK_EQ:
      expecttok( f, optype );
      parseexpr( f );            
      emit_eq();
      break;
    case TOK_NEQ:
      expecttok( f, optype );
      parseexpr( f );            
      emit_neq();
      break;            
    case TOK_GT:
      expecttok( f, optype );
      parseexpr( f );            
      emit_gt();
      break;            
    case TOK_GTE:
      expecttok( f, optype );
      parseexpr( f );            
      emit_gte();
      break;            
    case TOK_LT:
      expecttok( f, optype );
      parseexpr( f );            
      emit_lt();
      break;            
    case TOK_LTE:
      expecttok( f, optype );
      parseexpr( f );            
      emit_lte();
      break;                  
    default:
      break;
    }

  }
  
}
      

static int parsestatement( FILE *f ) {
  int kw;
  kw = 0;
  if( acceptkeyword( f, "call" ) ) kw = 1;
  else if( acceptkeyword( f, "syscall" ) ) kw = 2;
  
  if( kw ) {
    uint16_t addr;
    int ipar;
    struct var *v;
    uint32_t siginfo;
    char procname[MAXNAME];
    
    /* call|syscall procname(args...) */
    if( glob.tok.type != TOK_NAME ) usage( "Expected procname" );
    strcpy( procname, glob.tok.val );
    
    if( kw == 1 ) {
      /* call - lookup proc */
      struct proc *proc;      
      proc = getproc( glob.tok.val );
      if( !proc ) usage( "Unknown proc %s", glob.tok.val );
      siginfo = proc->siginfo;
      addr = proc->address;
    } else {
      /* syscall - lookup syscall */
      struct syscall *sc;      
      sc = getsyscall( glob.tok.val );
      if( !sc ) usage( "Unknown syscall %s", glob.tok.val );
      siginfo = sc->siginfo;
      addr = sc->id;
    }
    expecttok( f, TOK_NAME );
    
    expecttok( f, TOK_OPAREN );
    ipar = 0;
    while( glob.tok.type != TOK_CPAREN ) {
      if( ipar > ((siginfo >> 24) & 0x1f) ) usage( "Too many params supplied to proc %s", procname );

      if( (siginfo >> (3*ipar)) & 0x4 ) {
	/* var type param requires a variable name */
	if( glob.tok.type != TOK_NAME ) usage( "Param %u expected a var name", ipar );
	v = getlocal( glob.currentproc, glob.tok.val );
	if( v ) {
	  /* local var - push address */
	  emit_leasp( v->offset );
	} else {
	  v = getglobal( glob.tok.val );
	  if( v ) {
	    /* global var - push address */
	    emit_ldi32( v->address );	    
	  } else {
	    struct param *p = getparam( glob.currentproc, glob.tok.val );
	    if( !p ) usage( "Unknown variable or parameter %s", glob.tok.val );
	    if( p->isvar ) {
	      /* var type parameter - value on stack is already an address */
	      emit_ldsp( p->offset + glob.currentproc->localsize + glob.stackoffset );
	    } else {
	      /* get address of parameter */
	      emit_leasp( p->offset + glob.currentproc->localsize + glob.stackoffset );
	    }
	  }

	}
	
	expecttok( f, TOK_NAME );
      } else {
	parseexpr( f );
      }
      
      ipar++;
      if( !accepttok( f, TOK_COMMA ) ) break;
    }
    expecttok( f, TOK_CPAREN );      

    if( glob.pass == 2 && (ipar < ((siginfo >> 24) & 0x1f)) ) usage( "Insufficient params supplied to proc %s", procname );
    
    if( kw == 1 ) emit_call( addr );
    else emit_syscall( addr );

    /* remove args from stack */
    emit_subsp( ((siginfo >> 24) & 0x1f) * 4 );
    glob.stackoffset -= ((siginfo >> 24) & 0x1f) * 4;
    
  } else if( acceptkeyword( f, "if" ) ) {
    /* if expr then statement [ else statement ] */
    uint16_t elseaddr, endaddr;
    struct label *l;
    char lnameelse[MAXNAME], lnameend[MAXNAME];

    getlabelname( "ELSE", lnameelse );
    getlabelname( "END", lnameend );

    if( glob.pass == 1 ) {
      elseaddr = 0;
      endaddr = 0;
    } else {
      l = getlabel( lnameelse );
      if( !l ) usage( "Failed to get label" );
      elseaddr = l->address;

      l = getlabel( lnameend );
      if( !l ) usage( "Failed to label" );
      endaddr = l->address;
    }
    
    parseexpr( f );
    emit_not();
    emit_br( elseaddr );
    expectkeyword( f, "then" );
    parsestatement( f );
    emit_jmp( endaddr );
    addlabel( lnameelse ); // else address 
    if( acceptkeyword( f, "else" ) ) {
      parsestatement( f );
    }
    addlabel( lnameend ); // end address
    
  } else if( acceptkeyword( f, "do" ) ) {
    /* do statement while expr */
  } else if( acceptkeyword( f, "while" ) ) {
    /* while expr do statement */
    struct label *l;
    uint16_t addr1, addr2;
    char lnamew[MAXNAME], lnamedo[MAXNAME];

    getlabelname( "WHILE", lnamew );
    getlabelname( "DO", lnamedo );

    if( glob.pass == 1 ) {
      addr1 = 0;
      addr2 = 0;
    } else {
      l = getlabel( lnamew );
      if( !l ) usage( "Failed to get label" );
      addr1 = l->address;
      l = getlabel( lnamedo );
      if( !l ) usage( "Failed to get label" );
      addr2 = l->address;
    }
    
    addlabel( lnamew );
    parseexpr( f );
    emit_not();
    emit_br( addr2 );
    expectkeyword( f, "Do" );
    parsestatement( f );
    emit_jmp( addr1 );
    addlabel( lnamedo );

  } else if( acceptkeyword( f, "goto" ) ) {
    /* goto label */
    struct label *l;
    uint16_t addr;
    
    if( glob.tok.type != TOK_NAME ) usage( "Expected label identifier" );
    if( glob.pass == 1 ) {
      addr = 0;
    } else {
      l = getlabel( glob.tok.val );
      if( !l ) usage( "Unknown label %s", glob.tok.val );
      addr = l->address;
    }
    
    emit_jmp( addr );
    expecttok( f, TOK_NAME );
  } else if( glob.tok.type == TOK_NAME ) {
    /* varname = expr */
    struct var *v;
    struct param *p;

    if( glob.tok.val[strlen( glob.tok.val ) - 1] == ':' ) {
      glob.tok.val[strlen( glob.tok.val ) - 1] = '\0';
      addlabel( glob.tok.val );
      free( glob.tok.val );
      memset( &glob.tok, 0, sizeof(glob.tok) );
      glob.tok.type = TOK_SEMICOLON;
      
      return 1;
    }
    
    v = getlocal( glob.currentproc, glob.tok.val );
    if( v ) {
      expecttok( f, TOK_NAME );
      expecttok( f, TOK_EQ );
      parseexpr( f );
      
      emit_stsp( v->offset + glob.stackoffset );
    } else {
      v = getglobal( glob.tok.val );
      if( v ) {
	emit_ldi32( v->address );
	
	expecttok( f, TOK_NAME );
	expecttok( f, TOK_EQ );
	parseexpr( f );
	
	emit_st();
      } else {
	p = getparam( glob.currentproc, glob.tok.val );
	if( !p ) return 0;

	if( p->isvar ) {
	  emit_ldsp( p->offset + glob.currentproc->localsize + glob.stackoffset ); /* var param so value on stack is address, just load it */
	} else {
	  emit_leasp( p->offset + glob.currentproc->localsize +  glob.stackoffset ); /* get address of param */
	}
	
	expecttok( f, TOK_NAME );
	expecttok( f, TOK_EQ );
	parseexpr( f );

	emit_st(); /* store value */
      }
    }

  } else return 0;

  return 1;
}

static void parseproceduresig( FILE *f, char *procname, struct param *params, int *nparams, uint32_t *siginfop ) {
  /* parse procedure */
  int nparam, i;
  uint32_t siginfo, arraylen;
  
  if( glob.tok.type != TOK_NAME ) usage( "Expected procname not %s", gettokname( glob.tok.type ) );
  strcpy( procname, glob.tok.val );
  expecttok( f, TOK_NAME );

  nparam = 0;
  siginfo = 0;
  memset( params, 0, sizeof(*params) * MAXPARAM );
  
  expecttok( f, TOK_OPAREN );
  /* parse params */
  while( glob.tok.type != TOK_CPAREN ) {
    if( nparam >= MAXPARAM ) usage( "Max params exceeded" );
    
    /* [var] name : type */
    if( acceptkeyword( f, "var" ) ) {
      params[nparam].isvar = 1;
    }

    if( glob.tok.type != TOK_NAME ) usage( "Expected parameter name not %s", gettokname( glob.tok.type ) );
    /* check for name clash */
    for( i = 0; i < nparam; i++ ) {
      if( strcasecmp( params[i].name, glob.tok.val ) == 0 ) usage( "Param name %s already exists", glob.tok.val );
    }
    if( getglobal( glob.tok.val ) ) usage( "Param %s name clash with global", glob.tok.val );
    
    strncpy( params[nparam].name, glob.tok.val, MAXNAME - 1 );
    expecttok( f, TOK_NAME );
    expecttok( f, TOK_COLON );
    parsevartype( f, &params[nparam].type, &arraylen );
    if( arraylen ) usage( "array vars not allowed in proc params" );
    if( params[nparam].type == VAR_TYPE_OPAQUE ) {
      if( nparam == 0 || (params[nparam - 1].type != VAR_TYPE_U32) ) usage( "Opaque parameters MUST follow a u32 implicit length parameter" );

      if( params[nparam].isvar && !params[nparam - 1].isvar ) usage( "Var type opaque parameters MUST follow a var type u32 parameter" );
      if( params[nparam - 1].isvar && !params[nparam].isvar ) usage( "Non-var opaque parameters MUST follow a non-var u32 parameter" );
    }
    
    siginfo |= ((params[nparam].type | (params[nparam].isvar ? 4 : 0)) << (nparam * 3));
    nparam++;
    
    if( !accepttok( f, TOK_COMMA ) ) break;
  }
  expecttok( f, TOK_CPAREN );
  siginfo |= (nparam << 24);

  for( i = 0; i < nparam; i++ ) {
    params[(nparam - 1) - i].offset = 8 + 4*i;
  }
  
  *nparams = nparam;
  *siginfop = siginfo;
  return; 
}

static void parsefile( FILE *f ) {
  struct token *tok;

  tok = nexttok( f );
  if( !tok ) {
    printf( ";; Empty file\n" );
    return;
  }
  
  expectkeyword( f, "program" );
  if( glob.tok.type != TOK_NAME ) usage( "Unexpected symbol %s - expected program name", gettokname( glob.tok.type ) );
  strncpy( glob.progname, glob.tok.val, sizeof(glob.progname) - 1 );  
  expecttok( f, TOK_NAME );
  expecttok( f, TOK_OPAREN );
  if( glob.tok.type != TOK_U32 ) usage( "Expected progid" );
  glob.progid = glob.tok.u32;
  expecttok( f, TOK_U32 );
  expecttok( f, TOK_COMMA );
  if( glob.tok.type != TOK_U32 ) usage( "Expected versid" );
  glob.versid = glob.tok.u32;
  expecttok( f, TOK_U32 );

  while( accepttok( f, TOK_COMMA ) ) {
    if( glob.tok.type != TOK_NAME ) usage( "Expected exported proc name" );
    addexport( glob.tok.val );
    expecttok( f, TOK_NAME );
  }

  expecttok( f, TOK_CPAREN );
  expecttok( f, TOK_SEMICOLON );

  expectkeyword( f, "begin" );

  /* constants, declarations etc */
  while( 1 ) {
    if( acceptkeyword( f, "const" ) ) {
      /* const name = value */
      char cname[MAXNAME];
      
      if( glob.tok.type != TOK_NAME ) usage( "const expects name not %s", gettokname( glob.tok.type ) );
      strncpy( cname, glob.tok.val, MAXNAME - 1 );
      expecttok( f, TOK_NAME );
      expecttok( f, TOK_EQ );
      switch( glob.tok.type ) {
      case TOK_U32:
	addconstval( cname, VAR_TYPE_U32, (char *)&glob.tok.u32, 4 );
	expecttok( f, TOK_U32 );
	break;
      case TOK_STRING:
	addconstval( cname, VAR_TYPE_STRING, strdup( glob.tok.val ), strlen( glob.tok.val ) + 1 );
	expecttok( f, TOK_STRING );      
	break;
      default:
	usage( "Bad constant type" );
	break;
      }
      expecttok( f, TOK_SEMICOLON );
    } else if( acceptkeyword( f, "declare" ) ) {
      /* declare procedure name(...) */
      char procname[MAXNAME];
      struct param params[MAXPARAM];
      int nparams;
      uint32_t siginfo;
      struct proc *proc;
      
      expectkeyword( f, "procedure" );
      parseproceduresig( f, procname, params, &nparams, &siginfo );
      if( glob.pass == 1 ) {
	proc = getproc( procname );
	if( proc ) {
	  if( proc->siginfo != siginfo ) usage( "Declaration does not match definition for %s", procname );
	} else {
	  proc = addproc( procname, params, nparams, siginfo );
	  proc->address = 0;
	}
      }
      
      expecttok( f, TOK_SEMICOLON );
    } else break;
  }
  
  /* parse data segment - i.e. global variables */
  while( acceptkeyword( f, "var" ) ) {
    char varname[MAXNAME];
    var_t vartype;
    uint32_t arraylen;
    struct var *v;
    
    /* var name : type; */
    if( glob.tok.type != TOK_NAME ) usage( "Expected var name not %s", gettokname( glob.tok.type ) );
    strncpy( varname, glob.tok.val, MAXNAME - 1 );
    expecttok( f, TOK_NAME );
    expecttok( f, TOK_COLON );
    parsevartype( f, &vartype, &arraylen );
    expecttok( f, TOK_SEMICOLON );

    v = addglobal( varname, vartype, arraylen );
  }

  /* everything else following this is in the text segment */
  while( glob.tok.type == TOK_NAME ) {
    if( acceptkeyword( f, "procedure" ) ) {
      /* parse procedure */
      char name[MAXNAME];
      struct param params[MAXPARAM];
      int nparams;
      uint32_t siginfo;
      struct proc *proc;
      var_t vartype;
      uint32_t arraylen;
      
      /* reset stackoffset at start of new procedure */
      if( glob.stackoffset != 0 ) printf( "Invalid stack offset %04x?\n", glob.stackoffset );
      glob.stackoffset = 0;

      parseproceduresig( f, name, params, &nparams, &siginfo );
      if( glob.pass == 1 ) {
	proc = getproc( name );
	if( proc ) {
	  if( proc->address ) usage( "Duplicate procedure definition for %s", name  );
	  if( proc->siginfo != siginfo ) usage( "Declaration signature does not match definition for %s", name );
	  proc->address = glob.pc;
	} else {
	  proc = addproc( name, params, nparams, siginfo );
	}
      } else {
	proc = getproc( name );
      }
      glob.currentproc = proc;
      
      expectkeyword( f, "Begin" );
      /* parse body: var definitions followed by statements */
      while( acceptkeyword( f, "var" ) ) {
	/* var name : type; */
	if( glob.tok.type != TOK_NAME ) usage( "Expected var name not %s", gettokname( glob.tok.type ) );
	strncpy( name, glob.tok.val, MAXNAME - 1 );	
	expecttok( f, TOK_NAME );
	expecttok( f, TOK_COLON );
	parsevartype( f, &vartype, &arraylen );

	addlocal( proc, name, vartype, arraylen );
	expecttok( f, TOK_SEMICOLON );
      }

      /* allocate space for local variables */
      if( proc->localsize ) {
	emit_addsp( proc->localsize );
      }
      
      while( parsestatement( f ) ) {
	expecttok( f, TOK_SEMICOLON );
      }

      /* free local vars */
      if( proc->localsize ) {
	emit_subsp( proc->localsize );
      }
      
      emit_ret();
      expectkeyword( f, "End" );
      expecttok( f, TOK_SEMICOLON );
      printf( ";; ------------------------\n\n" );
      
    } else if( acceptkeyword( f, "const" ) ) {
      /* parse constant data: const var name := value (type infered from value) */
      char varname[MAXNAME];
      var_t vartype;
      char *val;
      int len;
      
      val = NULL;
      len = 0;
      vartype = 0;
      
      expectkeyword( f, "var" );
      if( glob.tok.type != TOK_NAME ) usage( "Expected const var name" );
      strncpy( varname, glob.tok.val, MAXNAME - 1 );
      expecttok( f, TOK_NAME );
      expecttok( f, TOK_EQ );
      switch( glob.tok.type ) {
      case TOK_U32:
	vartype = VAR_TYPE_U32;
	val = malloc( 4 );
	memcpy( val, &glob.tok.u32, 4 );
	len = 4;
	expecttok( f, TOK_U32 );
	break;
      case TOK_STRING:
	vartype = VAR_TYPE_STRING;
	len = strlen( glob.tok.val ) + 1;
	if( len % 4 ) len += 4 - (len % 4);
	val = malloc( len );
	strncpy( val, glob.tok.val, len );
	expecttok( f, TOK_STRING );      
	break;
      default:
	usage( "Invalid const type, must be u32 or string" );
	break;
      }
      
      addconst( varname, vartype, val, len );
      
      expecttok( f, TOK_SEMICOLON );
    } else expectkeyword( f, "end" );
  }

  //expectkeyword( f, "end" );
  expecttok( f, TOK_PERIOD );

  printf( ";; Done\n" );
}

static void processfile( char *path ) {
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

  switch( glob.pass ) {
  case 0:
    /* print lexing info if first pass */
    while( 1 ) {
      struct token *tok;
      tok = nexttok( f );
      if( !tok ) break;
      
      switch( tok->type ) {
      case TOK_U32:
	printf( "U32: %u\n", tok->u32 );
	break;
      case TOK_STRING:
	printf( "STRING: %s\n", tok->val );
	break;
      case TOK_NAME:
	printf( "NAME: %s\n", tok->val );
	break;
      default:
	printf( "%s\n", gettokname( tok->type ) );
	break;
      }

    }
    break;
  case 1:
    /* first pass - collect info on data variables, procs etc */
    glob.pc = 0x8000;
    parsefile( f );
    glob.datasize = glob.globals ? glob.globals->address + glob.globals->size : 0;
    glob.textsize = glob.pc;
    break;
  case 2:
    /* second pass - generate code */
    glob.pc = 0x8000;
    glob.stackoffset = 0;
    parsefile( f );
    break;    
  }
  
  
  fclose( f );
}

struct headerinfo {
  uint32_t magic;
  uint32_t version;
  char name[MAXNAME];
  uint32_t progid;
  uint32_t versid;
  uint32_t datasize;
  uint32_t textsize;
  uint32_t nprocs;
  struct {
    char name[MAXNAME];
    uint32_t address;
    uint32_t siginfo;
  } procs[MAXPROC];
  uint32_t spare[32];
};
  
static void compile_file( char *path, char *outpath ) {
  struct headerinfo header;
  struct export *e;
  struct proc *proc;
  
  glob.outfile = fopen( outpath, "wb" );
  if( !glob.outfile ) usage( "Unable to open output file" );
  
  //glob.pass = 0;  
  //processfile( path );

  printf( "--------------------- Pass 1 -------------------- \n" );
  glob.pass = 1;  
  processfile( path );
  
  printf( "--------------------- Pass 2 -------------------- \n" );
  glob.pass = 2;

  /* emit header section */
  memset( &header, 0, sizeof(header) );
  header.magic = 123;
  header.version = 1;
  strcpy( header.name, glob.progname );
  header.progid = glob.progid;
  header.versid = glob.versid;
  header.datasize = glob.datasize;
  header.textsize = glob.textsize;
  e = glob.exports;
  while( e ) {
    if( header.nprocs >= MAXPROC ) usage( "Max procs exceeded" );
    
    proc = getproc( e->name );
    if( !proc ) usage( "Cannot export %s - no proc found", e->name );
    strcpy( header.procs[header.nprocs].name, proc->name );
    header.procs[header.nprocs].address = proc->address;
    header.procs[header.nprocs].siginfo = proc->siginfo;
    header.nprocs++;
    e = e->next;
  }
  fwrite( &header, 1, sizeof(header), glob.outfile );

  /* reset label counter */
  glob.labelidx = 0;

  /* process 2nd pass */
  processfile( path );

  fclose( glob.outfile );


  printf( "--------------\n" );
  {
    struct label *l;
    l = glob.labels;
    while( l ) {
      printf( "Label: %s 0x%x\n", l->name, l->address );
      l = l->next;
    }
  }
  {
    struct var *v;
    v = glob.globals;
    while( v ) {
      printf( "Global: %s 0x%0x\n", v->name, v->address );
      v = v->next;
    }
  }
  {
    struct proc *p;
    int i;
    
    p = glob.procs;
    while( p ) {
      printf( "Proc: %s 0x%0x siginfo 0x%08x\n", p->name, p->address, p->siginfo );
      for( i = 0; i < p->nparams; i++ ) {
	printf( "  Param %u: %s%s : %s\n",
		i, p->params[i].isvar ? "var " : "",
		p->params[i].name,
		p->params[i].type == VAR_TYPE_U32 ? "U32" :
		p->params[i].type == VAR_TYPE_STRING ? "String" :
		p->params[i].type == VAR_TYPE_OPAQUE ? "Opaque" :
		"Other" );
      }
      p = p->next;
    }
  }
  {
    struct export *e;
    e = glob.exports;
    while( e ) {
      printf( "Export: %s\n", e->name );
      e = e->next;
    }
  }
  {
    struct constvar *v;
    v = glob.consts;
    while( v ) {
      printf( "Const: %s 0x%0x\n", v->name, v->address );
      v = v->next;
    }
  }
  {
    struct constval *v;
    v = glob.constvals;
    while( v ) {
      printf( "Const val: %s\n", v->name );
      v = v->next;
    }
  }
      
}
