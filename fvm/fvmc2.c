
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
    TOK_SHR,       /* >> */
    TOK_SHL,       /* << */
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
  uint64_t u64;
};

static struct token *copytoken( struct token *tok ) {
  struct token *c;
  c = malloc( sizeof(*c) );
  memset( c, 0, sizeof(*c) );
  c->len = tok->len;
  c->type = tok->type;
  c->u32 = tok->u32;
  c->u64 = tok->u64;
  if( tok->val ) {
    c->val = malloc( tok->len );
    memcpy( c->val, tok->val, tok->len );
  }
  return c;
}
static void freetoken( struct token *tok ) {
  if( tok->val ) free( tok->val );
  free( tok );
}

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

static void emitdata( void *data, int len );

#define MAXPROC 32 
#define MAXPARAM 8
#define MAXNAME 64

typedef enum {
    VAR_TYPE_U32 = 0,
    VAR_TYPE_U64 = 1,
    VAR_TYPE_STRING = 2,
    VAR_TYPE_OPAQUE = 3,
} var_t;

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
  uint32_t size; /* size of the variable. u32 is 4, u64 is 8 etc */
};

/* procedure parameter */
struct param {
  char name[MAXNAME];
  var_t type;
  int isvar;
  uint32_t size;    /* size is 4 for everything except u64 which is 8. var u64 is 4 because it is a pointer */
  uint32_t offset;  /* stack offset */
};

struct proc {
  struct proc *next;
  char name[MAXNAME];
  uint32_t address;
  struct param params[MAXPARAM];
  int nparams;
  uint32_t paramsize; /* total size of all params */
  struct var *locals;
  uint32_t localsize; /* total size of all locals */
  uint32_t siginfo;
};

struct label {
  struct label *next;
  uint32_t id;
  uint32_t address;
};

struct export {
  struct export *next;
  char name[64];
};

struct constvar {
  struct constvar *next;
  char name[MAXNAME];
  var_t type;
  uint32_t address;
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

  char progname[64];
  uint32_t progid, versid;

  uint32_t labelidx;
  struct label *labels;
  struct var *globals;
  struct proc *procs;
  struct export *exports;
  struct constvar *consts;
  
  struct token tok;
  uint32_t pc;
  uint32_t datasize;
  uint32_t textsize;

  FILE *outfile;
  struct proc *currentproc;
} glob;

static struct label *getlabel( void ) {
  struct label *l;
  l = glob.labels;
  while( l ) {
    if( l->id == glob.labelidx ) return l;
    l = l->next;
  }
  return NULL;
}
static struct label *addlabel( void ) {
  struct label *l;
  
  if( glob.pass == 2 ) {
    l = getlabel();
    glob.labelidx++;
    return l;
  }
  
  l = malloc( sizeof(*l) );
  l->id = glob.labelidx;
  glob.labelidx++;
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
  v->size = type == VAR_TYPE_U64 ? 8 : 4;
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
static struct proc *addproc( char *name ) {
  struct proc *p;

  if( glob.pass == 2 ) return getproc( name );
  
  p = getproc( name );
  if( p ) usage( "Proc %s already exists", name );

  p = malloc( sizeof(*p) );
  memset( p, 0, sizeof(*p) );
  strcpy( p->name, name );
  p->address = glob.pc;
  
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

static struct param *addparam( struct proc *proc, char *name, var_t type, int isvar ) {
  struct param *p;

  if( glob.pass == 2 ) return getparam( proc, name );
  
  p = getparam( proc, name );
  if( p ) usage( "Proc %s Param %s already exists", proc->name, name );
  if( proc->nparams >= MAXPARAM ) usage( "Can't add parameter %s to proc %s: Max parameters already used", name, proc->name );

  p = &proc->params[proc->nparams];
  strcpy( p->name, name );
  p->type = type;
  p->isvar = isvar;
  p->size = 4;
  if( type == VAR_TYPE_U64 && !isvar ) p->size = 8;

  /* we push params from right to left to rightmost is deepest in stack */
  p->offset = proc->nparams == 0 ? 4 : proc->params[proc->nparams - 1].offset + proc->params[proc->nparams - 1].size; 
  proc->nparams++;
  proc->paramsize += p->size;
  return p;  
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
  v->size = type == VAR_TYPE_U64 ? 8 : 4;
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
  for( i = 0; i < proc->nparams; i++ ) {
    proc->params[i].offset += v->size;
  }
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

typedef enum {
    OP_NOP = 0,    /* no op */
    OP_LDI32 = 1,  /* load 32bit immediate arg:u32*/
    OP_LDI64 = 2,  /* load 64bit immediate arg:u64*/
    OP_LEA = 3,    /* load address relative to pc arg:u16 */
    OP_ADDSP = 4,  /* add constant to sp (allocate stack space) arg:u16*/
    OP_SUBSP = 5,  /* subtract const from sp (free stack space) arg:u16*/
    OP_CALL = 6,   /* push pc and jump arg:u16*/
    OP_RET = 7,    /* pop pc and jump */
    OP_LEASP = 8,  /* load address relative to sp arg:u16*/
    OP_LDSP = 9,   /* load value relative to sp arg:u16*/
    OP_STSP = 10,  /* store value relative to sp arg:u16*/
    OP_BR = 11,    /* jump to const address if true arg:u16 */
    OP_EQ = 12,    /* pop two values, push 1 if equal 0 if false */
    OP_NEQ = 13,
    OP_GT = 14,
    OP_GTE = 15,
    OP_LT = 16,
    OP_LTE = 17,
    OP_JMP = 18,   /* unconditional jump arg:u16*/
    OP_ADD = 19,
    OP_SUB = 20,
    OP_MUL = 21,
    OP_DIV = 22,
    OP_MOD = 23,
    OP_AND = 24,
    OP_OR = 25,
    OP_XOR = 26,
    OP_NOT = 27,
    OP_SHL = 28,
    OP_SHR = 29,
    OP_LD = 30, /* load from address */
    OP_ST = 31, /* store to address */
    OP_SYSCALL = 32, /* syscall arg:u16 */
} op_t;


static void emitopcode( op_t op, void *data ) {
  uint8_t u8;

  printf( "Emitopcode: %u\n", op );
  
  u8 = op;
  fwrite( &u8, 1, 1, glob.outfile );
  glob.pc += 1;
  switch( op ) {
  case OP_LDI32:
    fwrite( data, 1, 4, glob.outfile );
    glob.pc += 4;    
    break;
  case OP_LDI64:
    fwrite( data, 1, 8, glob.outfile );
    glob.pc += 8;    
    break;
  case OP_LEA:
  case OP_ADDSP:
  case OP_SUBSP:
  case OP_CALL:
  case OP_LEASP:
  case OP_LDSP:
  case OP_STSP:
  case OP_BR:
  case OP_JMP:
  case OP_SYSCALL:
    fwrite( data, 1, 2, glob.outfile );
    glob.pc += 2;
    break;
  default:
    break;
  }
}

static void emitdata( void *data, int len ) {
  fwrite( data, 1, len, glob.outfile );
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
						(:u32 0) (:u64 1) (:string 2) (:opaque 3))
					      (if isvar 4 0))
				      (* 3 i))))))
	     (setf p (logior p (ash (length params) 24)))
	     p))
#endif

struct syscall {
  char *name;
  uint16_t id;
  uint32_t pars;
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
  if( glob.tok.type != TOK_NAME ) usage( "Unexpect symbol type %s - expected u32|u64|string|opaque", gettokname( glob.tok.type ) );
  if( strcasecmp( glob.tok.val, "u32" ) == 0 ) *type = VAR_TYPE_U32;
  else if( strcasecmp( glob.tok.val, "u64" ) == 0 ) *type = VAR_TYPE_U64;
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
  struct label *l;
  tok_t optype;
  
  /*
   * exprs: 
   * varname : load value of varname 
   * u32 : constant 
   * u64 : constant 
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
    emitopcode( OP_NOT, NULL );    
  } else if( glob.tok.type == TOK_U32 ) {
    emitopcode( OP_LDI32, &glob.tok.u32 );
    expecttok( f, TOK_U32 );
  } else if( glob.tok.type == TOK_U64 ) {
    emitopcode( OP_LDI64, &glob.tok.u64 );
    expecttok( f, TOK_U64 );    
  } else if( glob.tok.type == TOK_STRING ) {
    l = getlabel();
    addr = l ? l->address : 0;
    emitopcode( OP_JMP, &addr );
    emitdata( glob.tok.val, strlen( glob.tok.val ) + 1 );
    addlabel();

    addr = -(strlen( glob.tok.val ) + 1);
    emitopcode( OP_LEA, &addr );

    expecttok( f, TOK_STRING );
  } else if( glob.tok.type == TOK_NAME ) {
    struct var *v;
    struct param *p;
    
    v = getlocal( glob.currentproc, glob.tok.val );
    if( v ) {
      addr = v->offset;
      emitopcode( OP_LDSP, &addr );
    } else {
      v = getglobal( glob.tok.val );
      if( !v ) {
	p = getparam( glob.currentproc, glob.tok.val );
	if( !p ) return;
	addr = p->offset;
	emitopcode( OP_LDSP, &addr );
      } else {
	addr = v->address;
	emitopcode( OP_LD, &addr ); /* xxx */
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
      emitopcode( OP_ADD, NULL );
      break;
    case TOK_MINUS:
      expecttok( f, optype );
      parseexpr( f );      
      emitopcode( OP_SUB, NULL );
      break;
    case TOK_MUL:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_MUL, NULL );
      break;
    case TOK_DIV:
      expecttok( f, optype );
      parseexpr( f );         
      emitopcode( OP_DIV, NULL );
      break;
    case TOK_MOD:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_MOD, NULL );
      break;
    case TOK_AND:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_AND, NULL );
      break;
    case TOK_OR:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_OR, NULL );
      break;
    case TOK_XOR:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_XOR, NULL );
      break;
    case TOK_SHL:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_SHL, NULL );
      break;
    case TOK_SHR:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_SHR, NULL );
      break;
    case TOK_EQ:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_EQ, NULL );
      break;
    case TOK_NEQ:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_NEQ, NULL );
      break;            
    case TOK_GT:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_GT, NULL );
      break;            
    case TOK_GTE:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_GTE, NULL );
      break;            
    case TOK_LT:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_LT, NULL );
      break;            
    case TOK_LTE:
      expecttok( f, optype );
      parseexpr( f );            
      emitopcode( OP_LTE, NULL );
      break;                  
    default:
      break;
    }

  }
  
}
      

static int parsestatement( FILE *f ) {
  if( acceptkeyword( f, "call" ) ) {
    struct proc *proc;
    uint16_t addr;
    int ipar;
    struct var *v;
    
    /* call procname(args...) */
    if( glob.tok.type != TOK_NAME ) usage( "Expected procname" );
    proc = getproc( glob.tok.val );
    if( !proc && glob.pass == 2 ) usage( "Unknown proc %s", glob.tok.val );

    expecttok( f, TOK_OPAREN );
    ipar = 0;
    while( glob.tok.type != TOK_CPAREN ) {
      if( ipar > proc->nparams ) usage( "Too many params supplied to proc %s", proc->name );

      if( proc->params[ipar].isvar ) {
	/* var type param requires a variable name */
	if( glob.tok.type != TOK_NAME ) usage( "Param %s expected a var name", proc->params[ipar].name );
	v = getlocal( proc, glob.tok.val );
	if( v ) {
	  /* local var - push address */
	  addr = v->offset;
	  emitopcode( OP_LEASP, &addr );
	} else {
	  v = getglobal( glob.tok.val );
	  if( !v ) usage( "Unknown variable %s", glob.tok.val );

	  /* global var - push address */
	  emitopcode( OP_LDI32, &v->address );
	}
	
      } else {
	parseexpr( f );
      }
      
      ipar++;
      if( !accepttok( f, TOK_COMMA ) ) break;
    }
    expecttok( f, TOK_CPAREN );      

    if( ipar < proc->nparams ) usage( "Insufficient params supplied to proc %s", proc->name );
    
    addr = proc ? proc->address : 0;
    emitopcode( OP_CALL, &addr );
  } else if( acceptkeyword( f, "syscall" ) ) {
    /* syscall procname(Args...) */
    struct syscall *sc;

    sc = getsyscall( glob.tok.val );
    if( !sc ) usage( "Unknown syscall %s", glob.tok.val );
    expecttok( f, TOK_NAME );
    expecttok( f, TOK_OPAREN );
    while( glob.tok.type != TOK_CPAREN ) {
      parseexpr( f );
      if( !accepttok( f, TOK_COMMA ) ) break;
    }
    expecttok( f, TOK_CPAREN );
    emitopcode( OP_SYSCALL, &sc->id );
  } else if( acceptkeyword( f, "if" ) ) {
    /* if expr then statement [ else statement ] */
    uint16_t elseaddr, endaddr;
    struct label *l;
    
    l = getlabel();
    elseaddr = l ? l->address : 0;
    l = getlabel();
    endaddr = l ? l->address : 0;
    
    parseexpr( f );
    emitopcode( OP_NOT, NULL );
    emitopcode( OP_BR, &elseaddr );
    expectkeyword( f, "then" );
    parsestatement( f );
    emitopcode( OP_JMP, &endaddr );
    addlabel(); // else address 
    if( acceptkeyword( f, "else" ) ) {
      parsestatement( f );
    }
    addlabel(); // end address
    
  } else if( acceptkeyword( f, "do" ) ) {
    /* do statement while expr */
  } else if( acceptkeyword( f, "while" ) ) {
    /* while expr do statement */
    struct label *l;
    uint16_t addr1, addr2;

    l = addlabel();
    addr1 = l ? l->address : 0;
    l = getlabel();
    addr2 = l ? l->address : 0;
    
    parseexpr( f );
    emitopcode( OP_NOT, NULL );
    emitopcode( OP_BR, &addr2 );
    expectkeyword( f, "Do" );
    parsestatement( f );
    emitopcode( OP_JMP, &addr1 );
    addlabel();
    
  } else if( glob.tok.type == TOK_NAME ) {
    /* varname = expr */
    struct var *v;
    struct param *p;
    uint16_t addr;
    v = getlocal( glob.currentproc, glob.tok.val );
    if( v ) {
      expecttok( f, TOK_NAME );
      expecttok( f, TOK_EQ );
      parseexpr( f );
      
      addr = v->offset;    
      emitopcode( OP_STSP, &addr );
    } else {
      v = getglobal( glob.tok.val );
      if( v ) {
	expecttok( f, TOK_NAME );
	expecttok( f, TOK_EQ );
	parseexpr( f );
	
	addr = v->offset;    
	emitopcode( OP_ST, &addr );	 /* XXX */
      } else {
	p = getparam( glob.currentproc, glob.tok.val );
	if( !p ) return 0;

	expecttok( f, TOK_NAME );
	expecttok( f, TOK_EQ );
	parseexpr( f );

	if( p->isvar ) {
	  addr = p->offset;
	  emitopcode( OP_ST, NULL );
	} else {
	  addr = p->offset;
	  emitopcode( OP_STSP, &addr );
	}
      }
    }

  } else return 0;

  return 1;
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
    printf( ";; Adding export %s\n", glob.tok.val );
    addexport( glob.tok.val );
    expecttok( f, TOK_NAME );
  }

  expecttok( f, TOK_CPAREN );
  expecttok( f, TOK_SEMICOLON );

  expectkeyword( f, "begin" );
  
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
    printf( ";; Adding global %s type=%u arraylen=%u address=%u\n", varname, vartype, arraylen, v->address );    
  }

  /* everything else following this is in the text segment */
  while( glob.tok.type == TOK_NAME ) {
    if( acceptkeyword( f, "procedure" ) ) {
      /* parse procedure */
      char name[MAXNAME];
      var_t vartype;
      int isvar;
      struct proc *proc;
      uint32_t arraylen;
      int nparam;
      
      if( glob.tok.type != TOK_NAME ) usage( "Expected procname not %s", gettokname( glob.tok.type ) );
      proc = addproc( glob.tok.val );
      glob.currentproc = proc;
      expecttok( f, TOK_NAME );

      nparam = 0;
      proc->siginfo = 0;
      expecttok( f, TOK_OPAREN );
      /* parse params */
      while( glob.tok.type != TOK_CPAREN ) {
	isvar = 0;
	vartype = 0;
	strcpy( name, "" );

	/* [var] name : type */
	if( acceptkeyword( f, "var" ) ) {
	  isvar = 1;
	}

	if( glob.tok.type != TOK_NAME ) usage( "Expected parameter name not %s", gettokname( glob.tok.type ) );
	strncpy( name, glob.tok.val, MAXNAME - 1 );
	expecttok( f, TOK_NAME );
	expecttok( f, TOK_COLON );
	parsevartype( f, &vartype, &arraylen );
	if( arraylen ) usage( "array vars not allowed in proc params" );
	
	printf( ";; add param %s type=%u isvar=%s\n", name, vartype, isvar ? "true" : "false" );
	addparam( proc, name, vartype, isvar );
	proc->siginfo |= ((vartype | (isvar ? 4 : 0)) << (nparam * 3));
	nparam++;
	
	if( !accepttok( f, TOK_COMMA ) ) break;
      }
      expecttok( f, TOK_CPAREN );
      proc->siginfo |= (nparam << 24);
      
      expectkeyword( f, "Begin" );
      /* parse body: var definitions followed by statements */
      while( acceptkeyword( f, "var" ) ) {
	/* var name : type; */
	if( glob.tok.type != TOK_NAME ) usage( "Expected var name not %s", gettokname( glob.tok.type ) );
	strncpy( name, glob.tok.val, MAXNAME - 1 );	
	expecttok( f, TOK_NAME );
	expecttok( f, TOK_COLON );
	parsevartype( f, &vartype, &arraylen );

	printf( ";; add local %s type=%u arraylen=%u\n", name, vartype, arraylen );
	addlocal( proc, name, vartype, arraylen );
	expecttok( f, TOK_SEMICOLON );
      }

      while( parsestatement( f ) ) {
	expecttok( f, TOK_SEMICOLON );
      }
      
      
      expectkeyword( f, "End" );
      expecttok( f, TOK_SEMICOLON );
      
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
      case TOK_U64:
	vartype = VAR_TYPE_U64;
	val = malloc( 8 );
	memcpy( val, &glob.tok.u64, 8 );
	len = 8;
	expecttok( f, TOK_U64 );      
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
	usage( "Invalid const type, must be u32 u64 or string" );
	break;
      }
      
      printf( ";; Adding const %s\n", varname );
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
    glob.pc = 0;
    parsefile( f );
    glob.datasize = glob.globals ? glob.globals->address + glob.globals->size : 0;
    glob.textsize = glob.pc;
    break;
  case 2:
    /* second pass - generate code */
    glob.pc = 0;
    parsefile( f );
    break;    
  }
  
  
  fclose( f );
}

struct headerinfo {
  uint32_t magic;
  uint32_t version;
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
  
  glob.pass = 0;  
  processfile( path );
  
  glob.pass = 1;  
  processfile( path );
  
  glob.pass = 2;

  /* emit header section */
  memset( &header, 0, sizeof(header) );
  header.magic = 123;
  header.version = 1;
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
      printf( "Label: %u 0x%x\n", l->id, l->address );
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
		p->params[i].type == VAR_TYPE_U64 ? "U64" :
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
      
}
