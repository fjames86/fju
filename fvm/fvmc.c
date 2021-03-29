
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#define strcasecmp _stricmp
#define strdup _strdup
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <stdint.h>
#include <inttypes.h>

#include <fju/fvm.h>
#include <fju/mmf.h>
#include <fju/sec.h>

#include "fvm-private.h"

static void usage( char *fmt, ... );


static void compile_file( char *path, char *outpath );
static void addincludepath( char *path );
static void disassemblefile( char *path );

static int fvmc_debug = 0;
static void fvmc_printf( char *fmt, ... ) {
  va_list args;
  
  if( fvmc_debug ) {
    va_start( args, fmt );
    vprintf( fmt, args );
    va_end( args );
  }
}

int fvmc_main( int argc, char **argv ) {
  int i;
  char *outpath = NULL;
  int disass;

  disass = 0;

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
    } else if( strcmp( argv[i], "-v" ) == 0 ) {
      fvmc_debug = 1;
    } else if( strcmp( argv[i], "-d" ) == 0 ) {
      disass = 1;
    } else {
      char outpathstr[256];
      int j;
      char *p, *lastdirsep;
      
      strncpy( outpathstr, argv[i], sizeof(outpathstr) );
      p = outpathstr;
      lastdirsep = NULL;
      while( p ) {
#ifdef WIN32
	p = strchr( p, '/' );
#else
	p = strchr( p, '/' );
#endif
	if( p ) {
	  lastdirsep = p;
	  p++;
	}
      }
      if( lastdirsep ) {
	*lastdirsep = '\0';
	addincludepath( outpathstr );
      }

      
      if( !outpath ) {
	sprintf( outpathstr, "%s.fvm", argv[i] );
	for( j = strlen( argv[i] ) - 1; j > 0; j-- ) {
	  if( outpathstr[j] == '.' ) {
	    outpathstr[j] = '\0';
	    strcat( outpathstr + j, ".fvm" );
	    break;
	  }
	}
	      
      } else {
	strncpy( outpathstr, outpath, sizeof(outpathstr) );
      }
      
      if( disass ) disassemblefile( argv[i] );
      else compile_file( argv[i], outpathstr );
      break;
    }
    i++;
  }

  return 0;
}

/* ----------- lexing ------------------- */

static void incrementlinecount( void );

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
    TOK_QUESTION,  /* ? */
} tok_t;

static struct {
  tok_t type;
  char *name;
} toknames[] = { { TOK_U32, "u32" }, { TOK_STRING, "String" }, { TOK_NAME, "Name" }, {TOK_SEMICOLON, "Semicolon" },
		 { TOK_COLON, "colon" }, { TOK_COMMA, "comma" }, { TOK_PERIOD, "period" }, { TOK_OPAREN, "oparen" }, { TOK_CPAREN, "cparen" },
		 { TOK_OARRAY, "oarray" }, { TOK_CARRAY, "carray" }, { TOK_PLUS, "plus" }, { TOK_MINUS, "minus" }, { TOK_MUL, "mul" },
		 { TOK_DIV, "div" }, { TOK_MOD, "mod" }, { TOK_EQ, "eq" }, { TOK_NEQ, "neq" }, { TOK_GT, "gt" }, { TOK_GTE, "gte" },
		 { TOK_LT, "lt" }, { TOK_LTE, "lte" }, { TOK_AND, "and" }, { TOK_OR, "or" }, { TOK_NOT, "not" }, { TOK_TILDE, "tilde" }, { TOK_ANDAND, "andand" },
		 { TOK_OROR, "oror" }, { TOK_QUESTION, "?" }, { 0, NULL } };
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
	if( c == '\n' ) incrementlinecount();
      } while( 1 );
      found = 1;
#if 0
    } else if( c == '#' ) {
      /* skip single line comment */
      do {
	c = fgetc( f );
	if( c == '\n' ) {
	  incrementlinecount();
	  break;
	}
      } while( 1 );
#endif
    } else {    
      found = 0;
      for( w = whitespacechars; *w; w++ ) {
	if( c == *w ) {
	  if( c == '\n' ) {
	    incrementlinecount();
	  }
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
      if( i >= (tok->len - 2)) {
	tok->len = (3 * tok->len) / 2;
	tok->val = realloc( tok->val, tok->len );
	p = tok->val + i;
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
  } else if( c == '?' ) {
    tok->type = TOK_QUESTION;
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
	if( c2 >= '0' && c2 <= '9' ) {
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
    tok->type = TOK_MOD;
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
  } else if( c == '\'' ) {
    c2 = fgetc( f );
    if( c2 == '\\' ) {
      c2 = fgetc( f );
      switch( c2 ) {
      case '\\':
	c2 = '\\';
	break;
      case 'n':
	c2 = '\n';
	break;
      case 't':
	c2 = '\t';
	break;
      case 'r':
	c2 = '\r';
	break;
      case '\'':
	c2 = '\'';
	break;
      default:
	break;
      }
    }
    tok->type = TOK_U32;
    tok->u32 = c2;
    c2 = fgetc( f );
    if( c2 != '\'' ) usage( "Unmatched Character quote" );    
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
static void processincludefile( char *path );
static uint32_t parseconstdefexpr( FILE *f );
static uint32_t parsesizeof( FILE *f );
static uint32_t parseoffsetof( FILE *f );

struct includepath {
  struct includepath *next;
  char *path;
};

/* parsed information */

/* global or local variable */
struct var {
  struct var *next;
  char name[FVM_MAX_NAME];
  
  var_t type;
  uint32_t arraylen;
  uint32_t address; /* address if global, stack offset if local */
  uint32_t offset;
  uint32_t size; /* size of the variable. u32 is 4 */
  struct record *record;
};

/* procedure parameter */
struct param {
  struct param *next;
  
  char name[FVM_MAX_NAME];
  var_t type;
  int isvar;
  uint32_t size;    /* size is 4 for everything */
  uint32_t offset;  /* stack offset */
};

struct proc {
  struct proc *next;
  char name[FVM_MAX_NAME];
  uint32_t address;
  struct param *params; 
  int nparams;
  struct var *locals;
  uint32_t localsize; /* total size of all locals */
  uint64_t siginfo; /* signature */

  int xcall;
  char modname[FVM_MAX_NAME];
};

struct label {
  struct label *next;
  char name[FVM_MAX_NAME];
  uint32_t address;
};

struct export {
  struct export *next;
  char name[FVM_MAX_NAME];
};

struct constvar {
  struct constvar *next;
  char name[FVM_MAX_NAME];
  var_t type;
  uint32_t address;
  char *val;
  int len;
};

struct constval {
  struct constval *next;
  char name[FVM_MAX_NAME];
  var_t type;
  char *val;
  int len;
};

struct syscall {
  struct syscall *next;
  
  char name[FVM_MAX_NAME];
  struct param *params;
  int nparams;
  uint16_t id;
  uint64_t siginfo;
};

struct record {
  struct record *next;

  char name[FVM_MAX_NAME];
  struct var *fields;
  int size;
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

  char progname[FVM_MAX_NAME];
  uint32_t progid, versid;

  uint32_t labelidx;
  struct label *labels;
  struct var *globals;
  struct proc *procs;
  struct export *exports;
  struct constvar *consts;
  struct constval *constvals;
  struct syscall *syscalls;
  struct record *records;
  
  struct token tok;
  uint32_t pc;
  uint32_t datasize;
  uint32_t textsize;

  char outpath[256];
  FILE *outfile;
  struct proc *currentproc;
  uint32_t stackoffset;  /* offset relative to last local var. */
  uint32_t linecount;
  char curfile[256];

  /* 
   * store label names for break and continue statements. 
   * if these are empty string then break/continue statements are invalid 
   */
  char brklbl[64];
  char contlbl[64];
} glob;

static void incrementlinecount( void ) {
  glob.linecount++;
}

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
    fvmc_printf( ";; PC=%04x SP=%04u %s:\n", l ? l->address : 0, glob.stackoffset, lname );
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
  v->address = glob.globals ? glob.globals->address + glob.globals->size : FVM_ADDR_DATA;
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

  fvmc_printf( ";; Adding global %s type=%u arraylen=%u address=%u\n", name, type, arraylen, v->address );
  
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
static struct proc *getxcall( char *modname, char *procname ) {
  struct proc *p;
  p = glob.procs;
  while( p ) {
    if( (strcasecmp( p->name, procname ) == 0) && (strcasecmp( p->modname, modname) == 0) ) return p;
    p = p->next;
  }
  return NULL;
}
static struct proc *addproc( char *name, struct param *params, int nparams, uint64_t siginfo ) {
  struct proc *p;
  
  if( glob.pass == 2 ) usage( "assert" );

  fvmc_printf( ";; %04x Add procedure %s\n", glob.pc, name );
  
  p = getproc( name );
  if( p ) usage( "Proc %s already exists", name );

  p = malloc( sizeof(*p) );
  memset( p, 0, sizeof(*p) );
  strcpy( p->name, name );
  p->address = glob.pc;
  p->params = params;
  p->nparams = nparams;
  p->siginfo = siginfo;
  
  p->next = glob.procs;
  glob.procs = p;
  return p;
}
static struct param *getparam( struct proc *proc, char *name ) {
  struct param *p;
  p = proc->params;
  while( p ) {
    if( strcasecmp( p->name, name ) == 0 ) return p;
    p = p->next;
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

  if( glob.pass == 2 ) return getlocal( proc, name );
  
  v = getglobal( name );
  if( v ) fvmc_printf( ";; Warning: local variable %s shadows existing global\n", name );
  
  v = getlocal( proc, name );
  if( v ) usage( "Local variable with name %s already exists", name );

  fvmc_printf( ";; add local %s type=%u arraylen=%u\n", name, type, arraylen );
  
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

  /* push all other locals further back in the stack */
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

  fvmc_printf( ";; Adding export %s\n", name );
    
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

  fvmc_printf( ";; Adding const %s\n", name );
  v = malloc( sizeof(*v) );
  strncpy( v->name, name, FVM_MAX_NAME - 1 );
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
  strncpy( v->name, name, FVM_MAX_NAME - 1 );
  v->type = type;
  v->val = val;
  v->len = len;
  v->next = glob.constvals;
  glob.constvals = v;

  return v;
}

static void addincludepath( char *path ) {
  struct includepath *p;
  p = malloc( sizeof(*p) );
  p->next = glob.includepaths;
  p->path = path;
  glob.includepaths = p;
}

static struct record *getrecord( char *name ) {
  struct record *r;
  r = glob.records;
  while( r ) {
    if( strcasecmp( r->name, name ) == 0 ) return r;
    r = r->next;
  }
  return NULL;
}

static struct var *getrecordfield( struct record *r, char *name ) {
  struct var *v;
  v = r->fields;
  while( v ) {
    if( strcasecmp( v->name, name ) == 0 ) return v;
    v = v->next;
  }
  return NULL;
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
    if( !tok ) fvmc_printf( ";; Unexpected end of file\n" );
    return 1;
  }
  return 0;
}

static void expectkeyword( FILE *f, char *name ) {
  if( !acceptkeyword( f, name ) ) usage( "Unexpected symbol %s - expected %s", glob.tok.val ? glob.tok.val : gettokname( glob.tok.type ), name );
}

static struct syscall *getsyscall( char *name ) {
  struct syscall *sc;
  sc = glob.syscalls;
  while( sc ) {
    if( strcasecmp( sc->name, name ) == 0 ) return sc;
    sc = sc->next;
  }
  return NULL;
}

static struct syscall *addsyscall( char *name, struct param *params, int nparams, uint64_t siginfo, uint32_t id ) {
  struct syscall *sc;
  sc = getsyscall( name );
  if( sc ) {
    if( sc->siginfo != siginfo ) usage( "Syscall \"%s\" already defined with different signature", name );
    if( sc->id != id ) usage( "Syscall \"%s\" already defined with different id", name );    
    return sc;
  }

  sc = malloc( sizeof(*sc) );
  memset( sc, 0, sizeof(*sc) );
  
  strcpy( sc->name, name );
  sc->id = id;
  sc->params = params;
  sc->nparams = nparams;
  sc->siginfo = siginfo;
  sc->next = glob.syscalls;
  glob.syscalls = sc;
  return sc;  
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
   { OP_BRZ, "BRZ", 2, -4 },
   { OP_LD8, "LD8", 0, 0 },
   { OP_ST8, "ST8", 0, -8 },
   { 0, NULL, 0, 0 }
  };
static struct opinfo *getopinfo( op_t op ) {
  int i;
  for( i = 0; opcodeinfo[i].name; i++ ) {
    if( opcodeinfo[i].op == op ) return &opcodeinfo[i];
  }
  return NULL;
}

#if 0
static void emitcomment( char *fmt, ... ) {
  va_list args;
  if( fvmc_debug ) {
    printf( ";; " );
    va_start( args, fmt );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
}
#endif

static void emitopcode( op_t op, void *data, int len ) {
  uint8_t u8;
  struct opinfo *info;

  info = getopinfo( op );
  if( !info ) usage( "Unknown opcode" );
  if( len != info->pcdata ) usage( "opcode %s data mismatch %u != %u", info->name, len, info->pcdata );
  
  if( glob.pass == 2 ) {
    if( len == 0 ) fvmc_printf( ";; PC=%04x SP=%04u %s\n", glob.pc, glob.stackoffset, info->name );  
    else if( len == 2 ) {
      uint16_t u16 = *((uint16_t *)data);
      fvmc_printf( ";; PC=%04x SP=%04u %s\t%u (%d) 0x%x\n", glob.pc, glob.stackoffset, info->name, (uint32_t)u16, (int32_t)(int16_t)u16, (uint32_t)u16 );
    } else if( len == 4 ) {
      uint32_t u32 = *((uint32_t *)data);
      fvmc_printf( ";; PC=%04x SP=%04u %s\t%u (%d) 0x%x\n", glob.pc, glob.stackoffset, info->name, u32, u32, u32 );
    }
  }
  
  u8 = op;
  if( glob.pass == 2 ) {
    fwrite( &u8, 1, 1, glob.outfile );
    fwrite( data, 1, info->pcdata, glob.outfile );
  }
  glob.pc += 1 + info->pcdata;
  glob.stackoffset += info->stackadjust;
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
static void emit_brz( uint16_t u ) {
  emitopcode( OP_BRZ, &u, 2 );
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
static void emit_ld8( void ) {
  emitopcode( OP_LD8, NULL, 0 );
}
static void emit_st8( void ) {
  emitopcode( OP_ST8, NULL, 0 );
}
static void emit_syscall( uint16_t u ) {
  emitopcode( OP_SYSCALL, &u, 2 );
}


static void emitdata( void *data, int len ) {
  if( glob.pass == 2 ) {
    fvmc_printf( ";; PC=%04x SP=%04u Const data len %u %.*s\n", glob.pc, glob.stackoffset, len, len, data );
    fwrite( data, 1, len, glob.outfile );
  }
  glob.pc += len;
}

/*
 * encode param info into a uint32: bits 0-23: 3 bits per param encoding type and isvar flag
 * bits 24-27: param count
 * bits 28-31: unused 
 */

/* --------------- */

static void parsevartype( FILE *f, var_t *type, uint32_t *arraylen, struct record **rec ) {
  struct record *r;
  r = NULL;
  
  if( glob.tok.type != TOK_NAME ) usage( "Unexpect symbol type %s - expected u32|string|opaque", gettokname( glob.tok.type ) );
  if( strcasecmp( glob.tok.val, "u32" ) == 0 ) *type = VAR_TYPE_U32;
  else if( strcasecmp( glob.tok.val, "int" ) == 0 ) *type = VAR_TYPE_U32;
  else if( strcasecmp( glob.tok.val, "integer" ) == 0 ) *type = VAR_TYPE_U32;    
  else if( strcasecmp( glob.tok.val, "string" ) == 0 ) *type = VAR_TYPE_STRING;
  else if( strcasecmp( glob.tok.val, "opaque" ) == 0 ) *type = VAR_TYPE_OPAQUE;
  else {
    r = getrecord( glob.tok.val );
    if( !r ) usage( "Unexpected var type %s", glob.tok.val );
    *type = VAR_TYPE_OPAQUE;
    *arraylen = r->size;
  }
  
  expecttok( f, TOK_NAME );
  if( accepttok( f, TOK_OARRAY ) ) {
    uint32_t u32 = parseconstdefexpr( f );
    *arraylen = (r ? r->size : 1) * u32;
    expecttok( f, TOK_CARRAY );
  } else {
    if( !r ) *arraylen = 0;
  }

  *rec = r;
}

static void parsevariableexpr( FILE *f, var_t *vartypep ) {
  struct var *v;
  var_t vartype;
  struct param *p;
  struct constvar *cv;
  struct constval *cl;
  
  v = getlocal( glob.currentproc, glob.tok.val );
  if( v ) {
    vartype = v->type;
    
    if( v->arraylen ) emit_leasp( v->offset + glob.stackoffset );
    else emit_ldsp( v->offset + glob.stackoffset );
  } else {
    v = getglobal( glob.tok.val );
    if( v ) {
      vartype = v->type;
      
      if( v->arraylen ) emit_ldi32( v->address );
      else {
	emit_ldi32( v->address );
	emit_ld();
      }
    } else {
      p = getparam( glob.currentproc, glob.tok.val );
      if( p ) {
	vartype = p->type;
	
	emit_ldsp( p->offset + glob.currentproc->localsize + glob.stackoffset );
	if( p->isvar ) {
	  emit_ld();
	}
      } else {
	cv = getconst( glob.tok.val );
	if( cv ) {
	  vartype = cv->type;
	  
	  if( cv->type == VAR_TYPE_U32 ) {
	    emit_ldi32( cv->address );
	    emit_ld();
	  } else if( cv->type == VAR_TYPE_STRING ) {
	    emit_ldi32( cv->address );
	  } else usage( "Bad const type" );
	  
	} else {
	  cl = getconstval( glob.tok.val );
	  if( !cl ) usage( "Variable %s does not name a known local, global, param, const or constval", glob.tok.val );
	  
	  vartype = cl->type;
	  
	  if( cl->type == VAR_TYPE_U32 ) {
	    emit_ldi32( *((uint32_t *)cl->val) );
	  } else if( cl->type == VAR_TYPE_STRING ) {
	    char lname[FVM_MAX_NAME];
	    struct label *l;
	    uint16_t startaddr;
	    uint32_t addr;
	    
	    getlabelname( "STRING", lname );
	    if( glob.pass == 1 ) l = NULL;
	    else {
	      l = getlabel( lname );
	      if( !l ) usage( "Failed to get label" );
	    }
	    addr = l ? l->address : 0;
	    emit_jmp( addr );
	    startaddr = glob.pc;
	    emitdata( glob.tok.val, strlen( cl->val ) + 1 );
	    addlabel( lname );
	    
	    emit_lea( startaddr - glob.pc - 3 ); /* LEA opcode uses 3 bytes */
	  }
	}
      }	  
    }
  }
  expecttok( f, TOK_NAME );
  
  *vartypep = vartype;  
}

static int parsebuiltinfn( FILE *f, var_t *vartypep ) {
  // TODO: parse builtn functions like sizeof, offset etc */
  if( acceptkeyword( f, "sizeof" ) ) {
    uint32_t u32;
    u32 = parsesizeof( f );
    emit_ldi32( u32 );
    return 1;
  }

  if( acceptkeyword( f, "offsetof" ) ) {
    uint32_t u32;
    u32 = parseoffsetof( f );
    emit_ldi32( u32 );
    return 1;
  }
  
  return 0;
}

static void parseexpr( FILE *f );
static void parseexpr2( FILE *f, int nobinaryops ) {
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
  } else if( accepttok( f, TOK_TILDE ) ) {
    /* ~expr */
    parseexpr( f );
    emit_not();
  } else if( accepttok( f, TOK_NOT ) ) {
    /* ! expr evaluates as expr ? 1 : 0 */
    char lname1[64], lname2[64];
    struct label *l;
    uint32_t addr1, addr2;
    
    getlabelname( NULL, lname1 );
    getlabelname( NULL, lname2 );
    l = getlabel( lname1 );
    addr1 = l ? l->address : 0;
    l = getlabel( lname2 );
    addr2 = l ? l->address : 0;

    parseexpr( f );
    emit_br( addr1 );
    emit_ldi32( 1 );
    emit_jmp( addr2 );
    addlabel( lname1 );
    emit_ldi32( 0 );
    addlabel( lname2 );
    glob.stackoffset -= 4;
  } else if( accepttok( f, TOK_MINUS ) ) {
    /* - expr */
    /* TODO: could improve this by adding a negation opcode, but for now we just subtract from 0 */
    emit_ldi32( 0 ); 
    parseexpr( f );
    emit_sub();
  } else if( glob.tok.type == TOK_U32 ) {
    emit_ldi32( glob.tok.u32 );
    expecttok( f, TOK_U32 );
  } else if( glob.tok.type == TOK_STRING ) {
    char lname[FVM_MAX_NAME];
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

    emit_lea( startaddr - glob.pc - 3 ); /* the emit_lea opcode takes 3 bytes */

    expecttok( f, TOK_STRING );
  } else if( accepttok( f, TOK_AND ) ) {
    /* address of operator */
    struct var *v;
    struct param *p;
    struct constvar *cv;
        
    if( glob.tok.type != TOK_NAME )
      usage( "Address operator (&) expects variable name not %s",
	     gettokname( glob.tok.type ) );

    v = getlocal( glob.currentproc, glob.tok.val );
    if( v ) {
      emit_leasp( v->offset + glob.stackoffset );
    } else {
      v = getglobal( glob.tok.val );
      if( v ) {
	emit_ldi32( v->address );
      } else {
	p = getparam( glob.currentproc, glob.tok.val );
	if( p ) {
	  emit_leasp( p->offset + glob.currentproc->localsize + glob.stackoffset );
	} else {
	  cv = getconst( glob.tok.val );
	  if( cv ) {
	    emit_ldi32( cv->address );
	  } else {
	    usage( "Unknown variable %s", glob.tok.val );
	  }
	}
      }
    }
    expecttok( f, TOK_NAME );
  } else if( accepttok( f, TOK_MUL ) ) {
    /* dereference operator: *expr */
    parseexpr( f );
    emit_ld();
  } else if( glob.tok.type == TOK_NAME ) {
    var_t vartype = VAR_TYPE_U32;

    // parse builtin function calls e.g. sizeof,offsetof etc */
    if( parsebuiltinfn( f, &vartype ) ) {
    } else {
      parsevariableexpr( f, &vartype );
    }    

    if( accepttok( f, TOK_OARRAY ) ) {
      /* name[expr] */
      parseexpr( f );
      if( vartype == VAR_TYPE_U32 ) {
	emit_ldi32( 4 );
	emit_mul();
	emit_add();
	emit_ld();
      } else {
	emit_add();
	emit_ld8();
      }
      expecttok( f, TOK_CARRAY );
    } else if( accepttok( f, TOK_PERIOD ) ) {
      usage( "record field not implementd" );
    }
  } else usage( "Bad expr %s (%s)", gettokname( glob.tok.type ), glob.tok.val ? glob.tok.val : "" );

  if( nobinaryops ) return;
  
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
    case TOK_ANDAND:
      {
	char lname1[64], lname2[64];
	uint32_t addr1, addr2;
	struct label *l;

	expecttok( f, TOK_ANDAND );
	
	/* expr && expr */
	/* if the value on stack is true then evaluate second expression. otherwise 0 */
	getlabelname( "ANDAND", lname1 );
	l = getlabel( lname1 );
	addr1 = l ? l->address : 0;
	getlabelname( "ANDAND2", lname2 );
	l = getlabel( lname2 );
	addr2 = l ? l->address : 0;
	
	emit_br( addr1 ); /* if true then evaluate other expression */
	emit_ldi32( 0 ); /* not true - push false */
	emit_jmp( addr2 ); /* go to end */
	addlabel( lname1 );

	/* both parseexpr() functions have adjusted the stack, but only one of them 
	 * will be executed by the runtime. so we end up with an incorrect stack adjustment here.
	 * to work around this we modify it by hand */
	glob.stackoffset -= 4;
	
	parseexpr( f );
	addlabel( lname2 );
      }
      break;
    case TOK_OROR:
      {
	char lname1[64], lname2[64];
	uint32_t addr1, addr2;
	struct label *l;

	expecttok( f, TOK_OROR );
	
	/* expr1 || expr */
	/* if expr1 is true jump to end, otherwise evaluate expr */
	getlabelname( "OROR", lname1 );
	l = getlabel( lname1 );
	addr1 = l ? l->address : 0;
	getlabelname( "OROR", lname2 );
	l = getlabel( lname2 );
	addr2 = l ? l->address : 0;

	emit_br( addr1 ); /* if true, push 1 */
	parseexpr( f ); /* first expr was false so evaluate 2nd */
	emit_jmp( addr2 );
	addlabel( lname1 );

	/* both parseexpr() functions have adjusted the stack, but only one of them 
	 * will be executed by the runtime. so we end up with an incorrect stack adjustment here.
	 * to work around this we modify it by hand */
	glob.stackoffset -= 4;

	emit_ldi32( 1 ); /* push true */
	addlabel( lname2 );
      }
      break;
    case TOK_QUESTION:
      {
      /* expr1 ? expr2 : expr3 */
	char lname1[64], lname2[64];
	struct label *l;
	uint32_t addr1, addr2;

	expecttok( f, TOK_QUESTION );
	
	getlabelname( "Q", lname1 );
	getlabelname( "Q", lname2 );
	l = getlabel( lname1 );
	addr1 = l ? l->address : 0;
	l = getlabel( lname2 );	
	addr2 = l ? l->address : 0;

	emit_brz( addr1 ); /* if expr evaluates to false jump */
	parseexpr( f );
	expecttok( f, TOK_COLON );
	emit_jmp( addr2 );
	addlabel( lname1 );

	/* both parseexpr() functions have adjusted the stack, but only one of them 
	 * will be executed by the runtime. so we end up with an incorrect stack adjustment here.
	 * to work around this we modify it by hand */
	glob.stackoffset -= 4;
	
	parseexpr( f );
	addlabel( lname2 );

      }
      break;
    default:
      break;
    }

  }
  
}
      
static void parseexpr( FILE *f ) {
  parseexpr2( f, 0 );
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
    char procname[FVM_MAX_NAME], modname[FVM_MAX_NAME];
    struct proc *proc = NULL;
    struct syscall *sc = NULL;
    struct param *params;
    int nparams, xcall;

    xcall = 0;
    /* call|syscall procname(args...) */
    if( glob.tok.type != TOK_NAME ) usage( "Expected procname" );
    strcpy( procname, glob.tok.val );
    expecttok( f, TOK_NAME );
    if( accepttok( f, TOK_DIV ) ) {
      if( glob.tok.type != TOK_NAME ) usage( "Expected procname" );
      strcpy( modname, procname );
      strcpy( procname, glob.tok.val );
      expecttok( f, TOK_NAME );
      xcall = 1;
    }
    
    if( kw == 1 ) {
      /* call - lookup proc */
      proc = xcall ? getxcall( modname, procname ) : getproc( procname );
      if( !proc ) usage( "Unknown proc %s", procname );
      addr = proc->address;
      nparams = proc->nparams;
      params = proc->params;
    } else {
      /* syscall - lookup syscall */
      sc = getsyscall( procname );
      if( !sc ) usage( "Unknown syscall %s", procname );
      addr = sc->id;
      nparams = sc->nparams;
      params = sc->params;
    }

    
    expecttok( f, TOK_OPAREN );
    ipar = 0;
    while( glob.tok.type != TOK_CPAREN ) {
      if( (!params) || (ipar > nparams) ) usage( "Too many params supplied to proc %s", procname );

      if( params->isvar ) {
	/* var type param requires a variable name */
	if( (glob.tok.type == TOK_U32) && (glob.tok.u32 == 0) ) {
	  /* push constant address 0 i.e. don't pass a pointer */
	  emit_ldi32( 0 );
	  expecttok( f, TOK_U32 );
	} else if( glob.tok.type != TOK_NAME ) usage( "Param %s expected a var name", params->name );
	else {
	  v = getlocal( glob.currentproc, glob.tok.val );
	  if( v ) {
	    /* local var - push address */
	    emit_leasp( v->offset + glob.stackoffset );
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
	}
	
      } else {
	parseexpr( f );
      }
      
      ipar++;
      params = params->next;
      if( !accepttok( f, TOK_COMMA ) ) break;
    }
    expecttok( f, TOK_CPAREN );      

    if( glob.pass == 2 && (ipar < nparams) ) usage( "Insufficient params supplied to proc %s", procname );
    
    if( kw == 1 ) {
      if( proc->xcall ) {
	struct constvar *cv;
	cv = getconst( proc->modname );
	if( !cv ) usage( "Failed to get modname const" );
	emit_ldi32( cv->address );
	cv = getconst( proc->name );
	if( !cv ) usage( "Failed to get procname const" );
	emit_ldi32( cv->address );
	nparams += 2;
	emit_syscall( 0xffff );
      } else {
	emit_call( addr );
      }
    } else emit_syscall( addr );

    /* remove args from stack */
    emit_subsp( nparams * 4 );
    glob.stackoffset -= nparams * 4;

  } else if( acceptkeyword( f, "begin" ) ) {
    /* begin statement statement ... end */
    while( !acceptkeyword( f, "end") ) {
      if( !parsestatement( f ) ) usage( "Expected statement not %s (%s)", gettokname( glob.tok.type ), glob.tok.val ? glob.tok.val : "" );
      expecttok( f, TOK_SEMICOLON );
    }
  } else if( acceptkeyword( f, "if" ) ) {
    /* if expr then statement [ else statement ] */
    uint16_t elseaddr, endaddr;
    struct label *l;
    char lnameelse[FVM_MAX_NAME], lnameend[FVM_MAX_NAME];

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
    emit_brz( elseaddr );
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
    struct label *l;
    uint32_t doaddr;
    char dolbl[64], brklbl[64], contlbl[64];

    /* copy current break/continue labels */
    strcpy( brklbl, glob.brklbl );
    strcpy( contlbl, glob.contlbl );

    /* allocate label names */
    getlabelname( "DO", dolbl );
    getlabelname( "BRK", glob.brklbl );
    getlabelname( "CONT", glob.contlbl );

    /* set start address */
    l = addlabel( dolbl );
    doaddr = l->address;
    
    parsestatement( f );

    expectkeyword( f, "while" );
    addlabel( glob.contlbl );    /* continue statement takes us to the test */
    parseexpr( f );
    emit_br( doaddr );
    addlabel( glob.brklbl );
    
    /* restore break/cont labels */
    strcpy( glob.brklbl, brklbl );
    strcpy( glob.contlbl, contlbl );
  } else if( acceptkeyword( f, "while" ) ) {
    /* while expr do statement */
    struct label *l;
    uint16_t whileaddr, doaddr;
    char whilelbl[FVM_MAX_NAME], dolbl[FVM_MAX_NAME];
    char brklbl[64], contlbl[64];

    /* save break/continue labels */
    strcpy( brklbl, glob.brklbl );
    strcpy( contlbl, glob.contlbl );
    
    getlabelname( "WHILE", whilelbl );
    getlabelname( "DO", dolbl );

    /* set break/continue labels */
    strcpy( glob.brklbl, dolbl );
    strcpy( glob.contlbl, whilelbl );
    
    if( glob.pass == 1 ) {
      whileaddr = 0;
      doaddr = 0;
    } else {
      l = getlabel( whilelbl );
      if( !l ) usage( "Failed to get label" );
      whileaddr = l->address;
      l = getlabel( dolbl );
      if( !l ) usage( "Failed to get label" );
      doaddr = l->address;
    }
    
    addlabel( whilelbl );
    parseexpr( f );
    emit_brz( doaddr );
    expectkeyword( f, "Do" );
    parsestatement( f );
    emit_jmp( whileaddr );
    addlabel( dolbl );

    /* restore break/cont labels */
    strcpy( glob.brklbl, brklbl );
    strcpy( glob.contlbl, contlbl );
  } else if( acceptkeyword( f, "break" ) ) {
    struct label *l;
    if( !glob.brklbl[0] ) usage( "Break statement invalid - not in a loop context" );
    l = getlabel( glob.brklbl );
    emit_jmp( l ? l->address : 0 );
  } else if( acceptkeyword( f, "continue" ) ) {
    struct label *l;
    if( !glob.contlbl[0] ) usage( "Continue statement invalid - not in a loop context" );
    l = getlabel( glob.contlbl );
    emit_jmp( l ? l->address : 0 );    
  } else if( acceptkeyword( f, "return" ) ) {
    if( glob.currentproc->localsize ) {
      emit_subsp( glob.currentproc->localsize );
    }
    emit_ret();
  } else if( acceptkeyword( f, "goto" ) ) {
    /* goto label */
    struct label *l;
    uint16_t addr;
    
    if( glob.tok.type != TOK_NAME ) usage( "Expected label identifier" );
    if( glob.pass == 1 ) {
      addr = 0;
    } else {
      char lname[64];
      /* prefix the label name with current procname to enforce jumping only within current proc */      
      snprintf( lname, 63, "%.*s%.*s", 30, glob.currentproc->name, 30, glob.tok.val );
      l = getlabel( lname );
      if( !l ) usage( "Unknown label %s", glob.tok.val );
      addr = l->address;
    }
    
    emit_jmp( addr );
    expecttok( f, TOK_NAME );
  } else if( accepttok( f, TOK_MUL ) ) {
    /* assign *varname = expr */
    //    struct var *v;
    //struct param *p;

    parseexpr2( f, 1 );
    expecttok( f, TOK_EQ );
    parseexpr( f );
    emit_st();

#if 0    
    if( glob.tok.type != TOK_NAME ) usage( "Expected varname" );
    
    if( (v = getlocal( glob.currentproc, glob.tok.val )) ) {
      emit_ldsp( v->offset + glob.stackoffset ); /* get value of variable */
      expecttok( f, TOK_NAME );
      expecttok( f, TOK_EQ );
      parseexpr( f );      
      emit_st();
            
    } else if( (v = getglobal( glob.tok.val )) ) {
      emit_ldi32( v->address );
      emit_ld(); /* get value of variable */
      
      expecttok( f, TOK_NAME );
      expecttok( f, TOK_EQ );
      parseexpr( f );
      emit_st();
	
    } else if( (p = getparam( glob.currentproc, glob.tok.val )) ) {
      if( p->isvar ) {
	emit_ldsp( p->offset + glob.currentproc->localsize + glob.stackoffset ); /* var param so value on stack is address, just load it */
      } else {
	emit_leasp( p->offset + glob.currentproc->localsize +  glob.stackoffset ); /* get address of param */
      }
      emit_ld();
      
      expecttok( f, TOK_NAME );
      expecttok( f, TOK_EQ );
      parseexpr( f );

      /* store value */
      emit_st();
    } else usage( "Unknown variable %s", glob.tok.val );
#endif
    
  } else if( glob.tok.type == TOK_NAME ) {
    /* varname = expr i.e. an assignment operation */
    struct var *v;
    struct param *p;
    int setarray8 = 0;
    
    if( glob.tok.val[strlen( glob.tok.val ) - 1] == ':' ) {
      char lname[64];
      
      glob.tok.val[strlen( glob.tok.val ) - 1] = '\0';

      /* prefix the label name with current procname to enforce jumping only within current proc */
      snprintf( lname, 63, "%.*s%.*s", 30, glob.currentproc->name, 30, glob.tok.val );
      addlabel( lname );
      free( glob.tok.val );
      memset( &glob.tok, 0, sizeof(glob.tok) );
      glob.tok.type = TOK_SEMICOLON;
      
      return 1;
    }

    /* TODO: support assigning record fields */
    
    v = getlocal( glob.currentproc, glob.tok.val );
    if( v ) {
      expecttok( f, TOK_NAME );
      if( accepttok( f, TOK_OARRAY ) ) {
	/* name[expr] = expr */
	setarray8 = 1;
	parseexpr( f );
	if( v->type == VAR_TYPE_U32 ) {
	  emit_ldi32( 4 );
	  emit_mul();
	  emit_leasp( v->offset + glob.stackoffset );
	  emit_add();
	} else {
	  emit_leasp( v->offset + glob.stackoffset );
	  emit_add();
	}
	
	expecttok( f, TOK_CARRAY );
      } else if( accepttok( f, TOK_PERIOD ) ) {
	usage( "assigning record field not implemented" );
      }
      
      expecttok( f, TOK_EQ );
      parseexpr( f );

      if( setarray8 ) {
	if( v->type == VAR_TYPE_U32 ) {
	  emit_st();
	} else {
	  emit_st8();
	}
      } else {
	emit_stsp( v->offset + glob.stackoffset );
      }
            
    } else {
      v = getglobal( glob.tok.val );
      if( v ) {
	emit_ldi32( v->address );
	
	expecttok( f, TOK_NAME );
	if( accepttok( f, TOK_OARRAY ) ) {
	  /* name[expr] = expr */
	  parseexpr( f );
	  if( v->type == VAR_TYPE_U32 ) {
	    emit_ldi32( 4 );
	    emit_mul();
	    emit_add();
	  } else {
	    setarray8 = 1;	    
	    emit_add();
	  }
	  expecttok( f, TOK_CARRAY );
	} else if( accepttok( f, TOK_PERIOD ) ) {
	  usage( "assigning record field not implemented" );
	}
	
	expecttok( f, TOK_EQ );
	parseexpr( f );

	if( setarray8 ) {
	  emit_st8();
	} else {
	  emit_st();
	}
	
      } else {
	p = getparam( glob.currentproc, glob.tok.val );
	if( !p ) return 0;

	if( p->isvar ) {
	  emit_ldsp( p->offset + glob.currentproc->localsize + glob.stackoffset ); /* var param so value on stack is address, just load it */
	} else {
	  emit_leasp( p->offset + glob.currentproc->localsize +  glob.stackoffset ); /* get address of param */
	}
	
	expecttok( f, TOK_NAME );
	if( accepttok( f, TOK_OARRAY ) ) {
	  /* name[expr] = expr */
	  emit_ld();
	  
	  parseexpr( f );
	  if( p->type == VAR_TYPE_U32 ) {
	    emit_ldi32( 4 );
	    emit_mul();
	    emit_add();
	  } else {
	    setarray8 = 1;
	    emit_add();
	  }
	  
	  expecttok( f, TOK_CARRAY );
	} else if( accepttok( f, TOK_PERIOD ) ) {
	  usage( "assigning record field not implemented" );
	}
	
	expecttok( f, TOK_EQ );
	parseexpr( f );

	/* store value */
	if( setarray8 ) {
	  emit_st8();
	} else {
	  emit_st();
	}
      }
    }

  } else return 0;

  return 1;
}

static void parseproceduresig( FILE *f, struct param **params, int *nparams, uint64_t *siginfop ) {
  /* parse procedure */
  int nparam, isvar;
  uint32_t arraylen;
  struct param *p, *np, *tmpp;
  uint64_t siginfo;
  struct record *r;
  
  nparam = 0;
  siginfo = 0;
  np = NULL;
  *params = NULL;
  
  expecttok( f, TOK_OPAREN );
  /* parse params */
  while( glob.tok.type != TOK_CPAREN ) {
    /* [var] name : type */
    isvar = 0;
    if( acceptkeyword( f, "var" ) ) {
      isvar = 1;
    }

    if( glob.tok.type != TOK_NAME ) usage( "Expected parameter name not %s", gettokname( glob.tok.type ) );
    /* check for name clash */
    {
      struct param *p2 = *params;
      while( p2 ) {
	if( strcasecmp( p2->name, glob.tok.val ) == 0 ) usage( "Param name %s already exists", glob.tok.val );
	p2 = p2->next;
      }
    }
    if( getglobal( glob.tok.val ) ) usage( "Param %s name clash with global", glob.tok.val );

    p = malloc( sizeof(*p) );
    memset( p, 0, sizeof(*p) );
    
    strncpy( p->name, glob.tok.val, FVM_MAX_NAME - 1 );
    p->isvar = isvar;
    expecttok( f, TOK_NAME );
    expecttok( f, TOK_COLON );
    parsevartype( f, &p->type, &arraylen, &r );
    if( r ) usage( "records not allowed in proc params" );
    if( arraylen ) usage( "array vars not allowed in proc params" );
    if( nparam <= FVM_MAX_PARAM ) siginfo |= ((p->type | (p->isvar ? 4 : 0)) << (nparam * 3));

    /* push all previous params back in the stack */
    tmpp = *params;
    while( tmpp ) {
      tmpp->offset += 4;
      tmpp = tmpp->next;
    }
    /* last parameter has offset 8 (4 for the return address, 4 for the parameter itsefl) */
    p->offset = 8;      
    
    if( np ) np->next = p;
    else *params = p;
    np = p;
    
    nparam++;
    
    if( !accepttok( f, TOK_COMMA ) ) break;
  }
  expecttok( f, TOK_CPAREN );
  siginfo |= ((uint64_t)nparam) << 57;

  *nparams = nparam;
  *siginfop = siginfo;
  return; 
}

static void parsedeclaration( FILE *f ) {
  char procname[FVM_MAX_NAME], modname[FVM_MAX_NAME];
  struct param *params;
  int nparams, xcall;
  uint64_t siginfo;
  struct proc *proc;

  if( acceptkeyword( f, "procedure" ) ) {
    /* declare procedure name(...) */
    /* declare procedure modnmae/procname(...) */
    xcall = 0;
    if( glob.tok.type != TOK_NAME ) usage( "Expected procname not %s", gettokname( glob.tok.type ) );
    strcpy( procname, glob.tok.val );
    expecttok( f, TOK_NAME );
    if( accepttok( f, TOK_DIV ) ) {
      strcpy( modname, procname );
      if( glob.tok.type != TOK_NAME ) usage( "Expected procname not %s", gettokname( glob.tok.type ) );
      strcpy( procname, glob.tok.val );
      xcall = 1;

      addconst( modname, VAR_TYPE_STRING, strdup( modname ), strlen( modname ) + 1 );
      addconst( procname, VAR_TYPE_STRING, strdup( procname ), strlen( procname ) + 1 );
      expecttok( f, TOK_NAME );      
    }
    
    parseproceduresig( f, &params, &nparams, &siginfo );
    if( glob.pass == 1 ) {
      proc = getproc( procname );
      if( proc ) {
	if( proc->siginfo != siginfo ) usage( "Declaration does not match definition for %s", procname );
      } else {
	proc = addproc( procname, params, nparams, siginfo );
	proc->address = 0;
	if( xcall ) {
	  proc->xcall = 1;
	  strcpy( proc->modname, modname );
	}
      }
    }
  } else if( acceptkeyword( f, "const" ) ) {
    /* declare const var name : type */
    var_t type;
    char name[FVM_MAX_NAME];

    expectkeyword( f, "var" );
    if( glob.tok.type != TOK_NAME ) usage( "Expected constant name" );
    strncpy( name, glob.tok.val, FVM_MAX_NAME - 1 );
    expecttok( f, TOK_NAME );
	
    expecttok( f, TOK_COLON );
    if( acceptkeyword( f, "u32" ) ) {
      type = VAR_TYPE_U32;
    } else if( acceptkeyword( f, "int" ) ) {
      type = VAR_TYPE_U32;
    } else if( acceptkeyword( f, "integer" ) ) {
      type = VAR_TYPE_U32;      
    } else if( acceptkeyword( f, "string" ) ) {
      type = VAR_TYPE_STRING;
    } else usage( "Invalid constant type" );

    if( glob.pass == 1 ) {
      struct constvar *cv = getconst( name );
      if( cv ) {
	if( cv->type != type ) usage( "Constant type mismatch for %s", name );
      } else {
	cv = addconst( name, type, NULL, 0 );
	cv->address = 0;
      }
    }
  } else if( acceptkeyword( f, "syscall" ) ) {
    /* declare syscall name(args) : id; */
    if( glob.tok.type != TOK_NAME ) usage( "Expected procname not %s", gettokname( glob.tok.type ) );
    strcpy( procname, glob.tok.val );
    expecttok( f, TOK_NAME );
    
    parseproceduresig( f, &params, &nparams, &siginfo );
    expecttok( f, TOK_COLON );
    if( glob.tok.type != TOK_U32 ) usage( "Expected syscall id" );
    addsyscall( procname, params, nparams, siginfo, glob.tok.u32 );
    expecttok( f, TOK_U32 );
  } else usage( "Invalid declaration %s", glob.tok.val );
      
  expecttok( f, TOK_SEMICOLON );  
}

static uint32_t parsesizeof( FILE *f ) {
  char name[FVM_MAX_NAME];
  struct var *v;
  struct param *param;
  struct constvar *cvar;
  struct record *record;
  uint32_t val;
  
  val = 0;
  
  /* Sizeof(identifier) where identifier names a record type or variable (local,global,const) */
  expecttok( f, TOK_OPAREN );
  if( glob.tok.type != TOK_NAME ) usage( "SizeOf expected identifier" );
  strncpy( name, glob.tok.val, FVM_MAX_NAME - 1 );
  
  v = getlocal( glob.currentproc, name );
  if( v ) val = v->size;
  else {
    param = getparam( glob.currentproc, name );
    if( param ) val = param->size;
    else {
      v = getglobal( name );
      if( v ) val = v->size;
      else {
	cvar = getconst( name );
	if( cvar ) val = cvar->len;
	else {
	  record = getrecord( name );
	  if( record ) val = record->size;
	  else if( accepttok( f, TOK_PERIOD ) ) {
	    /* record.field */
	    if( glob.tok.type != TOK_NAME ) usage( "Expected record.field" );
	    record = getrecord( name );
	    if( !record ) usage( "Unknown record %s", name );
	    v = getrecordfield( record, glob.tok.val );
	    if( !v ) usage( "Unknown field %s.%s", name, glob.tok.val );
	    val = v->size;
	    expecttok( f, TOK_NAME );
	  } else usage( "Unknown identifier %s", name );
	}
      }
    }
  }
  
  expecttok( f, TOK_NAME );
  expecttok( f, TOK_CPAREN );
  
  return val;
}

static uint32_t parseoffsetof( FILE *f ) {
  /* OffsetOf(recordname.fieldname) */
  struct record *record;
  struct var *field;
  uint32_t val;
  
  expecttok( f, TOK_OPAREN );
  if( glob.tok.type != TOK_NAME ) usage( "Expected record name" );
  
  record = getrecord( glob.tok.val );
  if( !record ) usage( "Unknown record %s", glob.tok.val );
  
  expecttok( f, TOK_NAME );
  expecttok( f, TOK_PERIOD );
  if( glob.tok.type != TOK_NAME ) usage( "Expected record field name" );
  
  field = getrecordfield( record, glob.tok.val );
  if( !field ) usage( "Unknown field %s", glob.tok.val );
  val = field->offset;
  
  expecttok( f, TOK_NAME );
  expecttok( f, TOK_CPAREN );

  return val;
}

static uint32_t parseconstexprval( FILE *f ) {
  struct constval *cv;
  uint32_t val;
  
  switch( glob.tok.type ) {
  case TOK_U32:
    val = glob.tok.u32;
    expecttok( f, TOK_U32 );
    return val;
  case TOK_NAME:
    if( acceptkeyword( f, "sizeof" ) ) {
      val = parsesizeof( f );
      return val;
    } else if( acceptkeyword( f, "offsetof" ) ) {
      val = parseoffsetof( f );
      return val;
    }
    
    cv = getconstval( glob.tok.val );
    if( !cv ) usage( "Unknown const %s", glob.tok.val );
    if( cv->len != 4 ) usage( "Bad const %s", glob.tok.val );
    val = *((uint32_t *)cv->val);
    expecttok( f, TOK_NAME );
    return val;
    break;
  case TOK_OPAREN:
    accepttok( f, glob.tok.type );
    val = parseconstdefexpr( f );
    expecttok( f, TOK_CPAREN );
    return val;
    break;
  default:
    usage( "Unexpected const expr" );
  }
  return 0;
}

static uint32_t parseconstdefexpr( FILE *f ) {
  /* u32 | name where name names another getconst 
   * (expr)
   * expr || expr [+ - * / % & | ^ >> <<] expr 
   */
  uint32_t val;

  val = parseconstexprval( f );
  while( 1 ) {
    switch( glob.tok.type ) {
    case TOK_PLUS:
      accepttok( f, glob.tok.type );      
      val += parseconstdefexpr( f );
      break;
    case TOK_MINUS:
      accepttok( f, glob.tok.type );
      val -= parseconstdefexpr( f );      
      break;
    case TOK_MUL:
      accepttok( f, glob.tok.type );
      val *= parseconstdefexpr( f );      
      break;
    case TOK_DIV:
      accepttok( f, glob.tok.type );
      val /= parseconstdefexpr( f );      
      break;
    case TOK_MOD:
      accepttok( f, glob.tok.type );
      val %= parseconstdefexpr( f );      
      break;
    case TOK_AND:
      accepttok( f, glob.tok.type );
      val &= parseconstdefexpr( f );      
      break;
    case TOK_OR:
      accepttok( f, glob.tok.type );
      val |= parseconstdefexpr( f );      
      break;
    case TOK_XOR:
      accepttok( f, glob.tok.type );
      val ^= parseconstdefexpr( f );      
      break;
    case TOK_SHL:
      accepttok( f, glob.tok.type );
      val <<= parseconstdefexpr( f );
      break;
    case TOK_SHR:
      accepttok( f, glob.tok.type );
      val >>= parseconstdefexpr( f );
      break;
    default:
      goto done;
    }
   
  }

 done:
  return val;
}

static void parseconstdef( FILE *f ) {
  /* const name = value */
  char cname[FVM_MAX_NAME];
  void *ptr;
  uint32_t u32;
  
  if( glob.tok.type != TOK_NAME ) usage( "const expects name not %s", gettokname( glob.tok.type ) );
  strncpy( cname, glob.tok.val, FVM_MAX_NAME - 1 );
  expecttok( f, TOK_NAME );
  expecttok( f, TOK_EQ );
  switch( glob.tok.type ) {
  case TOK_STRING:
    addconstval( cname, VAR_TYPE_STRING, strdup( glob.tok.val ), strlen( glob.tok.val ) + 1 );
    expecttok( f, TOK_STRING );      
    break;
  default:
    ptr = malloc( 4 );
    u32 = parseconstdefexpr( f );
    *((uint32_t *)ptr) = u32;
    addconstval( cname, VAR_TYPE_U32, ptr, 4 );
    break;
  }
  expecttok( f, TOK_SEMICOLON );  
}

static void parseprocedure( FILE *f ) {
  /* parse procedure */
  char name[FVM_MAX_NAME];
  struct param *params;
  int nparams;
  uint64_t siginfo;
  struct proc *proc;
  var_t vartype;
  uint32_t arraylen;
  struct record *r;
  struct var *local;
  
  /* reset stackoffset at start of new procedure */
  if( glob.stackoffset != 0 ) printf( "Invalid stack offset %04x?\n", glob.stackoffset );
  glob.stackoffset = 0;

  if( glob.tok.type != TOK_NAME ) usage( "Expected procname not %s", gettokname( glob.tok.type ) );
  strcpy( name, glob.tok.val );
  expecttok( f, TOK_NAME );

  parseproceduresig( f, &params, &nparams, &siginfo );
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
    struct label *namelist, *nl, *nextnl, *nlp;

    namelist = NULL;
    if( glob.tok.type != TOK_NAME ) usage( "Expected var name not %s", gettokname( glob.tok.type ) );
    nl = malloc( sizeof(*nl) );
    strncpy( nl->name, glob.tok.val, FVM_MAX_NAME - 1 );
    nl->next = namelist;
    namelist = nl;
    nlp = nl;
    expecttok( f, TOK_NAME );
    while( accepttok( f, TOK_COMMA ) ) {
      if( glob.tok.type != TOK_NAME ) usage( "Expected var name not %s", gettokname( glob.tok.type ) );
      nl = malloc( sizeof(*nl) );
      memset( nl, 0, sizeof(*nl) );
      strncpy( nl->name, glob.tok.val, FVM_MAX_NAME - 1 );
      nlp->next = nl;
      nlp = nl;
      expecttok( f, TOK_NAME );
    }
    
    expecttok( f, TOK_COLON );
    parsevartype( f, &vartype, &arraylen, &r );

    nl = namelist;
    while( nl ) {
      nextnl = nl->next;
      local = addlocal( proc, nl->name, vartype, arraylen );
      local->record = r;
      free( nl );
      nl = nextnl;
    }
    
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
  fvmc_printf( ";; ------------------------\n\n" );
        
}

static void parserecord( FILE *f ) {
  /* record <name> = fieldname : type; ... end; */
  struct record *r;
  struct var *v, *vp;
  
  r = malloc( sizeof(*r) );
  memset( r, 0, sizeof(*r) );
  vp = NULL;
  
  if( glob.tok.type != TOK_NAME ) usage( "Unexpected symbol %s - expected record name", gettokname( glob.tok.type ) );
  strncpy( r->name, glob.tok.val, sizeof(r->name) - 1 );
  expecttok( f, TOK_NAME );
  expecttok( f, TOK_EQ );
  while( !acceptkeyword( f, "end" ) ) {
    v = malloc( sizeof(*v) );
    memset( v, 0, sizeof(*v) );
    if( glob.tok.type != TOK_NAME ) usage( "Unexpected symbol %s - expected field name", gettokname( glob.tok.type ) );
    strncpy( v->name, glob.tok.val, sizeof(v->name) - 1 );
    expecttok( f, TOK_NAME );
    expecttok( f, TOK_COLON );
    if( glob.tok.type != TOK_NAME ) usage( "Unexpected symbol %s - expected field type", gettokname( glob.tok.type ) );
    parsevartype( f, &v->type, &v->arraylen, &v->record );

    if( vp ) v->offset = vp->offset + vp->size;
    else v->offset = 0;
    if( v->arraylen ) v->size = (v->type == VAR_TYPE_U32 ? 4 : 1) * v->arraylen;
    else v->size = 4;
    if( v->size % 4 ) v->size += 4 - (v->size % 4);
    
    if( glob.pass == 2 ) free( v );
    else {
      if( vp ) vp->next = v;
      else r->fields = v;
      vp = v;
    }
    
    expecttok( f, TOK_SEMICOLON );
  }
  expecttok( f, TOK_SEMICOLON );

  r->size = vp ? vp->offset + vp->size : 0;
  
  if( glob.pass == 2 ) free( r );
  else {
    r->next = glob.records;
    glob.records = r;
  }
}


static void parsefile( FILE *f ) {
  struct token *tok;

  glob.linecount = 1;
  tok = nexttok( f );
  if( !tok ) {
    fvmc_printf( ";; Empty file\n" );
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
      parseconstdef( f );
    } else if( acceptkeyword( f, "declare" ) ) {
      parsedeclaration( f );
    } else if( acceptkeyword( f, "record" ) ) {
      parserecord( f );
    } else if( acceptkeyword( f, "include" ) ) {
      /* include string */
      uint32_t linecount;
      char curfile[256];
      
      if( glob.tok.type != TOK_STRING ) usage( "expected include string" );

      linecount = glob.linecount;
      strcpy( curfile, glob.curfile );
      glob.linecount = 1;
      strcpy( glob.curfile, glob.tok.val );
      processincludefile( glob.tok.val );
      glob.linecount = linecount;
      strcpy( glob.curfile, curfile );
      
      glob.tok.type = TOK_STRING;
      expecttok( f, TOK_STRING );
      expecttok( f, TOK_SEMICOLON );
    } else break;
  }
  
  /* parse data segment - i.e. global variables */
  while( acceptkeyword( f, "var" ) ) {
    char varname[FVM_MAX_NAME];
    var_t vartype;
    uint32_t arraylen;
    struct var *global;
    struct record *rec;
    
    /* var name : type; */
    if( glob.tok.type != TOK_NAME ) usage( "Expected var name not %s", gettokname( glob.tok.type ) );
    strncpy( varname, glob.tok.val, FVM_MAX_NAME - 1 );
    expecttok( f, TOK_NAME );
    expecttok( f, TOK_COLON );
    parsevartype( f, &vartype, &arraylen, &rec );
    expecttok( f, TOK_SEMICOLON );

    global = addglobal( varname, vartype, arraylen );
    global->record = rec;
  }

  /* everything else following this is in the text segment */
  while( glob.tok.type == TOK_NAME ) {
    if( acceptkeyword( f, "procedure" ) ) {
      parseprocedure( f );
    } else if( acceptkeyword( f, "const" ) ) {
      /* parse constant data: const var name = value (type infered from value) */
      char varname[FVM_MAX_NAME];
      var_t vartype;
      char *val;
      int len;
      
      val = NULL;
      len = 0;
      vartype = 0;
      
      expectkeyword( f, "var" );
      if( glob.tok.type != TOK_NAME ) usage( "Expected const var name" );
      strncpy( varname, glob.tok.val, FVM_MAX_NAME - 1 );
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
	//if( len % 4 ) len += 4 - (len % 4); /* XXX: do we need this? What value does it add? */
	val = malloc( len );
	strncpy( val, glob.tok.val, len );
	expecttok( f, TOK_STRING );      
	break;
      default:
	usage( "Invalid const type, must be u32 or string" );
	break;
      }

      if( glob.pass == 1 ) {
	struct constvar *cv;
	cv = getconst( varname );
	if( cv ) {
	  if( cv->address ) usage( "Duplicate definition of const var %s", varname );
	  if( cv->type != vartype ) usage( "Declaration type does not match definition type for const %s", varname );
	  cv->val = val;
	  cv->len = len;
	  cv->address = glob.pc;
	  glob.pc += len;
	} else {
	  addconst( varname, vartype, val, len );
	}
      } else {
	struct constvar *cv = getconst( varname );
	if( !cv ) usage( "Unknown const %s", varname );
	emitdata( cv->val, cv->len );
      }
      
      expecttok( f, TOK_SEMICOLON );
    } else expectkeyword( f, "end" );
  }

  //expectkeyword( f, "end" );
  expecttok( f, TOK_PERIOD );

  fvmc_printf( ";; Done\n" );
}

static void processincludefile( char *path ) {
  FILE *f;
  struct includepath *p;
  char ppath[256];
  struct token *tok;
  
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

  tok = nexttok( f );
  if( tok ) {
    while( 1 ) {
      if( acceptkeyword( f, "declare" ) ) {
	parsedeclaration( f );
      } else if( acceptkeyword( f, "const" ) ) {
	parseconstdef( f );
      } else if( acceptkeyword( f, "procedure" ) ) {
	parseprocedure( f );
      } else if( acceptkeyword( f, "record" ) ) {
	parserecord( f );
      } else usage( "Invalid form % (%s) in include file", gettokname( glob.tok.type ), glob.tok.val ? glob.tok.val : "" );
      if( glob.tok.type == TOK_PERIOD ) break;
    }
  }
  

  fclose( f );
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
    glob.pc = FVM_ADDR_TEXT;
    parsefile( f );
    glob.datasize = glob.globals ? (glob.globals->address + glob.globals->size) - FVM_ADDR_DATA : 0;
    glob.textsize = glob.pc - FVM_ADDR_TEXT;
    break;
  case 2:
    /* second pass - generate code */
    glob.pc = FVM_ADDR_TEXT;
    glob.stackoffset = 0;
    parsefile( f );
    break;    
  }
  
  
  fclose( f );
}


static void fvmc_encode_header( struct xdr_s *xdr, struct fvm_headerinfo *x ) {
  int i;
  xdr_encode_uint32( xdr, x->magic );
  xdr_encode_uint32( xdr, x->version );
  xdr_encode_string( xdr, x->name );
  xdr_encode_uint32( xdr, x->progid );
  xdr_encode_uint32( xdr, x->versid );
  xdr_encode_uint32( xdr, x->datasize );
  xdr_encode_uint32( xdr, x->textsize );
  xdr_encode_uint32( xdr, x->nprocs );
  for( i = 0; i < x->nprocs; i++ ) {
    xdr_encode_string( xdr, x->procs[i].name );
    xdr_encode_uint32( xdr, x->procs[i].address );
    xdr_encode_uint64( xdr, x->procs[i].siginfo );
  }
  xdr_encode_uint64( xdr, x->timestamp );
}
static int fvmc_decode_header( struct xdr_s *xdr, struct fvm_headerinfo *x ) {
  int i, sts;
  sts = xdr_decode_uint32( xdr, &x->magic );
  if( sts ) return sts;
  sts = xdr_decode_uint32( xdr, &x->version );
  if( sts ) return sts;  
  sts = xdr_decode_string( xdr, x->name, sizeof(x->name) );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->progid );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->versid );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->datasize );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->textsize );
  if( sts ) return sts;  
  sts = xdr_decode_uint32( xdr, &x->nprocs );
  if( sts ) return sts;
  if( x->nprocs > FVM_MAX_PROC ) return -1;
  for( i = 0; i < x->nprocs; i++ ) {
    sts = xdr_decode_string( xdr, x->procs[i].name, sizeof(x->procs[i].name) );
    if( sts ) return sts;    
    sts = xdr_decode_uint32( xdr, &x->procs[i].address );
    if( sts ) return sts;    
    sts = xdr_decode_uint64( xdr, &x->procs[i].siginfo );
    if( sts ) return sts;    
  }
  sts = xdr_decode_uint64( xdr, &x->timestamp );
  if( sts ) return sts;
  return 0;
}

static void compile_file( char *path, char *outpath ) {
  struct fvm_headerinfo header;
  struct export *e;
  struct proc *proc;
  char hdrbuf[2048];
  struct xdr_s xdr;
  
  strcpy( glob.curfile, path );
  strcpy( glob.outpath, outpath );
  glob.outfile = fopen( outpath, "wb" );
  if( !glob.outfile ) usage( "Unable to open output file" );
  
  //glob.pass = 0;  
  //processfile( path );

  fvmc_printf( "--------------------- Pass 1 -------------------- \n" );
  glob.pass = 1;  
  processfile( path );

  if( glob.pc > (FVM_ADDR_TEXT + FVM_MAX_TEXT) ) usage( "Text segment size %d > max %d", glob.textsize, FVM_MAX_TEXT);
  if( glob.datasize > FVM_MAX_DATA ) usage( "Data segment size %d > max %d", glob.datasize, FVM_MAX_DATA );
  
  fvmc_printf( "--------------------- Pass 2 -------------------- \n" );
  glob.pass = 2;

  /* emit header section */
  memset( &header, 0, sizeof(header) );
  header.magic = FVM_MAGIC;
  header.version = FVM_VERSION;
  strcpy( header.name, glob.progname );
  header.progid = glob.progid;
  header.versid = glob.versid;
  header.datasize = glob.datasize;
  header.textsize = glob.textsize;
  e = glob.exports;
  while( e ) {
    if( header.nprocs >= FVM_MAX_PROC ) usage( "Max procs exceeded" );
    
    proc = getproc( e->name );
    if( !proc ) usage( "Cannot export %s - no proc found", e->name );

    /* check export signatures are ok */
    {
      struct param *p, *prev;
      if( proc->nparams > FVM_MAX_PARAM ) usage( "Proc %s max params exceeded", proc->name );
      
      p = proc->params;
      prev = NULL;
      while( p ) {
	if( p->type == VAR_TYPE_OPAQUE ) {
	  if( prev == NULL || prev->type != VAR_TYPE_U32 ) usage( "Proc %s Opaque parameter %s MUST follow a u32 implicit length parameter", proc->name, p->name );

	  if( p->isvar && !prev->isvar ) usage( "Proc %s Var type opaque parameter %s MUST follow a var type u32 parameter", proc, p->name );
	  if( prev->isvar && !p->isvar ) usage( "Proc %s Non-var opaque parameter %s MUST follow a non-var u32 parameter", proc->name, p->name );
	}
	prev = p;
	p = p->next;
      }
    }

    
    strcpy( header.procs[header.nprocs].name, proc->name );
    header.procs[header.nprocs].address = proc->address;
    header.procs[header.nprocs].siginfo = proc->siginfo;
    header.nprocs++;
    e = e->next;
  }
  header.timestamp = time( NULL );
  xdr_init( &xdr, (uint8_t *)hdrbuf, sizeof(hdrbuf) );
  fvmc_encode_header( &xdr, &header );
  fwrite( xdr.buf, 1, xdr.offset, glob.outfile );

  /* reset label counter */
  glob.labelidx = 0;

  /* process 2nd pass */
  processfile( path );

  fclose( glob.outfile );


  fvmc_printf( "--------------\n" );
  fvmc_printf( "Module: %s\n", header.name );
  fvmc_printf( "Progid: %u:%u\n", header.progid, header.versid );
  fvmc_printf( "DataSize: %u\n", header.datasize );
  fvmc_printf( "TextSize: %u\n", header.textsize );
  
  {
    struct label *l;
    l = glob.labels;
    while( l ) {
      fvmc_printf( "Label: %s 0x%x\n", l->name, l->address );
      l = l->next;
    }
  }
  {
    struct var *v;
    v = glob.globals;
    while( v ) {
      fvmc_printf( "Global: %s 0x%0x\n", v->name, v->address );
      v = v->next;
    }
  }
  {
    struct proc *p;
    struct param *param;
    int i;
    
    p = glob.procs;
    while( p ) {
      fvmc_printf( "Proc: %s 0x%0x siginfo 0x%"PRIx64"\n", p->name, p->address, p->siginfo );
      param = p->params;
      i = 0;
      while( param ) {
	fvmc_printf( "  Param %u: %s%s : %s\n",
		     i,
		     param->isvar ? "var " : "",
		     param->name,
		     param->type == VAR_TYPE_U32 ? "Int" :
		     param->type == VAR_TYPE_STRING ? "String" :
		     param->type == VAR_TYPE_OPAQUE ? "Opaque" :
		     "Other" );
	i = i + 1;
	param = param->next;
      }
      p = p->next;
    }
  }
  {
    struct export *e;
    e = glob.exports;
    while( e ) {
      fvmc_printf( "Export: %s\n", e->name );
      e = e->next;
    }
  }
  {
    struct constvar *v;
    v = glob.consts;
    while( v ) {
      fvmc_printf( "Const: %s 0x%0x\n", v->name, v->address );
      v = v->next;
    }
  }
  {
    struct constval *v;
    v = glob.constvals;
    while( v ) {
      if( v->type == VAR_TYPE_U32 ) fvmc_printf( "Const val: %s %u\n", v->name, *((uint32_t *)v->val) );
      else fvmc_printf( "Const val: %s %s\n", v->name, v->val );
      v = v->next;
    }
  }
  {
    struct record *r;
    struct var *v;
    r = glob.records;
    while( r ) {
      fvmc_printf( "[%u] Record %s = \n", r->size, r->name );
      v = r->fields;
      while( v ) {
	fvmc_printf( "  [%u] %s : %s",
		     v->offset,
		     v->name,
		     v->type == VAR_TYPE_U32 ? "Int" :
		     v->type == VAR_TYPE_STRING ? "String" :
		     v->type == VAR_TYPE_OPAQUE ? "Opaque" :
		     "Other" );
	if( v->arraylen ) {
	  fvmc_printf( "[%u]", v->arraylen );
	}
	fvmc_printf( ";\n" );
	v = v->next;
      }
      r = r->next;
    }

  }
}

static void disassemblefile( char *path ) {
  struct mmf_s mmf;
  int sts, i, j, k, isvar, vartype, nargs;
  struct xdr_s xdr;
  struct fvm_headerinfo hdr;
  uint8_t *ptr;
  op_t op;
  struct opinfo *opinfo;
  uint16_t u16;
  uint32_t u32;
  
  sts = mmf_open2( path, &mmf, MMF_OPEN_EXISTING );
  if( sts ) usage( "Failed to open file %s", path );

  mmf_remap( &mmf, mmf.fsize );
  xdr_init( &xdr, mmf.file, mmf.fsize );
  sts = fvmc_decode_header( &xdr, &hdr );
  if( sts ) usage( "Failed to parse header" );

  if( hdr.magic != FVM_MAGIC ) {
    usage( "Bad magic" );
  }
  
  if( hdr.version != FVM_VERSION ) {
    usage( "Bad version" );
  }
  
  if( xdr.count != (xdr.offset + hdr.textsize) ) {
    usage( "Bad buffer size %u expected %u", xdr.count, xdr.offset + hdr.textsize );
  }
  
  for( i = 0; i < hdr.nprocs; i++ ) {
    if( (hdr.procs[i].address < FVM_ADDR_TEXT) ||
	(hdr.procs[i].address >= (FVM_ADDR_TEXT + hdr.textsize)) ) {
      usage( "Proc address outsize text" );
    }

    /* opaque params must be preceeded by a u32 param that receives the length */
    if( FVM_SIGINFO_VARTYPE(hdr.procs[i].siginfo,i) == VAR_TYPE_OPAQUE ) {
      if( i == 0 ) {
	usage( "Bad parameter" );
      }
      
      if( FVM_SIGINFO_VARTYPE(hdr.procs[i].siginfo, i - 1) != VAR_TYPE_U32 ) {
	usage( "Bad parameter" );
      }
      
      if( FVM_SIGINFO_ISVAR(hdr.procs[i].siginfo, i) && !FVM_SIGINFO_ISVAR(hdr.procs[i].siginfo, i - 1 ) ) {
	usage( "Bad parameter" );	
      }
      
      if( !FVM_SIGINFO_ISVAR(hdr.procs[i].siginfo, i) && FVM_SIGINFO_ISVAR(hdr.procs[i].siginfo, i - 1) ) {
	usage( "Bad parameter" );	
      }
    }
  }

  printf( "Name %s\n", hdr.name );
  printf( "ProgID %u %u\n", hdr.progid, hdr.versid );
  printf( "DataSize %u TextSize %u\n", hdr.datasize, hdr.textsize );
  {
    char timestr[64];
    printf( "Timestamp %s\n", sec_timestr( hdr.timestamp, timestr ) );
  }  
  for( i = 0; i < hdr.nprocs; i++ ) {
    printf( "[%u] %04x Procedure %s(", i, hdr.procs[i].address, hdr.procs[i].name );
    nargs = FVM_SIGINFO_NARGS(hdr.procs[i].siginfo);
    for( k = 0; k < nargs; k++ ) {
      vartype = FVM_SIGINFO_VARTYPE(hdr.procs[i].siginfo, k);
      isvar = FVM_SIGINFO_ISVAR(hdr.procs[i].siginfo, k);
      printf( "%s%s%s", k ? ", " : "", isvar ? "var " : "",
	      vartype == VAR_TYPE_U32 ? "Int" :
	      vartype == VAR_TYPE_STRING ? "String" :
	      vartype == VAR_TYPE_OPAQUE ? "Opaque" :
	      "Other" );
    }
    printf( ")\n" );    
  }
  printf( "\n" );

  
  /* read opcodes and print */
  i = xdr.offset;
  ptr = (uint8_t *)mmf.file;
  while( i < mmf.fsize ) {
    for( j = 0; j < hdr.nprocs; j++ ) {
      if( hdr.procs[j].address == (FVM_ADDR_TEXT + i - xdr.offset) ) {
	printf( "%04x Procedure %s(", FVM_ADDR_TEXT + i - xdr.offset, hdr.procs[j].name );
	nargs = FVM_SIGINFO_NARGS(hdr.procs[j].siginfo);
	for( k = 0; k < nargs; k++ ) {
	  vartype = FVM_SIGINFO_VARTYPE(hdr.procs[j].siginfo, k);
	  isvar = FVM_SIGINFO_ISVAR(hdr.procs[j].siginfo, k);
	  printf( "%s%s%s", k ? ", " : "", isvar ? "var " : "",
		  vartype == VAR_TYPE_U32 ? "Int" :
		  vartype == VAR_TYPE_STRING ? "String" :
		  vartype == VAR_TYPE_OPAQUE ? "Opaque" :
		  "Other" );
	}
	printf( ")\n" );
	break;
      }
    }
      
    op = ptr[i];
    opinfo = getopinfo( op );
    if( !opinfo ) {
      printf( "%04x %-8s %-8u | %-8d | 0x%04x %c\n",
	     FVM_ADDR_TEXT + i - xdr.offset, "Unknown", op, op, op, op );
      i++;
    } else {
      if( op == OP_ADDSP ) printf( "\n" );
      
      switch( opinfo->pcdata ) {
      case 0:
	printf( "%04x %-8s\n", FVM_ADDR_TEXT + i - xdr.offset, opinfo->name );
	i += 1;
	break;
      case 2:
	memcpy( &u16, &ptr[i + 1], 2 );
	printf( "%04x %-8s %-8u | %-8d | 0x%04x\n",
		FVM_ADDR_TEXT + i - xdr.offset,
		opinfo->name, (uint32_t)u16, (int32_t)(int16_t)u16, (uint32_t)u16 );
	i += 3;
	break;
      case 4:
	memcpy( &u32, &ptr[i + 1], 4 );
	printf( "%04x %-8s %-8u | %-8d | 0x%04x\n",
		FVM_ADDR_TEXT + i - xdr.offset, opinfo->name, u32, (int32_t)u32, u32 );
	i += 5;
	break;
      default:
	break;
      }
    }
  }

  
  mmf_close( &mmf );
}

static void usage( char *fmt, ... ) {
  va_list args;
  
  if( fmt ) {
    if( glob.linecount ) printf( "%s:%u ", glob.curfile, glob.linecount );
    
    va_start( args, fmt );
    printf( "Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  } else {
    printf( "Usage: fvmc [-o output] [-I includepath] [-v] [-d] filename\n"
	    "  -o            Set output path\n"
	    "  -I            Add include path\n"
	    "  -v            Verbose mode\n"
	    "  -d            Disassemble file\n"
	    );
  }

  /* delete the output file if we are exiting */
  if( glob.outfile ) {    
    fclose( glob.outfile );
    mmf_delete_file( glob.outpath );
  }
  
  exit( 1 );  
}
