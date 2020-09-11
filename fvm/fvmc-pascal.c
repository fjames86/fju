
/*
 * TODO: 
 * - better register allocation.
 * - arrays e.g. var x : array[length] of integer. arrays are same as string i.e. have a length prefix. would need two new types, array and array[] 
 * 
 */

 #ifdef WIN32
 #define _CRT_SECURE_NO_WARNINGS
 #define strcasecmp _stricmp
 #endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>

static void fvmc_printf( int lvl, char *fmt, ... );
static void fvmc_emit( char *fmt, ... );

static char whitespacechars[] = { ' ', '\n', '\t', '\r', '\0' };
static char terminalchars[] = { '+', '-', '*', '/', ';', '.', '{', '}', ' ', '\n', '\t', '\r', '(', ')', ':', '[', ']', ',', '^', '\0' };

static int skipcomment( FILE *f ) {
  char c;
  while( 1 ) {
    c = fgetc( f );
    if( c == EOF ) return 0;
    if( c == '}' ) break;
  }
  
  return 0;
}

static int skipwhitespace( FILE *f ) {
  /* read until non-whitespace char encountered */
  char c;
  int i, found;

  while( 1 ) {
    c = fgetc( f );
    if( c == EOF ) {
      fvmc_printf( 1, "skipwhitespace EOF\n" );
      return -1;
    }

    found = 0;
    for( i = 0; whitespacechars[i] != '\0'; i++ ) {
      if( whitespacechars[i] == c ) {
	found = 1;
	break;
      }
    }

    if( found ) continue;

    if( c == '{' ) {
      fvmc_printf( 1, "skipping comment\n" );
      skipcomment( f );
      continue;
    }

    /* not whitespace or a comment so we are done */
    ungetc( c, f );
    break;
  }
  
  return 0;  
}


/* read until terminal char */
static int getnexttoken( FILE *f, char *token, int toksize ) {
  char c, *p;
  int i, found, sts;
  
  sts = skipwhitespace( f );
  if( sts ) return sts;

  c = fgetc( f );
  if( c == EOF ) {
    fvmc_printf( 1, "end of file\n" );
    return -1;
  }
  if( c == '"' ) {
    /* read until " */
    p = token;
    *p = '"';
    p++;
    while( 1 ) {
      c = fgetc( f );
      if( c == EOF ) {
	fvmc_printf( 1, "end of file\n" );
	return -1;
      }
      if( toksize > 0 ) {
	*p = c;
	p++;
      }
      if( c == '"' ) break;
      
      toksize--;
      if( toksize == 0 ) {
	fvmc_printf( 1, "token buffer out of space\n" );
	return -1;
      }
    }
    return 0;
  } else if( c == ':' ) {
    c = fgetc( f );
    if( c == '=' ) {
      strcpy( token, ":=" );
      return 0;
    }
    ungetc( c, f );    
    strcpy( token, ":" );
    return 0;    
  }
  ungetc( c, f );
  
  p = token;
  *p = '\0';
  while( 1 ) {
    c = fgetc( f );
    if( c == EOF ) {
      fvmc_printf( 1, "getnexttoken EOF\n" );
      if( strlen( token ) == 0 ) return -1;
      break;
    }

    found = 0;
    for( i = 0; terminalchars[i] != '\0'; i++ ) {
      if( terminalchars[i] == c ) {
	found = 1;
	break;
      }
    }

    if( found ) {
      if( strlen( token ) == 0 ) {
	if( c == '{' ) {
	  fvmc_printf( 1, "skipping comment\n" );
	  skipcomment( f );
	  continue;
	}
	
	*p = c;
	p++;
	break;
      }
      
      ungetc( c, f );
      break;
    }

    *p = c;
    p++;
    toksize--;
    if( toksize == 0 ) {
      fvmc_printf( 1, "token \"%s\" too large\n", token );
      return -1;
    }
  }
  *p = '\0';
	
  return 0;
}


/* ------------------------------------------ */


/* 
 * Information we need to keep track of: 
 * - location of local variables on the stack. Need a list of local variables and their stack offsets.
 * - names of global variables (unknown locals can be assumed to be globals)
 * - Contents of registers i.e. whether a register is known to contain a variable or is unused  
 */


static void usage( char *fmt, ... ) {
  va_list args;
  
  if( fmt ) {
    va_start( args, fmt );
    printf( "Syntax Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }
   
  exit( 1 );  
}


typedef enum {
	      TOK_PROGRAM,
	      TOK_PERIOD,
	      TOK_COLON,
	      TOK_SEMICOLON,
	      TOK_IF,
	      TOK_ELSE,
	      TOK_THEN,
	      TOK_PROCEDURE,
	      TOK_BEGIN,
	      TOK_END,
	      TOK_VAR,
	      TOK_INTEGER,
	      TOK_STRING,
	      TOK_OPAQUE,
	      TOK_OPENARRAY,
	      TOK_CLOSEARRAY,
	      TOK_RETURN,
	      TOK_VALSTRING,
	      TOK_VALINTEGER,
	      TOK_NAME,
	      TOK_CALL,
	      TOK_OPENPAREN,
	      TOK_CLOSEPAREN,
	      TOK_EQ,
	      TOK_GTE,
	      TOK_LTE,
	      TOK_GT,
	      TOK_LT,
	      TOK_COMMA,
	      TOK_NOT,
	      TOK_AND,
	      TOK_MUL,
	      TOK_DIV,
	      TOK_MOD,
	      TOK_PLUS,
	      TOK_MINUS,
	      TOK_NEQ,
	      TOK_CONST,
	      TOK_GOTO,
	      TOK_ASSIGN,
	      TOK_DO,
	      TOK_WHILE,
	      TOK_XOR,
	      TOK_OR,
	      TOK_DECLARE,
	      TOK_ASM,
	      TOK_SYSCALL,
	      TOK_CARET,
	      TOK_ADDRESS,
} token_t;
static struct {
  char *keyword;
  token_t type;
} keywordtokens[] = {
		     { "PROGRAM", TOK_PROGRAM },
		     { ".", TOK_PERIOD },
		     { ":", TOK_COLON },
		     { ";", TOK_SEMICOLON },
		     { "IF", TOK_IF },
		     { "THEN", TOK_THEN },
		     { "ELSE", TOK_ELSE },
		     { "PROCEDURE", TOK_PROCEDURE },
		     { "BEGIN", TOK_BEGIN },
		     { "END", TOK_END },
		     { "VAR", TOK_VAR },
		     { "INTEGER", TOK_INTEGER },
		     { "STRING", TOK_STRING },
		     { "[", TOK_OPENARRAY },
		     { "]", TOK_CLOSEARRAY },
		     { "RETURN", TOK_RETURN },
		     { "CALL", TOK_CALL },
		     { "(", TOK_OPENPAREN },
		     { ")", TOK_CLOSEPAREN },
		     { "=", TOK_EQ },
		     { ">", TOK_GT },
		     { ">=", TOK_GTE },
		     { "<", TOK_LT },
		     { "<=", TOK_LTE },
		     { ",", TOK_COMMA },
		     { "NOT", TOK_NOT },
		     { "AND", TOK_AND },
		     { "*", TOK_MUL },
		     { "/", TOK_DIV },
		     { "%", TOK_MOD },
		     { "+", TOK_PLUS },
		     { "-", TOK_MINUS },
		     { "<>", TOK_NEQ },
		     { "CONST", TOK_CONST },
		     { "GOTO", TOK_GOTO },
		     { ":=", TOK_ASSIGN },
		     { "DO", TOK_DO },
		     { "WHILE", TOK_WHILE },
		     { "XOR", TOK_XOR },
		     { "OR", TOK_OR },
		     { "OPAQUE", TOK_OPAQUE },
		     { "DECLARE", TOK_DECLARE },
		     { "ASM", TOK_ASM },
		     { "SYSCALL", TOK_SYSCALL },
		     { "^", TOK_CARET },
		     { "ADDRESSOF", TOK_ADDRESS },
		     
		     { NULL, 0 }
};

static token_t gettokentype( char *token ) {
  int i;  
  char *p;
  
  for( i = 0; keywordtokens[i].keyword != NULL; i++ ) {
    if( strcasecmp( keywordtokens[i].keyword, token ) == 0 ) return keywordtokens[i].type;
  }
  
  i = strtoul( token, &p, 0 );
  if( *p == '\0' ) return TOK_VALINTEGER;

  if( *p == '"' ) {
    p++;
    while( *p != '"' && *p != '\0' ) {
      p++;
    }
    if( *p == '"' ) return TOK_VALSTRING;
  }

  return TOK_NAME;
}

struct token {
  char token[64];
  token_t type;
};

typedef enum {
	      VAR_INTEGER,
	      VAR_STRING,
	      VAR_STRINGBUF,
	      VAR_OPAQUE,
	      VAR_OPAQUEBUF,
} var_t;

struct var {
  struct var *next;
  char name[64];
  var_t type;
  uint32_t flags;
#define VAR_ISVAR   0x00010000
#define VAR_ISPARAM 0x00020000
#define VAR_ISCONST 0x00040000
  uint32_t size;
  uint32_t offset;
  uint32_t reg;
};

struct proc {
  struct proc *next;
  char name[64];
  struct var *vars;
  uint32_t flags;
#define PROC_SYSCALL  0x0001
  uint32_t syscallid;
};

static struct {
  struct token tok;
  FILE *infile;
  FILE *outfile;
  struct var *vars;
  int verbosemode;
  struct proc *procs;
  struct var *globals;
  int labelidx;
  int stackoffset;
  int currentargsize;
} glob;




static int nexttok( void ) {
  struct token tok;
  int sts;

  memset( tok.token, 0, sizeof(tok.token) );
  sts = getnexttoken( glob.infile, tok.token, sizeof(tok.token) );
  if( sts ) return sts;

  tok.type = gettokentype( tok.token );
  glob.tok = tok;
  fvmc_printf( 1, "token %u %s\n", tok.type, tok.token );
  return 0;
}

static int accepttok( token_t type ) {
  int sts;
  if( glob.tok.type == type ) {
    sts = nexttok();
    if( sts ) {      
      glob.tok.type = TOK_PERIOD;
      strcpy( glob.tok.token, "." );
    }
    return 1;
  }
  
  return 0;
}

static int expecttok( token_t type ) {
  if( accepttok( type ) ) {
    return 1;
  }
  usage( "Unexpected symbol %u %s, expected %u", glob.tok.type, glob.tok.token, type );
  return 0;
}
#define expectok(type) expecttok(type)

static struct var *getvar( char *name ) {
  struct var *v;
  v = glob.vars;
  while( v ) {
    if( strcasecmp( v->name, name ) == 0 ) return v;
    v = v->next;
  }
  return NULL;
}
static struct var *addvar( char *name, var_t type, uint32_t size, uint32_t offset, uint32_t flags ) {
  struct var *v = malloc( sizeof(*v) );
  memset( v, 0, sizeof(*v) );
  strcpy( v->name, name );
  v->type = type;
  v->size = size;
  v->offset = offset;
  v->flags = flags;
  v->next = glob.vars;
  glob.vars = v;
  return v;
}
static void adjustvars( uint32_t offset ) {
  struct var *v = glob.vars;
  while( v ) {
    v->offset += offset;
    v = v->next;
  }
}
static void freevars( void ) {
  struct var *v = glob.vars, *next;
  while( v ) {
    next = v->next;
    free( v );
    v = next;
  }
  glob.vars = NULL;
}
static void assignreg( struct var *v, uint32_t reg ) {
  struct var *vv;

  vv = glob.vars;
  while( vv ) {
    if( vv == v ) {
      v->reg = reg;
    } else if( vv->reg == reg ) {
      vv->reg = -1;
    }
    vv = vv->next;
  }
      
}
static void voidregisters( void ) {
  struct var *v;
  v = glob.vars;
  while( v ) {
    v->reg = -1;
    v = v->next;
  }
}
static uint32_t allocregister( void ) {
  struct var *v;
  uint32_t reg, found;
  for( reg = 7; reg >= 2; reg-- ) {
    v = glob.vars;
    found = 0;
    while( v ) {
      if( v->reg == reg ) {
	found = 1;
	break;
      }
      v = v->next;
    }
    
    if( !found ) return reg;
  }

  /* choose a "random" register in range 2-7*/
  return 2 + (time( NULL ) % 6);
}


static struct proc *addproc( char *procname, struct var *vars ) {
  struct proc *p, *pprev;
  struct var *v1, *v2;

  /* check parameters are ok */
  v1 = vars;
  while( v1 ) {
    v2 = vars;
    while( v2 ) {
      if( (v1 != v2) && (strcasecmp( v1->name, v2->name ) == 0) ) usage( "Duplicate parameter names in procedure definition" );
      v2 = v2->next;
    }
    v1 = v1->next;
  }

  /* check for duplicate definition */
  p = glob.procs;
  pprev = NULL;
  while( p ) {
    if( strcasecmp( p->name, procname ) == 0 ) {
      v1 = p->vars;
      v2 = vars;
      while( v1 && v2 ) {
	if( strcasecmp( v1->name, v2->name ) != 0 ) usage( "Incompatible duplicate procedure %s: parameters different names", procname );
	if( v1->type != v2->type ) usage( "Incompatible duplicate procedure %s: parameter %s different types %u != %u", procname, v1->name, v1->type, v2->type );
	if( v1->flags != v2->flags ) usage( "Incompatible duplicate procedure %s: parameter %s different flags", procname, v1->name );
	v1 = v1->next;
	v2 = v2->next;
      }
      if( v1 || v2 ) usage( "Incompatible duplicate procedure %s: different number of parameters", procname );
      return p;
    }
    pprev = p;
    p = p->next;
  }

  /* push onto list */
  p = malloc( sizeof(*p) );
  memset( p, 0, sizeof(*p) );
  strcpy( p->name, procname );
  p->vars = vars;
  if( pprev ) pprev->next = p;
  else glob.procs = p;

  return p;
}

static struct proc *getproc( char *procname ) {
  struct proc *p;
  p = glob.procs;
  while( p ) {
    if( strcasecmp( p->name, procname ) == 0 ) return p;
    p = p->next;
  }
  return NULL;
}

static struct var *addglobal( char *name, var_t type, uint32_t flags ) {
  struct var *v;
  v = malloc( sizeof(*v) );
  memset( v, 0, sizeof(*v) );
  strcpy( v->name, name );
  v->type = type;
  v->flags = flags;
  v->next = glob.globals;
  glob.globals = v;

  if( flags & VAR_ISCONST ) fvmc_printf( 2, "Defining constant %s\n", name );
  return v;
}

static struct var *getglobal( char *name ) {
  struct var *v;
  v = glob.globals;
  while( v ) {
    if( ((v->flags & VAR_ISCONST) == 0) && (strcasecmp( v->name, name ) == 0) ) return v;
    v = v->next;
  }
  return NULL;
}
static struct var *getconst( char *name ) {
  struct var *v;
  v = glob.globals;
  while( v ) {
    if( (v->flags & VAR_ISCONST) && (strcasecmp( v->name, name ) == 0) ) return v;
    v = v->next;
  }
  return NULL;
}
static uint32_t parseconstant( void ) {
  struct token tok;
  tok = glob.tok;
  if( accepttok( TOK_VALINTEGER ) ) {
    return strtoul( tok.token, NULL, 0 );
  } else if( accepttok( TOK_NAME ) ) {
    struct var *v;
    v = getconst( tok.token );
    if( !v ) usage( "Unknown constant %s", tok.token );
    return v->offset;
  } else usage( "Version expected an integer or named constant, not %s", tok.token );
  return 0;
}

/* ----- */


static void parseblock( void );
static void parseexpr( int reg );

typedef enum {
	      OP_NONE,
	      OP_EQ,
	      OP_NEQ,
	      OP_GT,
	      OP_GTE,
	      OP_LT,
	      OP_LTE,
} op_t;
static op_t parsecondition( void );
static void parsestatement( void );
static void parseprogram( void );

static char *genlabel( char *prefix ) {
  static char labelname[64];

  sprintf( labelname, "L%s%s%s%u", prefix ? "-" : "", prefix ? prefix : "", prefix ? "-" : "", ++glob.labelidx );
  return labelname;
}

static void emitldimmediate( int reg, uint32_t u32 ) {
  if( u32 <= 0xffff ) {
    fvmc_emit( "\tLDI\tR%u\t%u\n", reg, u32 );
  } else {
    fvmc_emit( "\tLDI\tR%u\t%u\n", reg, u32 >> 16 );
    fvmc_emit( "\tSHL\tR%u\t16\n", reg );
    fvmc_emit( "\tOR\tR%u\t%u\n", reg, u32 & 0xffff );
  }
}

static void parseexpr( int reg ) {
  struct token tok;
  struct var *v;

  if( reg >= 8 ) usage( "Bad register %u", reg );
  
  /* valinteger | constant | ( expr ) | expr operator expr */  
  tok = glob.tok;
  if( accepttok( TOK_VALINTEGER ) ) {
    emitldimmediate( reg, (unsigned int)strtoul( tok.token, NULL, 0 ) );
  } else if( accepttok( TOK_NAME ) ) {
    uint32_t arrayindex = -1;

    if( accepttok( TOK_OPENARRAY ) ) {
      arrayindex = parseconstant();
      expecttok( TOK_CLOSEARRAY );
    }
    
    /* check if constant ? */
    v = getvar( tok.token );
    if( v ) {
#if 0
      if( v->reg != -1 ) {
	/* variable already in a register */
	if( v->reg != reg ) {
	  fvmc_emit( "\tMOV\tR%u\tR%u\n", reg, v->reg );
	}
      } else
#endif

      /* TODO if accepttok( TOK_OPENARRAY ) then compute index into array */
	
      if( v->type == VAR_STRINGBUF || v->type == VAR_OPAQUEBUF ) {
	fvmc_emit( "\tLEASP\tR%u\t-%u\t ; load %s %s\n", reg, v->offset + glob.stackoffset, v->flags & VAR_ISPARAM ? "param" : "local", v->name );
	//assignreg( v, reg );
      } else {
	fvmc_emit( "\tLDSP\tR%u\t-%u\t ; load %s %s\n", reg, v->offset + glob.stackoffset, v->flags & VAR_ISPARAM ? "param" : "local", v->name );
	//assignreg( v, reg );
      }
      if( v->flags & VAR_ISVAR ) {
	/* var type parameters are really pointers, dereference it now */
	fvmc_emit( "\tLD\tR%u\tR%u\n", reg, reg );
      }
      if( arrayindex != -1 ) {
	if( v->type != VAR_OPAQUE && v->type != VAR_STRING && v->type != VAR_STRINGBUF && v->type != VAR_OPAQUEBUF ) usage( "Cannot index into variable of wrong type" );
	fvmc_emit( "\tADD\tR%u\t%u\n", reg, (v->type == VAR_STRING && v->type == VAR_STRINGBUF ? 4 : 0) + arrayindex );
	fvmc_emit( "\tLD\tR%u\tR%u\n", reg, reg );
	fvmc_emit( "\tAND\tR%u\t0xff\n", reg );
      }
      
    } else {
      /* assume a global */
      v = getglobal( tok.token );
      if( v ) {
	if( v->type == VAR_STRINGBUF || v->type == VAR_OPAQUEBUF ) {
	  fvmc_emit( "\tLDI\tR%u\t%s\n", reg, v->name );
	  if( arrayindex != -1 ) fvmc_emit( "\tADD\tR%u\t%u\n", reg, arrayindex );
	} else {
	  fvmc_emit( "\tLD\tR%u\t%s\n", reg, v->name );
	  if( arrayindex != -1 ) fvmc_emit( "\tADD\tR%u\t%u\n", reg, arrayindex );	  
	}
      } else {	
	/* not a known global, check constant */
	v = getconst( tok.token );
	if( !v ) fvmc_printf( 0, "Unknown variable %s, assuming constant\n", tok.token );
	if( arrayindex != -1 ) usage( "Cannot index into constant" );
	fvmc_emit( "\tLDI\tR%u\t%s\n", reg, tok.token );
      }
    }
  } else if( accepttok( TOK_VALSTRING ) ) {
    char jmplabel[64], strlabel[64];
    strcpy( jmplabel, genlabel( NULL ) );
    strcpy( strlabel, genlabel( NULL ) );    
    fvmc_emit( "\tJMP\t%s\n", jmplabel );
    fvmc_emit( "\t.TEXT\t%s\t%s\n", strlabel, tok.token );
    fvmc_emit( "%s:\n", jmplabel );
    fvmc_emit( "\tLDI\tR%u\t%s\n", reg, strlabel );
  } else if( accepttok( TOK_OPENPAREN ) ) {
    parseexpr( reg );
    expectok( TOK_CLOSEPAREN );
  } else if( accepttok( TOK_NOT ) ) {
    parseexpr( reg );
    fvmc_emit( "\tNOT\tR%u\n", reg );
  } else if( accepttok( TOK_CARET ) ) {
    /* ^var */
    tok = glob.tok;
    expecttok( TOK_NAME );
    v = getvar( tok.token );
    if( v ) {
      if( v->type != VAR_OPAQUE ) usage( "deference must be a variable of pointer type" );

      fvmc_emit( "\tLDSP\tR%u\t-%u\n", reg, v->offset + glob.stackoffset );
      fvmc_emit( "\tLD\tR%u\tR%u\n", reg, reg );
      
      if( v->flags & VAR_ISVAR ) {
	fvmc_emit( "\tLD\tR%u\tR%u\n", reg, reg );
      }
      
    } else {
      /* assume global */
      v = getglobal( tok.token );
      if( !v ) usage( "Unknown global %s", tok.token );
      
      fvmc_emit( "\tLD\tR%u\t%s\n", reg, tok.token );
      fvmc_emit( "\tLD\tR%u\tR%u\n", reg, reg );
    }
  } else if( accepttok( TOK_ADDRESS ) ) {
    /* address var */
    tok = glob.tok;
    expecttok( TOK_NAME );
    v = getvar( tok.token );
    if( v ) {
      fvmc_emit( "\tLEASP\tR%u\t-%u\n", reg, v->offset + glob.stackoffset );
    } else {
      v = getglobal( tok.token );      
      if( !v ) usage( "address operator expects a variable name, not %s", tok.token );
      fvmc_emit( "\tLDI\tR%u\t%s\n", reg, tok.token );
    }
  }

 again:
  if( accepttok( TOK_PLUS ) ) {
    uint32_t u32 = strtoul( glob.tok.token, NULL, 0 );
    if( glob.tok.type == TOK_VALINTEGER && u32 <= 0xffff ) {
      fvmc_emit( "\tADD\tR%u\t%u\n", reg, u32 );
      expecttok( TOK_VALINTEGER );
      goto again;
    } else {    
      parseexpr( reg + 1 );
      fvmc_emit( "\tADD\tR%u\tR%u\n", reg, reg + 1 );
    }
  } else if( accepttok( TOK_MINUS ) ) {
    uint32_t u32 = strtoul( glob.tok.token, NULL, 0 );    
    if( glob.tok.type == TOK_VALINTEGER && u32 <= 0xffff ) {
      fvmc_emit( "\tSUB\tR%u\t%u\n", reg, u32 );
      expecttok( TOK_VALINTEGER );
      goto again;
    } else {
      parseexpr( reg + 1 );
      fvmc_emit( "\tSUB\tR%u\tR%u\n", reg, reg + 1 );
    }
  } else if( accepttok( TOK_MUL ) ) {
    uint32_t u32 = strtoul( glob.tok.token, NULL, 0 );    
    if( glob.tok.type == TOK_VALINTEGER && u32 <= 0xffff ) {
      fvmc_emit( "\tMUL\tR%u\t%u\n", reg, u32 );
      expecttok( TOK_VALINTEGER );
      goto again;
    } else {
      parseexpr( reg + 1 );
      fvmc_emit( "\tMUL\tR%u\tR%u\n", reg, reg + 1 );
    }
  } else if( accepttok( TOK_DIV ) ) {
    uint32_t u32 = strtoul( glob.tok.token, NULL, 0 );    
    if( glob.tok.type == TOK_VALINTEGER && u32 <= 0xffff ) {
      fvmc_emit( "\tDIV\tR%u\t%u\n", reg, u32 );
      expecttok( TOK_VALINTEGER );
      goto again;
    } else {    
      parseexpr( reg + 1 );
      fvmc_emit( "\tDIV\tR%u\tR%u\n", reg, reg + 1 );
    }
  } else if( accepttok( TOK_MOD ) ) {
    uint32_t u32 = strtoul( glob.tok.token, NULL, 0 );    
    if( glob.tok.type == TOK_VALINTEGER && u32 <= 0xffff ) {
      fvmc_emit( "\tMOD\tR%u\t%u\n", reg, u32 );
      expecttok( TOK_VALINTEGER );
      goto again;
    } else {    
      parseexpr( reg + 1 );
      fvmc_emit( "\tMOD\tR%u\tR%u\n", reg, reg + 1 );
    }
  } else if( accepttok( TOK_AND ) ) {
    uint32_t u32 = strtoul( glob.tok.token, NULL, 0 );    
    if( glob.tok.type == TOK_VALINTEGER && u32 <= 0xffff ) {
      fvmc_emit( "\tAND\tR%u\t%u\n", reg, u32 );
      expecttok( TOK_VALINTEGER );
      goto again;
    } else {    
      parseexpr( reg + 1 );
      fvmc_emit( "\tAND\tR%u\tR%u\n", reg, reg + 1 );
    }
  } else if( accepttok( TOK_OR ) ) {
    uint32_t u32 = strtoul( glob.tok.token, NULL, 0 );    
    if( glob.tok.type == TOK_VALINTEGER && u32 <= 0xffff ) {
      fvmc_emit( "\tOR\tR%u\t%u\n", reg, u32 );
      expecttok( TOK_VALINTEGER );
      goto again;
    } else {    
      parseexpr( reg + 1 );
      fvmc_emit( "\tOR\tR%u\tR%u\n", reg, reg + 1 );
    }
  } else if( accepttok( TOK_XOR ) ) {
    uint32_t u32 = strtoul( glob.tok.token, NULL, 0 );    
    if( glob.tok.type == TOK_VALINTEGER && u32 <= 0xffff ) {
      fvmc_emit( "\tXOR\tR%u\t%u\n", reg, u32 );
      expecttok( TOK_VALINTEGER );
      goto again;
    } else {    
      parseexpr( reg + 1 );
      fvmc_emit( "\tXOR\tR%u\tR%u\n", reg, reg + 1 );
    }
  } 
  
}


static op_t parsecondition( void ) {
  op_t op;
  
  parseexpr( 0 );  
  if( accepttok( TOK_EQ ) ) {
    op = OP_EQ;
  } else if( accepttok( TOK_GT ) ) {
    op = OP_GT;
  } else if( accepttok( TOK_GTE ) ) {
    op = OP_GTE;
  } else if( accepttok( TOK_LT ) ) {
    op = OP_LT;
  } else if( accepttok( TOK_LTE ) ) {
    op = OP_LTE;
  } else if( accepttok( TOK_NEQ ) ) {
    op = OP_NEQ;
  } else {
    op = OP_NONE;
    return op;
  }
  
  parseexpr( 1 );
  fvmc_emit( "\tSUB\tR0\tR1\n" );
  return op;
}

static void printconditionjump( op_t op, char *label ) {
  fvmc_emit( "\t%s\t%s\n",
	  op == OP_NONE ? "JZ" :
	  op == OP_EQ ? "JZ" :
	  op == OP_NEQ ? "JPN" : 
	  op == OP_GT ? "JP" :
	  op == OP_GTE ? "JPZ" :
	  op == OP_LT ? "JZ" :
	  op == OP_LTE ? "JNZ" :
	  (usage( "Bad operator" ), ""),
	  label );
}

static void parseprocedurebody( void ) {
  /* 
   * var name : type
   * statement
   */
  struct token nametok;
  int argsize = 0;
  char names[8][64];
  int i, nnames;
  
  voidregisters();
  
  while( accepttok( TOK_VAR ) ) {
    /* var name : type */
    nametok = glob.tok;
    expectok( TOK_NAME );
    strcpy( names[0], nametok.token );
    nnames = 1;    
    while( accepttok( TOK_COMMA ) ) {
      if( nnames >= 8 ) usage( "Max 8 names in same var declaration" );      
      nametok = glob.tok;
      expecttok( TOK_NAME );
      strcpy( names[nnames], nametok.token );
      nnames++;
    }
    
    expectok( TOK_COLON );
    if( accepttok( TOK_INTEGER ) ) {
      for( i = 0; i < nnames; i++ ) {
	adjustvars( 4 );
	addvar( names[i], VAR_INTEGER, 4, 4, 0 );
	argsize += 4;	
      }
    } else if( accepttok( TOK_STRING ) ) {
      if( accepttok( TOK_OPENARRAY ) ) {
	int size;
	size = parseconstant();
	expectok( TOK_CLOSEARRAY );
	for( i = 0; i < nnames; i++ ) {	
	  adjustvars( 4 + size );
	  addvar( names[i], VAR_STRINGBUF, 4 + size, 4 + size, 0 );
	  argsize += 4 + size;
	}
      } else {
	for( i = 0; i < nnames; i++ ) {
	  adjustvars( 4 );	
	  addvar( names[i], VAR_STRING, 4, 4, 0 );
	  argsize += 4;
	}
      }
    } else if( accepttok( TOK_OPAQUE ) ) {
      if( accepttok( TOK_OPENARRAY ) ) {
	int size;
	size = parseconstant();
	expectok( TOK_CLOSEARRAY );
	for( i = 0; i < nnames; i++ ) {
	  adjustvars( size );
	  addvar( names[i], VAR_OPAQUEBUF, size, size, 0 );
	  argsize += size;
	}
      } else {
	for( i = 0; i < nnames; i++ ) {
	  adjustvars( 4 );
	  addvar( names[i], VAR_OPAQUE, 4, 4, 0 );
	  argsize += 4;
	}
      }
    } else usage( "Unexpected var type %s", glob.tok.token );
    
    if( !accepttok( TOK_SEMICOLON ) ) return;
  }

  glob.currentargsize = argsize;
  if( argsize > 0 ) fvmc_emit( "\tADDSP\t%u\t ; allocate locals\n", argsize );
  do {
    parsestatement();
  } while( accepttok( TOK_SEMICOLON ) );  
  if( argsize > 0 ) fvmc_emit( "\tSUBSP\t%u\t ; free locals\n", argsize );
  glob.currentargsize = 0;
}

static int parsedeclaration( void ) {
  /* 
   * const name := intval
   * procedure name()
   * var name : type
   */
  if( accepttok( TOK_CONST ) ) {
    struct token nametok;
    struct token valtok;
    
    if( glob.tok.type != TOK_NAME ) usage( "Failed to parse const statement" );
    nametok = glob.tok;    
    expecttok( TOK_NAME );
    expecttok( TOK_ASSIGN );
    valtok = glob.tok;
    if( accepttok( TOK_VALINTEGER ) ) {
      struct var *v;
      fvmc_emit( "\t.CONST\t%s\t%u\n", nametok.token, (unsigned int)strtoul( valtok.token, NULL, 0 ) );
      v = addglobal( nametok.token, VAR_INTEGER, VAR_ISCONST );
      v->offset = strtoul( valtok.token, NULL, 0 );
    } else if( accepttok( TOK_VALSTRING ) ) {
      fvmc_emit( "\t.TEXT\t%s\t%s\n", nametok.token, valtok.token );
      addglobal( nametok.token, VAR_STRINGBUF, 0 );
    } else usage( "bad constant value %s", valtok.token );
  } else if( accepttok( TOK_PROCEDURE ) ) {
    int vartype = 0;
    struct token nametok, partok;
    int nargs = 0, i, j;
    struct var *vars, *vlast, *v;

    vars = NULL;
    vlast = NULL;
    
    fvmc_printf( 1, "parsing procedure\n" );
    nametok = glob.tok;
    expecttok( TOK_NAME );
    fvmc_emit( "\n" );
    fvmc_emit( ";; ----- PROCEDURE %s(", nametok.token );
    expecttok( TOK_OPENPAREN );
    do {
      if( glob.tok.type == TOK_CLOSEPAREN ) break;

      vartype = 0;
      if( accepttok( TOK_VAR ) ) {
	/* var type parameter */
	vartype = 1;
      }
      partok = glob.tok;
      expecttok( TOK_NAME );
      expecttok( TOK_COLON );
      fvmc_emit( "%s", nargs > 0 ? ", " : "" );

      v = malloc( sizeof(*v) );
      memset( v, 0, sizeof(*v) );
      strcpy( v->name, partok.token );
      v->flags |= VAR_ISPARAM;
      if( vartype ) v->flags |= VAR_ISVAR;
      
      if( accepttok( TOK_INTEGER ) ) {
	fvmc_emit( "%s%s : INTEGER", vartype ? "VAR " : "", partok.token );
	v->type = VAR_INTEGER;	
      } else if( accepttok( TOK_STRING ) ) {
	fvmc_emit( "%s%s : STRING", vartype ? "VAR " : "", partok.token );	
	v->type = VAR_STRING;	
      } else if( accepttok( TOK_OPAQUE ) ) {
	fvmc_emit( "%s%s : OPAQUE", vartype ? "VAR " : "", partok.token );	
	v->type = VAR_OPAQUE;	
      } else usage( "unrecognized type \"%s\"", glob.tok.token );
      nargs++;
	
      if( vlast ) vlast->next = v;
      else vars = v;	
      vlast = v;
      
    } while( accepttok( TOK_COMMA ) );
    expecttok( TOK_CLOSEPAREN );
    fvmc_emit( ") ----- \n\n" );

    addproc( nametok.token, vars );
    for( i = 0; i < nargs; i++ ) {
      v = vars;
      j = 0;
      while( v ) {
	if( i == (nargs - 1 - j) ) break;
	v = v->next;
	j++;
      }

      if( !v ) usage( "Bad args" );
      addvar( v->name, v->type, 4, 8 + i*4, v->flags );
    }
    
    expectok( TOK_BEGIN );
    fvmc_emit( "%s:\n", nametok.token );    
    parseprocedurebody();
    expectok( TOK_END );
    fvmc_emit( "\tRET\n" );
    fvmc_emit( "\n" );
    freevars();
  } else if( accepttok( TOK_VAR ) ) {
    /* 
     * var name : integer
     * var name : string[len]
     */
    struct token nametok = glob.tok;
    expectok( TOK_NAME );
    expectok( TOK_COLON );
    if( accepttok( TOK_INTEGER ) ) {
      uint32_t u32 = 0;
      struct token valtok;
      if( accepttok( TOK_ASSIGN ) ) {
	valtok = glob.tok;
	expectok( TOK_VALINTEGER );
	u32 = strtoul( valtok.token, NULL, 0 );
      }
      fvmc_emit( "\t.DATA\t%s\t%u\n", nametok.token, u32 );
      addglobal( nametok.token, VAR_INTEGER, 0 );
    } else if( accepttok( TOK_STRING ) ) {
      int i;
      struct token valtok;

      if( accepttok( TOK_OPENARRAY ) ) {
	uint32_t len;
	len = parseconstant();
	expectok( TOK_CLOSEARRAY );
	fvmc_emit( "\t.DATA\t%s\t\"", nametok.token );
	if( accepttok( TOK_ASSIGN ) ) {
	  uint32_t slen;
	  valtok = glob.tok;
	  expecttok( TOK_VALSTRING );
	  slen = strlen( valtok.token + 1 ) - 1;
	  fvmc_emit("%.*s", slen > len ? len : slen, valtok.token + 1);
	  for( i = strlen( valtok.token ); i < len; i++ ) {
	    fvmc_emit( "\\0" );
	  }
	} else {
	  /* populate with 0 characters */
	  for( i = 0; i < len; i++ ) {
	    fvmc_emit( "\\0" );
	  }
	}
	
	fvmc_emit( "\"\n" );
	addglobal( nametok.token, VAR_STRINGBUF, 0 );
      } else {
	fvmc_emit( "\t.DATA\t%s\t0\n", nametok.token );
	addglobal( nametok.token, VAR_STRING, 0 );
      }
      
    } else if( accepttok( TOK_OPAQUE ) ) {
      if( accepttok( TOK_OPENARRAY ) ) {
	uint32_t val;
	val = parseconstant();
	fvmc_emit( "\t.DATA\t%s\tARRAY %u\n", nametok.token, val );
	expectok( TOK_CLOSEARRAY );
	addglobal( nametok.token, VAR_OPAQUEBUF, 0 );
      } else {
	fvmc_emit( "\t.DATA\t%s\t0\n", nametok.token );
	addglobal( nametok.token, VAR_OPAQUE, 0 );
      }
    } else usage( "Unexpected var type %s", nametok.token );
  } else if( accepttok( TOK_DECLARE ) ) {
    /* declare procedure name(params) */
    struct token nametok, partok;
    struct var *vars, *vlast, *v;
    int vartype;
    int syscalltype = 0;
    struct proc *proc;
    
    if( glob.tok.type == TOK_SYSCALL ) syscalltype = 1;
    if( accepttok( TOK_PROCEDURE ) || accepttok( TOK_SYSCALL ) ) {
      nametok = glob.tok;
      expectok( TOK_NAME );
      vars = NULL;
      vlast = NULL;
      expecttok( TOK_OPENPAREN );
      do {
	if( glob.tok.type == TOK_CLOSEPAREN ) break;

	vartype = 0;
	if( accepttok( TOK_VAR ) ) {
	  /* var type parameter */
	  vartype = 1;
	}
	partok = glob.tok;
	expecttok( TOK_NAME );
	expecttok( TOK_COLON );
	v = malloc( sizeof(*v) );
	memset( v, 0, sizeof(*v) );
	strcpy( v->name, partok.token );
	v->flags |= VAR_ISPARAM;
	if( vartype ) v->flags |= VAR_ISVAR;
	if( accepttok( TOK_INTEGER ) ) {
	  v->type = VAR_INTEGER;
	} else if( accepttok( TOK_STRING ) ) {
	  v->type = VAR_STRING;
	} else if( accepttok( TOK_OPAQUE ) ) {
	  v->type = VAR_OPAQUE;
	} else usage( "unrecognized type \"%s\"", glob.tok.token );
	
	if( vlast ) vlast->next = v;
	else vars = v;	
	vlast = v;
	
      } while( accepttok( TOK_COMMA ) );
      expectok( TOK_CLOSEPAREN );

      proc = addproc( nametok.token, vars );
      if( syscalltype ) {
	proc->flags |= PROC_SYSCALL;
	expectok( TOK_COLON );
	proc->syscallid = strtoul( glob.tok.token, NULL, 0 );
	expectok( TOK_VALINTEGER );
      }

    } else if( accepttok( TOK_VAR ) ) {
      /* declare var name : type */
      var_t type = 0;
      
      nametok = glob.tok;
      expectok( TOK_NAME );
      expectok( TOK_COLON );
      if( accepttok( TOK_INTEGER ) ) {
	type = VAR_INTEGER;
      } else if( accepttok( TOK_STRING ) ) {
	type = VAR_STRING;
      } else if( accepttok( TOK_OPAQUE ) ) {
	type = VAR_OPAQUE;
      } else usage( "Bad declare var type %s", glob.tok.token );
      
      addglobal( nametok.token, type, 0 );
    } else usage( "Unexpected declare statement \"%s\"", glob.tok.token );
  } else return 0;

  return 1;
}

static void parsestatement( void ) {
  char name[64];
  struct var *v;
  struct token tok;
  uint32_t reg;
  
  /* 
     name := expression 
     call name(expr, ... )
     if condition then statement [else statement]  
     while condition do statement 
     begin statement [ ; statement ] end 
   */
  tok = glob.tok;
  strcpy( name, glob.tok.token );
  if( accepttok( TOK_NAME ) ) {
    if( accepttok( TOK_COLON ) ) {
      fvmc_printf( 1, "parsing label statement\n" );
      fvmc_emit( "%s:\n", name );
      parsestatement();
      return;
    }
	
    fvmc_printf( 1, "parsing assignment\n" );
    expecttok( TOK_ASSIGN );
    v = getvar( name );

    /* choose a register to get the result of the expression */
    if( v && v->reg != -1 ) reg = v->reg;
    else reg = allocregister();
    reg = 0; //removeme
    parseexpr( reg );
    
    if( v ) {
      if( v->flags & VAR_ISVAR ) {
	fvmc_emit( "\tLDSP\tR1\t-%u\t ; get address of %s\n", v->offset + glob.stackoffset, v->name );
	fvmc_emit( "\tST\tR1\tR%u\t ; store into %s\n", reg, v->name );	
      } else {
	fvmc_emit( "\tSTSP\tR%u\t-%u\t ; store %s %s\n", reg, v->offset + glob.stackoffset, v->flags & VAR_ISPARAM ? "param" : "local", name );
      }
    } else {
      fvmc_emit( "\tLDI\tR1\t%s\n", name );
      fvmc_emit( "\tST\tR1\tR%u\n", reg );
    }
    assignreg( v, reg );
    assignreg( NULL, 1 );
  } else if( accepttok( TOK_CARET ) ) {
    /* ^var := expr */
    struct var *v;
    struct token nametok = glob.tok;
    expectok( TOK_NAME );
    expectok( TOK_ASSIGN );
    parseexpr( 0 );
    v = getvar( nametok.token );
    if( v ) {
      /* var must be a pointer type */
      if( (v->type != VAR_OPAQUE) ) usage( "Var type not a pointer" );
      
      if( v->flags & VAR_ISVAR ) {
	/* var already a pointer */
	fvmc_emit( "\tLDSP\tR1\t-%u\n", v->offset + glob.stackoffset );
	fvmc_emit( "\tLD\tR1\tR1\n" );
	fvmc_emit( "\tST\tR1\tR0 ; store into %s\n", v->name );	
      } else {
	fvmc_emit( "\tLDSP\tR1\t-%u ; load %s\n", v->offset + glob.stackoffset, v->name );
	fvmc_emit( "\tST\tR1\tR0 ; store into %s\n", v->name );
      }
      
    } else {
      v = getglobal( nametok.token );
      if( !v ) usage( "Unknown variable %s", nametok.token );
      
      /* assume global */
      fvmc_emit( "\tLD\tR1\t%s\n", nametok.token );
      fvmc_emit( "\tST\tR1\tR0\n" );
    }
    
  } else if( accepttok( TOK_CALL ) || accepttok( TOK_SYSCALL ) ) {
    char name[64];
    int nargs = 0;
    struct var *v, *vp;
    struct proc *p;
    struct token vartok;
    
    strcpy( name, glob.tok.token );
    p = getproc( name );
    if( !p ) usage( "Unknown procedure %s", name );
    v = p->vars;
    expecttok( TOK_NAME );
    expecttok( TOK_OPENPAREN );

    /* push args from left to right */
    glob.stackoffset = 0;
    do {
      if( v->flags & VAR_ISVAR ) {
	vartok = glob.tok;
	if( glob.tok.type != TOK_NAME ) usage( "Parameter %s is a VAR and expects a variable reference, not %s", v->name, glob.tok.token );
	expectok( TOK_NAME );
	vp = getvar( vartok.token );
	if( vp ) {
	  if( vp->flags & VAR_ISVAR ) {
	    /* already a var parameter so just pass it through */
	    fvmc_emit( "\tLDSP\tR0\t-%u\t ; arg %s\n", vp->offset + glob.stackoffset, v->name );
	  } else {
	    /* get address of the parameter */
	    fvmc_emit( "\tLEASP\tR0\t-%u\t ; arg %s\n", vp->offset + glob.stackoffset, v->name );
	  }
	} else {
	  /* assume global */
	  vp = getglobal( vartok.token );
	  if( !vp ) usage( "Unknown variable %s", vartok.token );
	  
	  fvmc_emit( "\tLDI\tR0\t%s\t ; arg %s\n", vartok.token, v->name );  
	}
      } else {
	parseexpr( 0 );
      }
      fvmc_emit( "\tPUSH\tR0\t\t ; arg %s\n", v->name );
      nargs++;
      glob.stackoffset += 4;
      v = v->next;
    } while( accepttok( TOK_COMMA ) );
    if( v ) usage( "Insufficient parameters supplied to procedure %s", name );
    expecttok( TOK_CLOSEPAREN );
    if( tok.type == TOK_SYSCALL ) fvmc_emit( "\tCALLNAT\t%u\t ; callnat %s\n", p->syscallid, p->name );
    else {
      fvmc_emit( "\tCALL\t%s\n", name );
      voidregisters();
    }
    if( nargs > 0 ) fvmc_emit( "\tSUBSP\t%u\n", nargs * 4 );
    glob.stackoffset = 0;
  } else if( accepttok( TOK_BEGIN ) ) {
    do {
      parsestatement();
    } while( accepttok( TOK_SEMICOLON ) );
    expecttok( TOK_END );
  } else if( accepttok( TOK_IF ) ) {
    char elselabel[64], endiflabel[64];
    op_t op;
    
    strcpy( elselabel, genlabel( "ELSE" ) );
    strcpy( endiflabel, genlabel( "ENDIF" ) );
    
    op = parsecondition();
    expecttok( TOK_THEN );
    printconditionjump( op, elselabel );
    parsestatement();
    fvmc_emit( "\tJMP\t%s\n", endiflabel );
    fvmc_emit( "%s:\n", elselabel );
    if( accepttok( TOK_ELSE ) ) {
      parsestatement();
    }
    fvmc_emit( "%s:\n", endiflabel );
  } else if( accepttok( TOK_WHILE ) ) {
    /* while condition do statement */
    op_t op;
    char startlabel[64], whilelabel[64], donelabel[64];
    strcpy( startlabel, genlabel( "START" ) );
    strcpy( whilelabel, genlabel( "WHILE" ) );
    strcpy( donelabel, genlabel( "DONE" ) );
    
    fvmc_emit( "%s:\n", startlabel );
    op = parsecondition();
    printconditionjump( op, whilelabel );
    fvmc_emit( "\tJMP\t%s\n", donelabel );
    fvmc_emit( "%s:\n", whilelabel );
    expectok( TOK_DO );
    parsestatement();
    fvmc_emit( "\tJMP\t%s\n", startlabel );
    fvmc_emit( "%s:\n", donelabel );

  } else if( accepttok( TOK_DO ) ) {
    /* do statement while condition */
    op_t op;
    char dolabel[64];
    strcpy( dolabel, genlabel( "DO" ) );

    fvmc_emit( "%s:\n", dolabel );
    parsestatement();
    expectok( TOK_WHILE );
    op = parsecondition();
    printconditionjump( op, dolabel );

  } else if( accepttok( TOK_GOTO ) ) {
    struct token nametok = glob.tok;
    if( nametok.type != TOK_NAME ) usage( "Failed to parse goto" );
    fvmc_emit( "\tJMP\t%s\n", nametok.token );
    expectok( TOK_NAME );
  } else if( accepttok( TOK_ASM ) ) {
    /* asm stringval */
    struct token strtok = glob.tok;
    expectok( TOK_VALSTRING );
    fvmc_emit( "\t%.*s\n", (int)strlen( strtok.token ) - 2, strtok.token + 1 );
  } else if( accepttok( TOK_RETURN ) ) {
      fvmc_emit( "\tSUBSP\t%u\t ; free locals\n", glob.currentargsize );
      fvmc_emit( "\tRET\n" );
  } else if( accepttok( TOK_SEMICOLON ) ) {
    fvmc_printf( 1, "empty statement\n" );
  } else if( glob.tok.type == TOK_END ) {
    fvmc_printf( 1, "empty statement\n" );
  } else usage( "failed to parse statement. type=%u token=\"%s\"", glob.tok.type, glob.tok.token );
}

static void parseblock( void ) {
  expecttok( TOK_BEGIN );
  
  do {
    if( !parsedeclaration() ) break;
  } while( accepttok( TOK_SEMICOLON ) );
    
  expecttok( TOK_END );
}

static void parseprogram( void ) {
  struct var *vars, *v, *next, *vlast;

  vars = NULL;
  vlast = NULL;
  
  fvmc_emit( ";;;\n"
	     ";;; This file was generated by fvmc pascal compiler\n"
	     ";;; Date: "__TIME__ " "  __DATE__ "\n" 
	     ";;; \n" );
  
  expecttok( TOK_PROGRAM );
  if( glob.tok.type == TOK_NAME ) {
    fvmc_emit( "\t.MODULE\t\t%s\n", glob.tok.token );
  }
  expecttok( TOK_NAME );
  if( accepttok( TOK_OPENPAREN ) ) {
    uint32_t pg, vs;
    struct token tok;
    
    pg = parseconstant();
    expectok( TOK_COMMA );
    vs = parseconstant();
    fvmc_emit( "\t.PROGRAM\t%u\t%u\n", pg, vs );

    while( accepttok( TOK_COMMA ) ) {
      v = malloc( sizeof(*v) );
      memset( v, 0, sizeof(*v) );
      tok = glob.tok;
      expectok( TOK_NAME );
      fvmc_printf( 1, "exporting %s\n", tok.token );
      strcpy( v->name, tok.token );
      if( vlast ) vlast->next = v;
      else vars = v;
      vlast = v;
    }
    
    expectok( TOK_CLOSEPAREN );
  }
  expecttok( TOK_SEMICOLON );

  parseblock();

  expecttok( TOK_PERIOD );

  v = vars;
  while( v ) {
    next = v->next;
    fvmc_emit( "\t.EXPORT\t\t%s\n", v->name );
    free( v );
    v = next;
  }
  
}


static int parsefile( void ) {
  if( glob.tok.type == TOK_PROGRAM ) {
    parseprogram();
  } else if( glob.tok.type == TOK_DECLARE ) {
    /* declare begin end. */
    expectok( TOK_DECLARE );
    expectok( TOK_BEGIN );

    do {
      if( glob.tok.type == TOK_END ) break;
      if( !parsedeclaration() ) usage( "Only declaration statements in toplevel declare unit" );
    } while( accepttok( TOK_SEMICOLON ) );
    
    expectok( TOK_END );
    expectok( TOK_PERIOD );
  } else usage( "Unexpected toplevel token %s", glob.tok.token );

  return 0;
}


#if 0
int main( int argc, char **argv ) {
  char *filename;
  FILE *f;
  char token[64];
  int sts, i;

  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-v" ) == 0 ) {
      glob.verbosemode++;
    } else break;
    i++;
  }

  if( i >= argc ) usage( NULL );
  filename = argv[i];
  f = fopen( filename, "r" );

  fvmc_printf( 1, " -------------- lexing -------------- \n" );
  
  /* process until . */ 
  while( 1 ) {
    sts = skipwhitespace( f );
    if( sts ) {
      fvmc_printf( 1, "end of file\n" );
      break;
    }

    sts = getnexttoken( f, token, sizeof(token) );
    if( sts ) {
      fvmc_printf( 1, "bad token\n" );
      break;
    }
    
    fvmc_printf( 1, "token \"%s\"\n", token );
    if( strcmp( token, "." ) == 0 ) {
      fvmc_printf( 1, "done\n" );
      break;
    }
  }

  fseek( f, 0, SEEK_SET );

  fvmc_printf( 1, " -------- parsing ------ \n" );

  glob.infile = f;
  nexttok();
  parseprogram();
   
  fclose( f );

  return 0;
}
#endif

int fvmc_compile( char *sourcefile, char *destfile ) {
  FILE *infile, *outfile;
  
  infile = fopen( sourcefile, "r" );
  if( !infile ) return -1;
  outfile = fopen( destfile, "w" );
  if( !outfile ) {
    fclose( infile );
    return -1;
  }
  fprintf( outfile, "\n" );
  
  glob.infile = infile;
  glob.outfile = outfile;
  nexttok();
  parsefile();
   
  fclose( infile );
  fclose( outfile );

  return 0;
}

void fvmc_pas_verbosemode( int lvl ) {
  glob.verbosemode = lvl;
}

static void fvmc_printf( int lvl, char *fmt, ... ) {
  va_list args;

  if( glob.verbosemode < lvl ) return;

  printf( ";; " );
  va_start( args, fmt );
  vprintf( fmt, args );
  va_end( args );
}

static void fvmc_emit( char *fmt, ... ) {
  va_list args;

  va_start( args, fmt );
  vfprintf( glob.outfile, fmt, args );
  va_end( args );
}
