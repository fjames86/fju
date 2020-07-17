
/*
 * Aim: take as input some text in a form of BASIC (?) and produce as output an 
 * assembly file suitable for input to fvmc. 
 * 
 * We want the following: 
 *  - types: int, string, pointer
 *  - define procedures 
 *  
 *  - associate symbolic name with location in stack 


Syntax of Mini-Pascal (Welsh & McKeag, 1980)
Syntax in recursive descent order
<program> ::= 	program <identifier> ; <block> .
<block> ::= 	<variable declaration part>
<procedure declaration part>
<statement part>
<variable declaration part> ::= 	<empty> |
var <variable declaration> ;
    { <variable declaration> ; }
<variable declaration> ::= 	<identifier > { , <identifier> } : <type>
<type> ::= 	<simple type> | <array type>
<array type> ::= 	array [ <index range> ] of <simple type>
<index range> ::= 	<integer constant> .. <integer constant>
<simple type> ::= 	<type identifier>
<type identifier> ::= 	<identifier>
<procedure declaration part> ::= 	{ <procedure declaration> ; }
<procedure declaration> ::= 	procedure <identifier> ; <block>
<statement part> ::= 	<compound statement>
<compound statement> ::= 	begin <statement>{ ; <statement> } end
<statement> ::= 	<simple statement> | <structured statement>
<simple statement> ::= 	<assignment statement> | <procedure statement> |
<read statement> | <write statement>
<assignment statement> ::= 	<variable> := <expression>
<procedure statement> ::= 	<procedure identifier>
<procedure identifier> ::= 	<identifier>
<read statement> ::= 	read ( <input variable> { , <input variable> } )
<input variable> ::= 	<variable>
<write statement> ::= 	write ( <output value> { , <output value> } )
<output value> ::= 	<expression>
<structured statement> ::= 	<compound statement> | <if statement> |
<while statement>
<if statement> ::= 	if <expression> then <statement> |
if <expression> then <statement> else <statement>
<while statement> ::= 	while <expression> do <statement>
<expression> ::= 	<simple expression> |
<simple expression> <relational operator> <simple expression>
<simple expression> ::= 	<sign> <term> { <adding operator> <term> }
<term> ::= 	<factor> { <multiplying operator> <factor> }
<factor> ::= 	<variable> | <constant> | ( <expression> ) | not <factor>
<relational operator> ::= 	= | <> | < | <= | >= | >
<sign> ::= 	+ | - | <empty>
<adding operator> ::= 	+ | - | or
<multiplying operator> ::= 	* | div | and
<variable> ::= 	<entire variable> | <indexed variable>
<indexed variable> ::= 	<array variable> [ <expression> ]
<array variable> ::= 	<entire variable>
<entire variable> ::= 	<variable identifier>
<variable identifier> ::= 	<identifier>
Lexical grammar
<constant> ::= 	<integer constant> | <character constant> | <constant identifier>
<constant identifier> ::= 	<identifier>
<identifier> ::= 	<letter> { <letter or digit> }
<letter or digit> ::= 	<letter> | <digit>
<integer constant> ::= 	<digit> { <digit> }
<character constant> ::= 	'< any character other than ' >'  |  ''''
<letter> ::= 	a | b | c | d | e | f | g | h | i | j | k | l | m | n | o |
p | q | r | s | t | u | v | w | x | y | z | A | B | C |
D | E | F | G | H | I | J | K | L | M | N | O | P
| Q | R | S | T | U | V | W | X | Y | Z
<digit> ::= 	0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<special symbol> ::= 	+ | - | * | = | <> | < | > | <= | >= |
( | ) | [ | ] | := | . | , | ; | : | .. | div | or |
and | not | if | then | else | of | while | do |
begin | end | read | write | var | array |
procedure | program
<predefined identifier> ::= 	integer | Boolean | true | false 

 */


/*

Minipascal might be a good option:

program name

begin procedure name([var] name:type)
end

if statement then
else
endif

var name : type




var fred : int
var fred : array[15] of int 



 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>

static char whitespacechars[] = { ' ', '\n', '\t', '\r', '\0' };
static char terminalchars[] = { '+', '-', '*', '/', ';', '.', '{', '}', ' ', '\n', '\t', '\r', '(', ')', ':', '[', ']', '\0' };

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
      printf( ";; skipwhitespace EOF\n" );
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
      printf( ";; skipping comment\n" );
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
  
  p = token;
  *p = '\0';
  while( 1 ) {
    c = fgetc( f );
    if( c == EOF ) {
      printf( ";; getnexttoken EOF\n" );
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
	  printf( ";; skipping comment\n" );
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
      printf( ";; token \"%s\" too large\n", token );
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
  
  printf( "fvmc OPTIONS file...\n"
	  "\n" );
  if( fmt ) {
    va_start( args, fmt );
    printf( "Error: " );
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
	      TOK_ELSEIF,
	      TOK_ENDIF,
	      TOK_PROCEDURE,
	      TOK_BEGIN,
	      TOK_END,
	      TOK_VAR,
	      TOK_INTEGER,
	      TOK_STRING,
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
		     { "ELSEIF", TOK_ELSEIF },
		     { "ELSE", TOK_ELSE },
		     { "ENDIF", TOK_ENDIF },
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
		     { "[", TOK_OPENARRAY },
		     { "]", TOK_CLOSEARRAY },
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
		     { "!=", TOK_NEQ },
		     
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

struct var {
  struct var *next;
  char name[64];
  uint32_t type;
  uint32_t offset;
};

static struct {
  struct token tok;
  FILE *f;
  struct var *vars;
} glob;

static int nexttok( void ) {
  struct token tok;
  int sts;

  sts = getnexttoken( glob.f, tok.token, sizeof(tok.token) );
  if( sts ) return sts;

  tok.type = gettokentype( tok.token );
  glob.tok = tok;
  printf( ";; token %u %s\n", tok.type, tok.token );
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
  usage( "Unexpected symbol %u %s", glob.tok.type, glob.tok.token );
  return 0;
}

static struct var *getvar( char *name ) {
  struct var *v;
  v = glob.vars;
  while( v ) {
    if( strcasecmp( v->name, name ) == 0 ) return v;
    v = v->next;
  }
  return NULL;
}

/* ----- */

static void parseblock( void );
static void parseexpr( int reg );
static void parsecondition( void );
static void parsestatement( void );
static void parseprogram( void );

static char *genlabel( char *prefix ) {
  static char labelname[64];
  static int labelidx;

  sprintf( labelname, "L-%s-%u", prefix ? prefix : "", ++labelidx );
  return labelname;
}


static void parsefactor( int reg ) {
  /* variable | literal | ( expression ) | not factor */
  if( accepttok( TOK_NAME ) ) {
    /* varaiable */
  } else if( accepttok( TOK_VALINTEGER ) ) {
    /* literal integer */
  } else if( accepttok( TOK_STRING ) ) {
    /* literal string */
  } else if( accepttok( TOK_OPENPAREN ) ) {
    parseexpr( reg );
    expecttok( TOK_CLOSEPAREN );
  } else if( accepttok( TOK_NOT ) ) {
    /* not factor */
    parsefactor( reg );
    printf( "\tNOT\tR%u\n", reg );
  } else usage( "Unable to parse factor" );  

}

static void parseterm( void ) {
  parsefactor( 0 );
  if( accepttok( TOK_MUL ) ) {
    parsefactor( 1 );
    printf( "\tMUL\tR0\tR1\n" );
  } else if( accepttok( TOK_DIV ) ) {
    parsefactor( 1 );
    printf( "\tDIV\tR0\tR1\n" );    
  } else if( accepttok( TOK_MOD ) ) {
    parsefactor( 1 );
    printf( "\tMOD\tR0\tR1\n" );
  } else if( accepttok( TOK_AND ) ) {
    parsefactor( 1 );
    printf( "\tAND\tR0\tR1\n" );
  } 
}

static void parseexpr( int reg ) {
  struct token tok;
  struct var *v;
  
  tok = glob.tok;
  if( accepttok( TOK_VALINTEGER ) ) {
    printf( "\tLDI\tR%u\t%u\n", reg, (int)strtoul( tok.token, NULL, 0 ) );
  } else if( accepttok( TOK_NAME ) ) {
    v = getvar( tok.token );
    if( v ) printf( "\tLEASP\tR%u\t-%u\n", reg, v->offset );
    else {
      printf( "\tLD\tR%u\t%s\n", reg, tok.token );
    }
  } else usage( "Unable to parse expression" );
  
}
static void parsecondition( void ) {
  int op = 0;
  
  parseexpr( 0 );  
  if( accepttok( TOK_EQ ) ) {
    op = 1; /* eq */
  } else if( accepttok( TOK_GT ) ) {
    op = 2;
  } else if( accepttok( TOK_GTE ) ) {
    op = 3;
  } else if( accepttok( TOK_LT ) ) {
    op = 4;
  } else if( accepttok( TOK_LTE ) ) {
    op = 5;
  } else if( accepttok( TOK_NEQ ) ) {
    op = 6;
  } else usage( "Bad comparison operator" );
  parseexpr( 1 );
  printf( "\tSUB\tR0\tR1\n" );
  /*XXX TODO */
}

static void parsestatement( void ) {
  char name[64];
  struct var *v;
  
  /* 
     name := expression 
     call name(expr, ... )
     if condition then body [elseif condition then body] [else body] endif 
     while condition do statement 
     begin statement [ ; statement ] end 
   */
  strcpy( name, glob.tok.token );
  if( accepttok( TOK_NAME ) ) {
    printf( ";; parsing assignment\n" );
    expecttok( TOK_COLON );
    expecttok( TOK_EQ );
    parseexpr( 0 );
    /* R0 contains result of expression */
    v = getvar( name );
    if( v ) printf( "\tLEASP\tR1\t-%u\n", v->offset );
    else printf( "\tLDI\tR1\t%s\n", name );
    printf( "\tST\tR1\tR0\n" );
  } else if( accepttok( TOK_CALL ) ) {
    char name[64];
    int nargs = 0;
    strcpy( name, glob.tok.token );
    expecttok( TOK_NAME );
    expecttok( TOK_OPENPAREN );
    do {
      parseexpr( 0 );
      printf( "\tPUSH\tR0\n" );
      nargs++;
    } while( accepttok( TOK_COMMA ) );
    expecttok( TOK_CLOSEPAREN );
    printf( "\tCALL\t%s\n", name );
    printf( "\tSUBSP\t%u\n", nargs * 4 );
  } else if( accepttok( TOK_BEGIN ) ) {
    do {
      parsestatement();
    } while( accepttok( TOK_SEMICOLON ) );
    expecttok( TOK_END );
  } else if( accepttok( TOK_IF ) ) {
    char elselabel[64], endiflabel[64];
    strcpy( elselabel, genlabel( "ELSE" ) );
    strcpy( endiflabel, genlabel( "ENDIF" ) );
    
    parsecondition();
    expecttok( TOK_THEN );
    printf( "\tJPN\t%s\n", elselabel );
    parsestatement();
    printf( "\tJMP\t%s\n", endiflabel );
    printf( "%s:\n", elselabel );    
    while( accepttok( TOK_ELSEIF ) ) {
      strcpy( elselabel, genlabel( "ELSE" ) );
      
      parsecondition();
      expecttok( TOK_THEN );
      printf( "\tJPN\t%s\n", elselabel );      
      parsestatement();
      printf( "\tJMP\t%s\n", endiflabel );      
      printf( "%s:\n", elselabel );
    }
    if( accepttok( TOK_ELSE ) ) {
      parsestatement();
    }
    printf( "%s:\n", endiflabel );
    expecttok( TOK_ENDIF );
  } else if( accepttok( TOK_PROCEDURE ) ) {
    int vartype = 0;
    
    printf( ";; parsing procedure\n" );
    strcpy( name, glob.tok.token );
    expecttok( TOK_NAME );
    printf( "%s:\n", name );
    expecttok( TOK_OPENPAREN );
    do {
      if( accepttok( TOK_VAR ) ) {
	/* var type parameter */
	vartype = 1;
      }
      strcpy( name, glob.tok.token );
      expecttok( TOK_NAME );
      expecttok( TOK_COLON );
      if( accepttok( TOK_INTEGER ) ) {
      } else if( accepttok( TOK_STRING ) ) {
      } else usage( "unrecognized type \"%s\"", glob.tok.token );
    } while( accepttok( TOK_COMMA ) );
    expecttok( TOK_CLOSEPAREN );
    parseblock();    
    printf( "\tRET\n" );
  } else usage( "failed to parse statement. token=\"%s\"", glob.tok.token );
}

static void parseblock( void ) {
  expecttok( TOK_BEGIN );
  do {
    parsestatement();
  } while( accepttok( TOK_SEMICOLON ) );
  expecttok( TOK_END );
}

static void parseprogram( void ) {
  int sts;
  
  expecttok( TOK_PROGRAM );
  if( glob.tok.type == TOK_NAME ) {
    printf( "\t.MODULE\t%s\n", glob.tok.token );
  }
  expecttok( TOK_NAME );
  expecttok( TOK_SEMICOLON );

  parseblock();

  expecttok( TOK_PERIOD );
}



int main( int argc, char **argv ) {
  char *filename;
  FILE *f;
  char token[64];
  int sts;
  
  filename = argv[1];
  f = fopen( filename, "r" );

  printf( ";; -------------- lexing -------------- \n" );
  
  sts = getnexttoken( f, token, sizeof(token) );
  if( sts ) usage( "Failed to read first token" );

  if( strcasecmp( token, "PROGRAM" ) != 0 ) {
    usage( "First token \"%s\" expected to be PROGRAM", token );
  }

  sts = getnexttoken( f, token, sizeof(token) );
  if( sts ) usage( "Bad token" );
  sts = getnexttoken( f, token, sizeof(token) );
  if( sts ) usage( "Bad token" );
  if( strcmp( token, ";" ) != 0 ) usage( "Expected ; got %s\n", token );
  
  /* process until . */ 
  while( 1 ) {
    sts = skipwhitespace( f );
    if( sts ) {
      printf( ";; end of file\n" );
      break;
    }

    sts = getnexttoken( f, token, sizeof(token) );
    if( sts ) {
      printf( "bad token\n" );
      break;
    }
    
    printf( ";; token \"%s\"\n", token );
    if( strcmp( token, "." ) == 0 ) {
      printf( ";; done\n" );
      break;
    }
  }

  fseek( f, 0, SEEK_SET );

  printf( ";; -------- parsing ------ \n" );

  glob.f = f;
  nexttok();
  parseprogram();
   
  fclose( f );

  return 0;
}
