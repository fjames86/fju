
/*
 * TODO:
 * 
 */
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <WinSock2.h>
#include <Windows.h>
#define strcasecmp _stricmp
#else
#include <arpa/inet.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>
#include <time.h>

#include <fju/sec.h>

#include "fvm-private.h"

int fvmc_compile( char *sourcefile, char *destfile );
void fvmc_pas_verbosemode( int lvl );

static FILE *openoutfile;

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "fvmc OPTIONS file...\n"
	  "\n"
	  "\n OPTIONS:\n"
	  "   -o outfile\n"
	  "   -v verbmose mode\n"
	  "   -d Disassemble\n"
	  "   -I include path\n"
	  "\n" );
  if( fmt ) {
    va_start( args, fmt );
    printf( "Assembler Error: " );
    vprintf( fmt, args );
    va_end( args );
    printf( "\n" );
  }

  if( openoutfile ) fclose( openoutfile );
  exit( 1 );
}

/*
 * TODO:
 * - On first pass, save data entries so we can emit them immediately before the 2nd pass (this avoids the weird middle pass).
 * - General tidy up required. 
 * - parse_directive is far too large.
 * - do we need the export symbol size/type stuff anymore?
 *
 * - First pass: collect labels, data entries (i.e. parse directives, check opcodes are valid but don't emit).
 * - Then Emit header. 
 * - Then Emit data segment (we saved the entries in the first pass).
 * - Second pass: emit text segment (opcodes, text data). Still need to parse directives for e.g. .includes?
 */

/* -------------------- structs ----------------------- */

struct label {
  struct label *next;
  
  char name[64];
  uint32_t addr;
  uint32_t symflags;
  uint32_t flags;
#define LABEL_EXPORT 0x01
  uint32_t exportidx;
};

struct searchpath {
  struct searchpath *next;
  char *path;
};

struct ifclause {
  struct ifclause *next;

  int skip;
  char name[64];
};

struct macroline {
  struct macroline *next;
  char line[1024];
};

struct defmacro {
  struct defmacro *next;

  char name[64];  
  struct macroline *lines;
};

struct constdef {
  struct constdef *next;
  char name[64];
  uint32_t val;
};

/* store things here */
static struct {
  char modulename[64];
  uint32_t progid;
  uint32_t versid;
  int verbosemode;
  struct label *labels;
  int datasize;
  struct searchpath *searchpaths;
  int exportidx;
  char *currentfile;
  int currentfileidx;
  int currentline;
  struct ifclause *ifclause;
  struct defmacro *macros;
  struct constdef *constdefs;
} glob;


static void fvmc_printf( int lvl, char *fmt, ... ) {
  va_list args;

  if( glob.verbosemode < lvl ) return;
  
  va_start( args, fmt );
  vprintf( fmt, args );
  va_end( args );
}

static int encode_inst( char *str, uint32_t *opcode, uint32_t addr, int firstpass );
static int parse_directive( char *buf, uint32_t *addr, FILE *f, int datasegment );
static void emit_header( FILE *f, uint32_t addr );
static void disassemble( char *filename );
static int emit_opcode( char *buf, uint32_t *addr, int passid, FILE *outfile );

static char *skipwhitespace( char *p ) {
  while( *p == ' ' || *p == '\t' ) p++;
  return p;
}

static char *copytoken( char *p, char *dest ) {
  char *q;
  int i;
  
  p = skipwhitespace( p );
  
  q = dest;
  i = 0;
  while( (i < FVM_MAX_NAME) && (*p != '\0') && (*p != ' ') && (*p != '\t') ) {
    *q = *p;
    p++;
    q++;
    i++;
  }
  *q = '\0';
  if( i == FVM_MAX_NAME ) usage( "Name too long" );
  
  return p;
}

static struct label *addlabel( char *name, uint32_t addr ) {
  char lname[256];
  struct label *l, *prev = NULL;

  if( name[strlen( name ) - 1] == '$' ) {
    sprintf( lname, "%s%u", name, glob.currentfileidx );
  } else {
    sprintf( lname, "%s", name );
  }
	     
  l = glob.labels;
  while( l ) {
    if( strcasecmp( l->name, lname ) == 0 ) {
      usage( ";; duplicate label %s\n", lname );
      return NULL;
    }
    prev = l;
    l = l->next;
  }

  fvmc_printf( 2, ";; adding label %s addr %x\n", lname, addr );
  l = malloc( sizeof(*l) );
  memset( l, 0, sizeof(*l) );
  strcpy( l->name, lname );
  l->addr = addr;
  l->next = NULL;
  if( prev ) prev->next = l;
  else glob.labels = l;    
  return l;
}
static struct label *getlabel( char *name ) {
  char lname[256];
  struct label *l;
  
  if( name[strlen( name ) - 1] == '$' ) {
    sprintf( lname, "%s%u", name, glob.currentfileidx );
  } else {
    sprintf( lname, "%s", name );
  }

  l = glob.labels;
  while( l ) {
    if( strcasecmp( l->name, lname ) == 0 ) return l;
    l = l->next;
  }
  return NULL;
}
static uint32_t getlabeladdr( char *name ) {
  struct label *l;
  l = getlabel( name );
  return l ? l->addr : 0;
}
static int exportlabel( char *name, uint32_t symflags ) {
  struct label *l;
  l = getlabel( name );
  if( l ) {
    if( !(l->flags & LABEL_EXPORT) ) {
      l->exportidx = glob.exportidx;
      glob.exportidx++;
    }
    
    l->flags |= LABEL_EXPORT;
    l->symflags |= symflags;
    return 0;
  }
  return -1;
}


static struct constdef *getconst( char *name ) {
  struct constdef *c;
  c = glob.constdefs;
  while( c ) {
    if( strcasecmp( c->name, name ) == 0 ) return c;
    c = c->next;
  }
  return NULL;
}

static struct constdef *addconst( char *name, uint32_t val ) {
  struct constdef *c;
  c = getconst( name );
  if( c ) {
    if( c->val == val ) {
      fvmc_printf( 2, ";; Redefining constant %s with same value %u\n", name, val );
      return c;
    }
    usage( "Attempt to redefine constant %s with new value %u\n", name, val );
  }

  c = malloc( sizeof(*c) );
  memset( c, 0, sizeof(*c) );
  strcpy( c->name, name );
  c->val = val;
  c->next = glob.constdefs;
  glob.constdefs = c;
  return c;
}

static void undefconst( char *name ) {
  struct constdef *c, *prev;
  prev = NULL;
  c = glob.constdefs;
  while( c ) {
    if( strcasecmp( c->name, name ) == 0 ) {
      if( prev ) prev->next = c->next;
      else glob.constdefs = c->next;
      free( c );
      return;
    }
    prev = c;
    c = c->next;
  }

}

static void resetconsts( void ) {
  struct constdef *c, *next;
  c = glob.constdefs;
  while( c ) {
    next = c->next;
    free( c );
    c = next;
  }
  glob.constdefs = NULL;
}



static FILE *opensourcefile( char *name, int firstpass ) {
  static char path[256];
  
  FILE *f;
  struct searchpath *sp;
  int i;

  for( i = strlen( name ) - 1; i >= 0; i-- ) {
    if( (name[i] == '.') && (strcasecmp( &name[i], ".pas" ) == 0) ) {
      /* compile pascal file first */
      sprintf( path, "%.*s.asm", i, name );
      if( firstpass ) {
	fvmc_pas_verbosemode( glob.verbosemode );	
	fvmc_compile( name, path );
      }
      
      f = fopen( path, "r" );
      if( !f ) usage( "Failed to compile file?" );
      glob.currentfileidx++;      
      return f;
    }
  }

   
  f = fopen( name, "r" );
  if( f ) {
    glob.currentfileidx++;
    return f;
  }
  
  sp = glob.searchpaths;
  while( sp ) {
    sprintf( path, "%s/%s", sp->path, name );
    fvmc_printf( 2, "Trying to open \"%s\"\n", path );
    f = fopen( path, "r" );
    if( f ) {
      glob.currentfile = path;
      glob.currentfileidx++;
      return f;
    }
    sp = sp->next;
  }
  usage( "Failed to open file \"%s\"", name );
  return NULL;
}


static void parse_file( FILE *f, uint32_t *addr, int passid, FILE *outfile ) {
  char buf[1024];
  char *p;

  glob.currentline = 0;
  memset( buf, 0, sizeof(buf) );
  while( fgets( buf, sizeof(buf) - 1, f ) ) {
    glob.currentline++;
    
    p = buf;
    while( *p != '\0' ) {
      if( *p == '\n' ) *p = '\0';
      p++;
    }
    
    p = buf;
    p = skipwhitespace( p );
    if( *p == '\0' ) continue;

    if( parse_directive( buf, addr, passid == 0 ? NULL : outfile, passid == 1 ? 1 : 0 ) == 0 ) continue;

    if( glob.ifclause && glob.ifclause->skip ) continue;

    emit_opcode( buf, addr, passid, outfile );
  }

  if( glob.ifclause ) usage( "Unexpected end of file waiting for terminating .ENDIF clause" );
  
}

static int emit_opcode( char *buf, uint32_t *addr, int passid, FILE *outfile ) {
  uint32_t opcode;
  
  if( passid != 1 ) {
    fvmc_printf( 2, ";; attempting to encode line \"%s\"\n", buf );
    if( encode_inst( buf, &opcode, FVM_ADDR_TEXT + *addr, passid == 0 ? 1 : 0 ) != -1 ) {
      if( passid == 2 ) {
	fvmc_printf( 2, ";; encoding \"%s\" = %08x\n", buf, opcode );
	opcode = htonl( opcode );
	fwrite( &opcode, 1, 4, outfile );
      } else {
	fvmc_printf( 1, ";; emitting (%u) %04x %s\n", *addr, *addr, buf );	
	*addr += 4;
      }
    } else {
      fvmc_printf( 2, ";; Failed to encode line\n" );
    }
      
  }

  return 0;
}

int main( int argc, char **argv ) {
  FILE *f, *outfile;
  char outfilename[256];
  int i, j, starti;
  uint32_t addr;
  int disass = 0;
  struct searchpath *sp;
  
  strcpy( outfilename, "" );
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-o" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      strncpy( outfilename, argv[i], 255 );
    } else if( strcmp( argv[i], "-v" ) == 0 ) {
      glob.verbosemode++;
    } else if( strcmp( argv[i], "-d" ) == 0 ) {
      disass = 1;
    } else if( strcmp( argv[i], "-I" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      sp = malloc( sizeof(*sp) );
      sp->path = argv[i];
      sp->next = glob.searchpaths;
      glob.searchpaths = sp;
    } else if( strcmp( argv[i], "-h" ) == 0 ) {
      usage( NULL );
    } else if( strcmp( argv[i], "--help" ) == 0 ) {
      usage( NULL );
    } else break;
    i++;
  }

  fvmc_pas_verbosemode( glob.verbosemode );
  
  addr = 0;

  fvmc_printf( 1, "\n ------ first pass ------- \n" );

  /* first pass collects labels and computes offsets. does not emit opcodes */
  glob.currentfileidx = 0;
  starti = i;
  while( i < argc ) {
    if( disass ) {
      disassemble( argv[i] );
      i++;
      continue;
    }
    
    f = opensourcefile( argv[i], 1 );
    parse_file( f, &addr, 0, NULL );    
    fclose( f );
    i++;
  }

  if( disass ) exit( 0 );
  
  fvmc_printf( 1, "\n ------ label table ------- \n" );
  {
    struct label *l;
    j = 0;
    l = glob.labels;
    while( l ) {
      fvmc_printf( 1, ";; Label %-4d %-16s = 0x%08x Flags %x SymFlags 0x%08x\n", j, l->name, l->addr, l->flags, l->symflags );
      j++;
      l = l->next;
    }
  }
  
  fvmc_printf( 1, "\n ------ second pass ------- \n" );

  /* second pass emits output */
  if( !outfilename[0] ) {
    strcpy( outfilename, "out.fvm" );
  }

  fvmc_printf( 1, ";; Opening outfile \"%s\"\n", outfilename );
  outfile = fopen( outfilename, "w" );  
  if( !outfile ) usage( "Failed to open outfile \"%s\"", outfilename );
  openoutfile = outfile;
  
  /* write header */
  emit_header( outfile, addr );

  resetconsts();
  glob.currentfileidx = 0;      
  i = starti;
  while( i < argc ) {
    f = opensourcefile( argv[i], 0 );
    parse_file( f, &addr, 1, outfile );
    fclose( f );
    i++;
  }

  resetconsts();
  glob.currentfileidx = 0;
  i = starti;
  while( i < argc ) {
    f = opensourcefile( argv[i], 0 );
    parse_file( f, &addr, 2, outfile );
    fclose( f );
    i++;
  }
  
  fclose( outfile );
    
  return 0;
}

/* -------------------- directives ---------------------- */

static char *parse_escaped_string( char *qq, FILE *f ) {
  char c;
  
  while( *qq != '"' && *qq != '\0' ) {
    if( *qq == '\\' ) {
      qq++;
      c = *qq;
      switch( c ) {
      case '0':
	c = '\0'; break;
      case 'a':
	c = '\a'; break;
      case 'b':
	c = '\b'; break;
      case 'f':
	c = '\f'; break;
      case 'n':
	c = '\n'; break;
      case 'r':
	c = '\r'; break;
      case 't':
	c = '\t'; break;
      case 'v':
	c = '\v'; break;
      case 'o':
	// octal
	qq++;
	c = (*qq - '0');
	qq++;
	c <<= 3; 
	c |= (*qq - '0');
	qq++;
	c <<= 3;
	c |= (*qq - '0');
	break;
      case 'x':
	// hex
	qq++;
	c = 0;
	if( *qq >= 'a' && *qq <= 'f' ) c = 10 + *qq - 'a';
	else if ( *qq >= 'A' && *qq <= 'F' ) c = 10 + *qq - 'A';
	else if ( *qq >= '0' && *qq <= '9' ) c = *qq - '0';
	qq++;
	c <<= 4;
	if( *qq >= 'a' && *qq <= 'f' ) c |= 10 + *qq - 'a';
	else if ( *qq >= 'A' && *qq <= 'F' ) c |= 10 + *qq - 'A';
	else if ( *qq >= '0' && *qq <= '9' ) c |= *qq - '0';		  
	break;
      }
      fvmc_printf( 3, "writing char %c\n", c );
      fwrite( &c, 1, 1, f );
    } else {
      fvmc_printf( 3, "writing char %c\n", *qq );
      fwrite( qq, 1, 1, f );
    }
    qq++;
  }

  return qq;
}

static int parse_directive( char *buf, uint32_t *addr, FILE *f, int datasegment ) {
  char *p;
  char directive[64], name[64];
  uint32_t size;
  
  p = buf;
  p = skipwhitespace( p );
  if( *p == '\0' ) return -1;
  if( *p != '.' ) return -1;
  
  /* assembler directive */
  memset( directive, 0, sizeof(directive) );
  p = copytoken( p, directive );

  if( strcasecmp( directive, ".ENDIF" ) == 0 ) {
    struct ifclause *ifc;
    
    if( !glob.ifclause ) usage( "Unexpected .ENDIF clause" );
    ifc = glob.ifclause;
    glob.ifclause = ifc->next;
    free( ifc );
    return 0;    
  }

  if( glob.ifclause && glob.ifclause->skip ) return 0;
  
  if( (strcasecmp( directive, ".data" ) == 0) ||
      (strcasecmp( directive, ".text" ) == 0) ) {
    /* reserve some bytes in the data/text segment */
    p = skipwhitespace( p );
    if( *p == '\0' ) usage( "Expected label identifier" );
    
    memset( name, 0, sizeof(name) );
    p = copytoken( p, name );
    
    if( f == NULL ) addlabel( name, strcasecmp( directive, ".data" ) == 0 ? FVM_ADDR_DATA + glob.datasize : FVM_ADDR_TEXT + *addr );

    while( 1 ) {
      /* data can be either a space separated list of numbers or a string */
      p = skipwhitespace( p );
      if( *p == '\0' ) return 0;

      {
	struct label *l = getlabel( name );
	if( l ) l->symflags |= FVM_SYMBOL_STRING;
      }
      
      size = 0;
      if( *p == '"' ) {
	p++;
	char *startp = p;
	
	/* string */
	size = 4; /* string length header */
	while( *p != '"' ) {
	  if( *p == '\0' ) usage( "Unexpected end of line, expected closing \"" );
	  if( *p == '\\' ) { 
	    p++;
	  }
	  
	  size++;
	  p++;
	}
	p++;
	  
	fvmc_printf( 3, "String \"%.*s\" length %u\n", size - 4, startp, size - 4 );

	if( f != NULL ) {	  
	  uint32_t s = size - 4;
	  if( (datasegment && (strcasecmp( directive, ".data" ) == 0)) || 
	      (!datasegment && (strcasecmp( directive, ".text" ) == 0)) ) {
	    
	    s = htonl( s );
	    fvmc_printf( 3, ";; writing string length %u\n", size - 4 );
	    fwrite( &s, 1, 4, f );

	    parse_escaped_string( startp, f );
	    if( size % 4 ) {	      
	      uint8_t tmp[4];
	      fvmc_printf( 3, "adding string padding strlen=%u padding=%u\n", size, 4 - (size % 4) );
	      memset( tmp, 0, 4 );
	      fwrite( tmp, 1, 4 - (size % 4), f );
	      size += 4 - (size % 4);
	    }
	  }
	}

	if( size % 4 ) {
	  fvmc_printf( 2, "size %u not multiple of 4, adding padding\n", size );
	  size += 4 - (size % 4);
	}

      } else {
	/* integer or label always 4 bytes */
	char numstr[64];
        int isarray = 0;

	/* integer literal, label identifier or ARRAY */
	memset( numstr, 0, sizeof(numstr) );
	p = copytoken( p, numstr );

	/* if value==ARRAY next arg is size of array */
	if( strcasecmp( numstr, "ARRAY" ) == 0 ) {
	  memset( numstr, 0, sizeof(numstr) );
	  p++;
	  p = skipwhitespace( p );

	  p = copytoken( p, numstr );
	  size = strtoul( numstr, NULL, 0 );
	  if( size % 4 ) {
	    size += 4 - (size % 4);
	  }
          isarray = 1;
	} else {	
	  size = 4;
	}
	
	if( f != NULL ) {
	  char *term;
	  uint32_t x;
	  
	  x = strtoul( numstr, &term, 0 );
	  if( *term ) {
	    // not a number, try a label
	    if( numstr[0] == '$' ) {
	      x = getlabeladdr( numstr + 2 );
	      if( numstr[1] == '-' ) {
		if( strcasecmp( directive, ".data" ) == 0 ) x = glob.datasize - x;
		else x = x - (*addr + 4);
	      } else if( numstr[1] == '+' ) {
		if( strcasecmp( directive, ".data" ) == 0 ) x = glob.datasize + x;
		else x = x + (*addr + 4);
	      } else usage( "Unknown operator %c", numstr[1] );
	    } else {
	      struct label *l = getlabel( numstr );
	      if( l ) {
		x = l->addr;
	      } else {
		struct constdef *c = getconst( numstr );
		if( !c ) usage( "Unknown label or constant %s", numstr );
		x = c->val;
	      }
	    }
	  }
	  
	  if( (datasegment && (strcasecmp( directive, ".data" ) == 0)) || 
	      (!datasegment && (strcasecmp( directive, ".text" ) == 0)) ) {
            if( isarray ) {
              char *b = malloc( size );
              memset( b, 0, size );
	      fvmc_printf( 3, "writing buffer size %u\n", size );
              fwrite( b, 1, size, f );
	      free( b );
             } else {
  	       x = htonl( x );
	       fvmc_printf( 3, "writing uint32 %x\n", x );
	       fwrite( &x, 1, 4, f );
             }
	  }
	  
	}
      }

      if( f == NULL ) {
	if( size % 4 ) {
	  usage( "size %u not multiple of 4", size );
	}
	
	if( strcasecmp( directive, ".data" ) == 0 ) {
	  glob.datasize += size;
	} else {
	  *addr = *addr + size;
	}

	{
	  struct label *l = getlabel( name );
	  l->symflags |= size;
	}
				      
      }
    }
    return 0;
  } else if( (strcasecmp( directive, ".DEFINE" ) == 0) || (strcasecmp( directive, ".CONST" ) == 0) ) {
    char str[64];

    p = skipwhitespace( p );
    if( *p == '\0' ) usage( "Expected identifier" );
    
    memset( name, 0, sizeof(name) );
    p = copytoken( p, name );

    p = skipwhitespace( p );
    if( *p == '\0' ) strcpy( str, "0" );
    else {
      memset( str, 0, sizeof(str) );
      p = copytoken( p, str );
    }
    
    addconst( name, strtoul( str, NULL, 0 ) );
    
    return 0;    
  } else if( strcasecmp( directive, ".EXPORT" ) == 0 ) {
    uint32_t symflags;
    char symtype[64];
    
    p = skipwhitespace( p );
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    p = copytoken( p, name );

    symflags = 0;
    
    p = skipwhitespace( p );
    memset( symtype, 0, sizeof(symtype) );
    p = copytoken( p, symtype );

    if( strcasecmp( symtype, "PROC" ) == 0 ) symflags |= FVM_SYMBOL_PROC;
    else if( strcasecmp( symtype, "STRING" ) == 0 ) symflags |= FVM_SYMBOL_STRING;
    else if( strcasecmp( symtype, "UINT32" ) == 0 ) {
      symflags |= FVM_SYMBOL_UINT32;
      symflags |= 0x0004;
    } else if( strcasecmp( symtype, "UINT64" ) == 0 ) {
      symflags |= FVM_SYMBOL_UINT64;
      symflags |= 0x0008;
    }

    p = skipwhitespace( p );
    memset( symtype, 0, sizeof(symtype) );
    p = copytoken( p, symtype );

    if( symtype[0] ) {
      symflags &= 0xffff0000;
      symflags |= (getlabeladdr( symtype ) - getlabeladdr( name )) & 0xffff;
    }

    exportlabel( name, symflags );

    return 0;
  } else if( strcasecmp( directive, ".MODULE" ) == 0 ) {
    p = skipwhitespace( p );
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    p = copytoken( p, name );

    fvmc_printf( 1, ";; setting modulename to %s\n", name );
    strcpy( glob.modulename, name );

    return 0;
  } else if( strcasecmp( directive, ".PROGRAM" ) == 0 ) {

    p = skipwhitespace( p );
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    p = copytoken( p, name );

    glob.progid = strtoul( name, NULL, 0 );

    p = skipwhitespace( p );
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    p = copytoken( p, name );

    glob.versid = strtoul( name, NULL, 0 );
    
    return 0;
  } else if( strcasecmp( directive, ".INCLUDE" ) == 0 ) {
    /* include path */
    FILE *incfile;
    char *q;
    char labelname[64];
    struct constdef *addedconsts = NULL, *ac, *nextac;
    
    p = skipwhitespace( p );
    if( *p == '\0' ) usage( "Unexpected end of line" );

    if( *p != '"' ) usage( "expected .INCLUDE \"path\"" );
    q = name;
    memset( name, 0, sizeof(name) );
    p++;
    while( *p != '"' ) {
      if( *p == '\0' ) usage( "Unexpected end of line" );
      if( *p == '\\' ) {
	p++;
      }
      *q = *p;
      p++;
      q++;
    }

    p++;
    p = skipwhitespace( p );
    while( *p ) {
      p = copytoken( p, labelname );
      addconst( labelname, 0 );

      ac = malloc( sizeof(*ac) );
      memset( ac, 0, sizeof(*ac) );
      strcpy( ac->name, labelname );
      ac->next = addedconsts;      
      addedconsts = ac;
    }
    

    incfile = opensourcefile( name, f == NULL ? 1 : 0 );
    if( !incfile ) usage( "failed to open include file \"%s\"", name );
    fvmc_printf( 2, "including \"%s\"\n", name );
    parse_file( incfile, addr, f == NULL ? 0 : datasegment ? 1 : 2, f );
    fclose( incfile );

    ac = addedconsts;
    while( ac ) {
      nextac = ac->next;
      undefconst( ac->name );      
      free( ac );
      ac = nextac;
    }
    
    return 0;
  } else if( (strcasecmp( directive, ".IFDEF" ) == 0) || (strcasecmp( directive, ".IFNDEF" ) == 0 ) ) {
    /* possibly enter new if clause */
    int ifdef = 0, skip;
    struct ifclause *ifc;
    
    p = skipwhitespace( p );
    if( *p == '\0' ) usage( "Expected label identifier" );
    
    memset( name, 0, sizeof(name) );
    p = copytoken( p, name );

    ifdef = (strcasecmp( directive, ".IFDEF" ) == 0);
    if( ifdef ) {
      skip = getconst( name ) ? 0 : 1;
    } else {
      skip = getconst( name ) ? 1 : 0;
    }

    fvmc_printf( 1, "adding if clause name=%s skip=%u\n", name, skip );
    
    ifc = malloc( sizeof(*ifc) );
    memset( ifc, 0, sizeof(*ifc) );
    strcpy( ifc->name, name );
    ifc->skip = skip;
    ifc->next = glob.ifclause;
    glob.ifclause = ifc;
    
    return 0;
  } else if( strcasecmp( directive, ".ENDIF" ) == 0 ) {
    struct ifclause *ifc;
    
    if( !glob.ifclause ) usage( "Unexpected .ENDIF clause" );
    ifc = glob.ifclause;
    glob.ifclause = ifc->next;
    free( ifc );
    return 0;
  } else if( strcasecmp( directive, ".UNDEF" ) == 0 ) {
    p = skipwhitespace( p );
    if( *p == '\0' ) usage( "Expected label identifier" );
    
    memset( name, 0, sizeof(name) );
    p = copytoken( p, name );

    if( !f ) {
      fvmc_printf( 2, "Undefining label %s\n", name );
      undefconst( name );
    }

    return 0;
  } else {
    /* unknown directive */
    usage( "Unknown directive \"%s\"", directive );
    return -1;
  }

  return -1;
}


static struct {
  char *inst;
  uint32_t opcode;
  uint32_t args;
#define ARG0_REG 0x00
#define ARG0_CONST 0x01
#define ARG1_REG 0x00
#define ARG1_CONST 0x02
} opcodes[] = {
	{ "NOP",  0x00, 0x00000000 },
	{ "LD",   0x01, 0x00020000 },   /* LD RX RY load from memory address ry into rx*/
	{ "LD",   0x02, 0x00020002 },   /* LD RX constant load from memory address const into rx */
	{ "ST",   0x03, 0x00020000 },   /* ST RX RY store ry into memory address rx */
	{ "ST",   0x04, 0x00020002 },   /* ST RX const store const into memory address rx */
	{ "LDI",  0x05, 0x00020002 },   /* LDI RX constant load const into rx*/
	{ "LEA",  0x06, 0x00020002 },   /* LEA RX constant */
	{ "PUSH", 0x07, 0x00010000 },   /* PUSH RX */
	{ "PUSH", 0x08, 0x00010001 },   /* PUSH const */
	{ "POP",  0x09, 0x00010000 },   /* POP RX */
	{ "RET",  0x0a, 0x00000000 },   /* RET */
	{ "CALL", 0x0b, 0x00010000 },   /* CALL RX */
	{ "CALL", 0x0c, 0x00010001 },   /* CALL const */
	{ "JMP",  0x0d, 0x00010000 },   /* JMP RX */
	{ "JMP",  0x0e, 0x00010001 },   /* JMP const */
	{ "JZ",   0x0f, 0x00010000 },   /* JZ RX */
	{ "JZ",   0x10, 0x00010001 },   /* JZ const */
	{ "JP",   0x11, 0x00010000 },   /* JP RX */
	{ "JP",   0x12, 0x00010001 },   /* JP const */
	{ "JN",   0x13, 0x00010000 },   /* JN RX */
	{ "JN",   0x14, 0x00010001 },   /* JN const */
	{ "JPZ",  0x15, 0x00010000 },   /* JPZ RX */
	{ "JPZ",  0x16, 0x00010001 },   /* JPZ const */
	{ "JPN",  0x17, 0x00010000 },   /* JPN RX */
	{ "JPN",  0x18, 0x00010001 },   /* JPN const */
	{ "JNZ",  0x19, 0x00010000 },   /* JNZ RX */
	{ "JNZ",  0x1a, 0x00010001 },   /* JNZ const */
	{ "ADD",  0x1b, 0x00020000 },   /* ADD RX RY */
	{ "ADD",  0x1c, 0x00020002 },   /* ADD RX const */
	{ "SUB",  0x1d, 0x00020000 },   /* SUB RX RY */
	{ "SUB",  0x1e, 0x00020002 },   /* SUB RX const */
	{ "MUL",  0x1f, 0x00020000 },   /* MUL RX RY */
	{ "MUL",  0x20, 0x00020002 },   /* MUL RX const */
	{ "DIV",  0x21, 0x00020000 },   /* DIV RX RY */
	{ "DIV",  0x22, 0x00020002 },   /* DIV RX const */
	{ "MOD",  0x23, 0x00020000 },   /* MOD RX RY */
	{ "MOD",  0x24, 0x00020002 },   /* MOD RX const */
	{ "AND",  0x25, 0x00020000 },   /* AND RX RY */
	{ "AND",  0x26, 0x00020002 },   /* AND RX const */
	{ "OR",   0x27, 0x00020000 },   /* OR RX RY */
	{ "OR",   0x28, 0x00020002 },   /* OR RX const */
	{ "XOR",  0x29, 0x00020000 },   /* XOR RX RY */
	{ "XOR",  0x2a, 0x00020002 },   /* XOR RX const */
	{ "NOT",  0x2b, 0x00010000 },   /* NOT RX */
	{ "SHL",  0x2c, 0x00020000 },   /* SHL RX RY */
	{ "SHL",  0x2d, 0x00020002 },   /* SHL RX const */
	{ "SHR",  0x2e, 0x00020000 },   /* SHR RX RY */
	{ "SHR",  0x2f, 0x00020002 },   /* SHR RX const */
	{ "ROL",  0x30, 0x00020000 },   /* ROL RX RY */
	{ "ROL",  0x31, 0x00020002 },   /* ROL RX const */
	{ "ROR",  0x32, 0x00020000 },   /* ROR RX RY */
	{ "ROR",  0x33, 0x00020002 },   /* ROR RX const */
	{ "RESV3", 0x34, 0x00000000 },   /* Reserved. Unused instruction */
	{ "RESV1", 0x35, 0x00000000 },   /* Reserved. Unused instruction */
	{ "RESV2", 0x36, 0x00000000 },   /* Reserved, UNused instruction */
	{ "LEASP", 0x37, 0x00020002 },   /* LEASP RX const. load address from stack pointer with offset */
	{ "ADDSP", 0x38, 0x00010000 },  /* ADDSP RX */
	{ "ADDSP", 0x39, 0x00010001 },  /* ADDSP const. adjust on stack. +ve allocate, -ve frees. */
	{ "MOV", 0x3a, 0x00020000 }, /* MOV RX RY. copy register */
	{ "CMP", 0x3b, 0x00020000 }, /* CMP RX RY. compare register */
	{ "CMP", 0x3c, 0x00020002 }, /* CMP RX const. compare with const */
	{ "LDINC", 0x3d, 0x00020000 }, /* LDINC RX RY. load from memory address ry into rx, then increment ry */
	{ "STINC", 0x3e, 0x00020000 }, /* STINC RX RY. store ry into memory address rx, then incremenet rx */
	{ "LDSP", 0x3f, 0x00020000 }, /* LDSP RX RY. load from stack pointer offset by ry into rx */
	{ "LDSP", 0x40, 0x00020002 }, /* LDSP RX const. load from stack pointer offset by const into rx */
	{ "SUBSP", 0x41, 0x00010000 },  /* SUB RX */
	{ "SUBSP", 0x42, 0x00010001 },  /* SUB const. adjust on stack. +ve frees, -ve allocates. */
	{ "LEASP", 0x43, 0x00020000 },   /* LEASP RX RY. load address from stack pointer with offset */
	{ "CALLNAT", 0x44, 0x00010000 }, /* CALLVNAT RX. call native function */
	{ "CALLNAT", 0x45, 0x00010001 }, /* CALLVNAT const. call native function */
	{ "HALT", 0x46, 0x00000000 }, /* HALT stop execution immediatly */
	{ "CALLZ", 0x47, 0x00010000 }, /* CALLZ RX call if zero flag set */
	{ "CALLZ", 0x48, 0x00010001 }, /* CALLZ const call if zero flag set */
	{ "STSP", 0x49, 0x00020000 }, /* STSP RX RY. store into stack pointer offset by ry into rx */
	{ "STSP", 0x4a, 0x00020002 }, /* STSP RX const. store into stack pointer offset by const into rx */
	{ "INC", 0x4b, 0x00020000 }, /* INC RX RY. increment at memory address RX by RY */	
	{ "INC", 0x4c, 0x00020002 }, /* INC RX const. increment at memory address RX by const */
	
	{ NULL, 0, 0 }
	  
};

static struct {
  char *reg;
  uint32_t regid;
} registers[] = {
       { "R0", 0 },
       { "R1", 1 },
       { "R2", 2 },
       { "R3", 3 },
       { "R4", 4 },
       { "R5", 5 },
       { "R6", 6 },
       { "R7", 7 },
       { "%R0", 0 },
       { "%R1", 1 },
       { "%R2", 2 },
       { "%R3", 3 },
       { "%R4", 4 },
       { "%R5", 5 },
       { "%R6", 6 },
       { "%R7", 7 },       
       { NULL, 0 }
};


static int encode_inst( char *str, uint32_t *opcode, uint32_t addr, int firstpass ) {
  char *p, *q, *startargs;
  char inst[64], arg[64];
  int i, nargs, j, k;
  
  p = str;
  p = skipwhitespace ( p );
  if( *p == ';' ) {
    //printf( ";; comment\n" );
    return -1;
  }
  if( *p == '\0' || *p == '\n' ) {
    //printf( ";; empty line 1 \"%s\"\n", str );
    return -1;
  }
  
  memset( inst, 0, sizeof(inst) );
  p = copytoken( p, inst );
  q = inst + strlen( inst ) - 1;
  if( *q == ':' ) {
    struct label *l;
    
    if( !firstpass ) return -1;
    
    /* last character is a : means this is a label identifier */
    *q = '\0';

    l = addlabel( inst, addr );
    if( l ) l->symflags = FVM_SYMBOL_PROC;
    return -1;
  }
        
  i = 0;
  startargs = p;
  while( opcodes[i].inst != NULL ) {
    if( strcasecmp( opcodes[i].inst, inst ) == 0 ) {
      p = startargs;
      *opcode = opcodes[i].opcode << 24;
      
      nargs = (opcodes[i].args & 0x000f0000) >> 16;
      for( k = 0; k < nargs; k++ ) {
	p = skipwhitespace( p );
	if( *p == '\0' || *p == '\n' ) {
	  //printf( ";; empty line 3\n" );
	  goto cont;
	}

	memset( arg, 0, sizeof(arg) );
	p = copytoken( p, arg );

	//printf( ";; arg %d = %d\n", k, (opcodes[i].args >> k) & 0x1 );
	switch( (opcodes[i].args >> k) & 0x1 ) {
	case 0:
	  /* register */
	  j = 0;
	  while( registers[j].reg != NULL ) {
	    if( strcasecmp( registers[j].reg, arg ) == 0 ) {
	      if( k == 0 ) *opcode |= registers[j].regid << 20;
	      else {
		//*opcode |= (0x0001000 << (k - 1)); // flag indicating 2nd arg is a register
		*opcode |= (registers[j].regid << ((k - 1) * 4));
	      }
	      break;
	    }
	    j++;
	  }
	  if( registers[j].reg == NULL ) {
	    fvmc_printf( 2, ";; bad register reg=\"%s\" k=%d\n" , arg, k );
	    goto cont;
	  }
	  
	  break;
	case 1:
	  /* constant */
	  j = strtoul( arg, &q, 0 );
	  if( *q ) {
	    /* bad constant, maybe a label? */
	    if( firstpass ) j = 0;
	    else {
	      struct label *l = getlabel( arg );
	      if( l ) fvmc_printf( 2, "Got label %s addr=%x\n", arg, l->addr );
	      else fvmc_printf( 2, "Failed to get label %s\n", arg );
	      
	      if( l ) {
		j = l->addr;
	      } else {
		if( arg[0] == '$' ) {
		  j = strtoul( arg + 2, &q, 0 );
		  if( *q ) j = getlabeladdr( arg + 2 );
		  
		  if( arg[1] == '-' ) j = (addr - j) % 0x10000;
		  else if( arg[1] == '+' ) j = (addr + j) + 0x10000;
		  else usage( "Bad operator %c", arg[1] );
		} else {
		  struct constdef *c = getconst( arg );
		  if( !c ) {
		    fvmc_printf( 2, "Bad constant or unknown label \"%s\"\n", arg );
		    goto cont;
		  } else {
		    j = c->val;
		  }
		}
	      }
	    }
	  }
	  fvmc_printf( 2, ";; constant = %x str=%s\n", j, arg );
	  *opcode |= j & 0xffff;
	  break;
	}
      }

      //      printf( ";; success\n" );
      return 0;
    }

  cont:
    i++;
  }

  usage( "Unknown opcode \"%s\"", inst ); 
  return -1;
}

static void emit_header( FILE *f, uint32_t addr ) {
  struct fvm_header header;
  int nsyms, idx;
  struct label *l;

  l = glob.labels;
  nsyms = 0;
  while( l ) {
    if( l->flags & LABEL_EXPORT ) nsyms++;
    l = l->next;
  }
  if( nsyms != glob.exportidx ) usage( "bad export number?" );
  
  if( !glob.progid ) {
    uint32_t val;
    if( glob.modulename[0] ) {
      val = sec_crc32( 0, glob.modulename, strlen( glob.modulename ) );
    } else {
      val = time( NULL );
    }
    
    glob.progid = 0x20000000 + (val % 0x20000000);
  }
  if( !glob.modulename[0] ) sprintf( glob.modulename, "%08x", glob.progid );
  
  memset( &header, 0, sizeof(header) );
  header.magic = FVM_MAGIC;
  header.version = FVM_VERSION;
  header.datasize = glob.datasize;
  header.textsize = addr;
  header.symcount = nsyms;
  strcpy( header.name, glob.modulename );
  header.progid = glob.progid;
  header.versid = glob.versid;
  fwrite( &header, 1, sizeof(header), f );

  for( idx = 0; idx < glob.exportidx; idx++ ) {
    l = glob.labels;
    while( l ) {
      if( (l->flags & LABEL_EXPORT) && (l->exportidx == idx) ) {
	struct fvm_symbol symbol;
	memset( &symbol, 0, sizeof(symbol) );
	strcpy( symbol.name, l->name );
	symbol.addr = l->addr;
	symbol.flags = l->symflags;
	fwrite( &symbol, 1, sizeof(symbol), f );
	break;
      }
      l = l->next;
    }
    if( !l ) usage( "failed to find exported symbol index %u", idx );
  }
  
}

/* ------------------------ Disassembly routine ------------------------------- */

static void disassemble( char *filename ) {
  FILE *f;
  struct fvm_header header;
  struct fvm_symbol *sym;
  uint8_t u[4];
  uint32_t opcode;
  int i, j, k, n;
  
  f = fopen( filename, "rb" );
  if( !f ) usage( "Failed to open file \"%s\"", filename );

  n = fread( &header, 1, sizeof(header), f );
  if( n != sizeof(header) ) usage( "Bad header" );
  if( header.magic != FVM_MAGIC ) usage( "Bad magic" );
  if( header.version != FVM_VERSION ) usage( "Bad version %d", header.version );
  printf( ";; -------------- HEADER ------------ \n" );
  printf( "NAME        \t%-32s\n", header.name );
  printf( "PROGID      \t%08x (%u) %u\n", header.progid, header.progid, header.versid );
  printf( "DATASIZE    \t%u\n", header.datasize );
  printf( "TEXTSIZE    \t%u\n", header.textsize );
  printf( ";; ------------- SYMBOLS ------------ \n" );
  sym = malloc( sizeof(*sym) * header.symcount );
  fread( sym, 1, sizeof(*sym) * header.symcount, f );

  printf( "%-4s\t%-16s\t%-4s\t%-4s\n", "PROCID", "NAME", "ADDR", "FLAGS" );
  for( i = 0; i < header.symcount; i++ ) {
    printf( "%-4u\t%-16s\t%04x\t%08x\n", i, sym[i].name, sym[i].addr, sym[i].flags );
  }

  printf( ";; ------------- DATA -------------- \n" );
  for( i = 0; i < header.datasize; i += 4 ) {
    n = fread( u, 1, sizeof(u), f );

    for( j = 0; j < header.symcount; j++ ) {
      if( sym[j].addr == (FVM_ADDR_DATA + i) ) {
	printf( "%04x\t%s:\n", FVM_ADDR_DATA + i, sym[j].name );
	break;
      }
    }    
    printf( "%04x\t\t%02x %02x %02x %02x\n", FVM_ADDR_DATA + i, u[0], u[1], u[2], u[3] );
  }

  printf( ";; ------------- TEXT -------------- \n" );
  for( i = 0; i < header.textsize; i += 4 ) {
    for( j = 0; j < header.symcount; j++ ) {
      if( sym[j].addr == (FVM_ADDR_TEXT + i) ) {
	printf( "%04x\t%s:\n", FVM_ADDR_TEXT + i, sym[j].name );
	break;
      }
    }

    n = fread( &opcode, 1, sizeof(opcode), f );
    if( n != 4 ) usage( "Failed to read opcode i=%u size=%u offset=%u", i, header.textsize, (int)ftell( f ) - sizeof(header) - header.symcount * sizeof(*sym) );
    
    opcode = ntohl( opcode );
    j = 0;
    while( opcodes[j].inst ) {
      if( (opcode >> 24) == opcodes[j].opcode ) {
	printf( "%04x\t\t%-8s\t", FVM_ADDR_TEXT + i, opcodes[j].inst );
	for( k = 0; k < (opcodes[j].args >> 16); k++ ) {
	  if( opcodes[j].args & (0x1 << k) ) printf( "%04x (%d)\t", opcode & 0xffff, (opcode & 0x8000) ? (opcode & 0xffff) | 0xffff0000 : opcode & 0xffff );
	  else printf( "R%u\t", k == 0 ? (opcode >> 20) & 0x7 : (opcode >> (4*(k-1))) & 0x7 );
	}
	printf( "\t;; 0x%08x\n", opcode );
	break;
      }
      j++;
    }
    if( !opcodes[j].inst ) {
      printf( "%04x\t\tUNKNOWN\t\t\t;; 0x%08x | %c%c%c%c\n", FVM_ADDR_TEXT + i, opcode,
	      (char)((opcode >> 24) & 0xff), (char)((opcode >> 16) & 0xff), (char)((opcode >> 8) & 0xff), (char)(opcode & 0xff) );
    }
  }
  
  fclose( f );
}

