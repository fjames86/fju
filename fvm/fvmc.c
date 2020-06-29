
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

#include "fvm-private.h"

#define ADDR_RESERVED 0x0000  /* 4k bytes of reserved address space */
#define FVM_ADDR_DATA     0x1000  /* 16k reserved for data segment */
#define FVM_ADDR_TEXT     0x5000  /* 28k reserved for text segment */
#define FVM_ADDR_STACK    0xc000  /* 16k reserved for stack */

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "fvmc OPTIONS file...\n"
	  "\n"
	  "\n OPTIONS:\n"
	  "   -o outfile\n"
	  "   -v verbmose mode\n"
	  "   -d Disassemble\n" 
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

static char modulename[64] = "default";
static uint32_t modprogid;
static uint32_t modversid;
static int verbosemode = 0;

static void fvmc_printf( char *fmt, ... ) {
  va_list args;

  if( !verbosemode ) return;
  
  va_start( args, fmt );
  vprintf( fmt, args );
  va_end( args );
}

static int encode_inst( char *str, uint32_t *opcode, uint32_t addr, int firstpass );
static int parse_directive( char *buf, uint32_t *addr, FILE *f, int datasegment );
static void emit_header( FILE *f, uint32_t addr );
static void disassemble( char *filename );

struct label {
  char name[64];
  uint32_t addr;
  uint32_t symflags;
  uint32_t flags;
#define LABEL_EXPORT 0x01  
};

static int nlabels;
static struct label labels[1024];
static int addlabel( char *name, uint32_t addr ) {
  int i ;
  for( i = 0; i < nlabels; i++ ) {
    if( strcasecmp( labels[i].name, name ) == 0 ) {
      fvmc_printf( ";; duplicate label %s\n", name );
      return -1;
    }
  }

  fvmc_printf( ";; adding label %s addr %x\n", name, addr );
  strcpy( labels[nlabels].name, name );
  labels[nlabels].addr = addr;
  nlabels++;
  return 0;
}
static uint32_t getlabel( char *name ) {
  int i;
  for( i = 0; i < nlabels; i++ ) {
    if( strcasecmp( labels[i].name, name ) == 0 ) return labels[i].addr;
  }
  return 0;
}
static int exportlabel( char *name, uint32_t symflags ) {
  int i;
  for( i = 0; i < nlabels; i++ ) {
    if( strcasecmp( labels[i].name, name ) == 0 ) {
      labels[i].flags |= LABEL_EXPORT;
      labels[i].symflags = symflags;
      return 0;
    }
  }
  return -1;
}

static int datasize;

struct staticlib {
  struct staticlib *next;

  struct fvm_header header;
  struct fvm_symbol *symbols;
  uint8_t *data;
  uint8_t *text;
};
static struct staticlib *staticlibs;

static struct staticlib *staticlib_by_name( char *name ) {
  struct staticlib *sl;
  sl = staticlibs;
  while( sl ) {
    if( strcasecmp( sl->header.name, name ) == 0 ) return sl;
    sl = sl->next;
  }
  return NULL;
}
    
static void staticlib_load( char *path ) {
  FILE *f;
  struct staticlib *sl;
  int n;
  
  f = fopen( path, "rb" );
  if( !f ) {
    fvmc_printf( "Failed to open static library %s\n", path );
    return;
  }

  sl = malloc( sizeof(*sl) );
  n = fread( &sl->header, 1, sizeof(sl->header), f );
  if( n != sizeof(&sl->header) ) {
    fvmc_printf( "Bad header" );
    goto done;
  }
  if( sl->header.magic != FVM_MAGIC ) {
    fvmc_printf( "Bad header" );
    goto done;    
  }
  if( sl->header.version != FVM_VERSION ) {
    fvmc_printf( "Bad header" );
    goto done;    
  }

  sl->symbols = malloc( sizeof(*sl->symbols) * sl->header.symcount );
  n = fread( sl->symbols, 1, sizeof(*sl->symbols) * sl->header.symcount, f );
  if( n != sizeof(*sl->symbols) * sl->header.symcount ) {
    fvmc_printf( "Bad symbol table" );
    goto done;
  }
  
  sl->data = malloc( sl->header.datasize );
  n = fread( sl->data, 1, sl->header.datasize, f );
  if( n != sl->header.datasize ) {
    fvmc_printf( "Bad data size" );
    goto done;
  }
  
  sl->text = malloc( sl->header.textsize );
  n = fread( sl->text, 1, sl->header.textsize, f );
  if( n != sl->header.textsize ) {
    fvmc_printf( "Bad text size" );
    goto done;
  }

  sl->next = staticlibs;
  staticlibs = sl;
  
 done:
  fclose( f );
}



int main( int argc, char **argv ) {
  FILE *f, *outfile;
  char outfilename[256];
  char buf[256];
  uint32_t opcode;
  int i, j, starti;
  char *p;
  uint32_t addr;
  int disass = 0;
  
  strcpy( outfilename, "" );
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-o" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      strncpy( outfilename, argv[i], 255 );
    } else if( strcmp( argv[i], "-v" ) == 0 ) {
      verbosemode = 1;
    } else if( strcmp( argv[i], "-d" ) == 0 ) {
      disass = 1;
    } else if( strcmp( argv[i], "-l" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      /* load library: -l path
       * Add library. .INCLUDE directive pastes symbol content. 
       */
      staticlib_load( argv[i] );
    } else if( strcmp( argv[i], "-h" ) == 0 ) {
      usage( NULL );
    } else if( strcmp( argv[i], "--help" ) == 0 ) {
      usage( NULL );
    } else break;
    i++;
  }

  addr = 0;

  fvmc_printf( "\n ------ first pass ------- \n" );

  /* first pass collects labels and computes offsets. does not emit opcodes */
  starti = i;
  while( i < argc ) {
    if( disass ) {
      disassemble( argv[i] );
      i++;
      continue;
    }
    
    f = fopen( argv[i], "r" );  
    if( !f ) usage( "Failed to open file \"%s\"", argv[i] );
    
    memset( buf, 0, sizeof(buf) );
    while( fgets( buf, sizeof(buf) - 1, f ) ) {
      p = buf;
      while( *p != '\0' ) {
	if( *p == '\n' ) *p = '\0';
	p++;
      }

      p = buf;
      while( *p == ' ' || *p == '\t' ) p++;
      if( *p == '\0' ) continue;

      if( parse_directive( buf, &addr, NULL, 0 ) == 0 ) continue;
      
      fvmc_printf( ";; attempting to encode line \"%s\"\n", buf );
      if( encode_inst( buf, &opcode, FVM_ADDR_TEXT + addr, 1 ) != -1 ) {
	addr += 4;
      }
    }
    
    fclose( f );
    i++;
  }

  if( disass ) exit( 0 );
  
  fvmc_printf( "\n ------ label table ------- \n" );
  for( j = 0; j < nlabels; j++ ) {
    fvmc_printf( ";; Label %-4d %-16s = 0x%08x\n", j, labels[j].name, labels[j].addr );
  }
  fvmc_printf( "\n ------ second pass ------- \n" );

  /* second pass emits output */

  if( !outfilename[0] ) {
    sprintf( outfilename, "%s.fvm", modulename );
  }
  outfile = fopen( outfilename, "w" );
  if( !outfile ) usage( "Failed to open outfile \"%s\"", outfilename );

  /* write header */
  emit_header( outfile, addr );

  i = starti;
  while( i < argc ) {
    f = fopen( argv[i], "r" );  
    if( !f ) usage( "Failed to open file \"%s\"", argv[i] );
    
    memset( buf, 0, sizeof(buf) );
    addr = 0;
    while( fgets( buf, sizeof(buf) - 1, f ) ) {
      p = buf;
      while( *p != '\0' ) {
	if( *p == '\n' ) *p = '\0';
	p++;
      }
      
      p = buf;
      while( *p == ' ' || *p == '\t' ) p++;
      if( *p == '\0' ) continue;
      
      if( parse_directive( buf, &addr, outfile, 1 ) == 0 ) continue;
    }
    
    fclose( f );
    i++;
  }
  
  i = starti;
  while( i < argc ) {
    f = fopen( argv[i], "r" );  
    if( !f ) usage( "Failed to open file \"%s\"", argv[i] );
    
    memset( buf, 0, sizeof(buf) );
    addr = 0;
    while( fgets( buf, sizeof(buf) - 1, f ) ) {
      p = buf;
      while( *p != '\0' ) {
	if( *p == '\n' ) *p = '\0';
	p++;
      }
      
      p = buf;
      while( *p == ' ' || *p == '\t' ) p++;
      if( *p == '\0' ) continue;
      
      if( parse_directive( buf, &addr, outfile, 0 ) == 0 ) continue;

      fvmc_printf( ";; attempting to encode line \"%s\"\n", buf );
      if( encode_inst( buf, &opcode, 0, 0 ) != -1 ) {
	fvmc_printf( ";; encoding \"%s\" = %08x\n", buf, opcode );
	opcode = htonl( opcode );
	fwrite( &opcode, 1, 4, outfile );
      }
    }
    
    fclose( f );
    i++;
  }
  
  fclose( outfile );
    
  return 0;
}

static int parse_directive( char *buf, uint32_t *addr, FILE *f, int datasegment ) {
  char *p;
  char directive[64], name[64];
  uint32_t size;
  char *q;
  
  p = buf;
  while( *p == ' ' || *p == '\t' ) p++;
  if( *p == '\0' ) return -1;
  if( *p != '.' ) return -1;
  
  /* assembler directive */
  memset( directive, 0, sizeof(directive) );
  q = directive;
  while( *p != ' ' && *p != '\t' && *p != '\0' ) {
    *q = *p;
    p++;
    q++;
  }
  
  if( (strcasecmp( directive, ".data" ) == 0) ||
      (strcasecmp( directive, ".text" ) == 0) ) {
    /* reserve some bytes in the data/text segment */
    while( *p == ' ' || *p == '\t' ) p++;    
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    q = name;
    while( *p != ' ' && *p != '\t' ) {
      *q = *p;
      p++;
      q++;
    }
    
    if( f == NULL ) addlabel( name, strcasecmp( directive, ".data" ) == 0 ? FVM_ADDR_DATA + datasize : FVM_ADDR_TEXT + *addr );

    while( 1 ) {
      /* data can be either a space separated list of numbers or a string */
      while( *p == ' ' || *p == '\t' ) p++;
      if( *p == '\0' ) return 0;

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
	  
	//fvmc_printf( "String \"%.*s\" length %u\n", size - 4, startp, size - 4 );

	if( f != NULL ) {	  
	  uint32_t s = size - 4;
	  if( (datasegment && (strcasecmp( directive, ".data" ) == 0)) || 
	      (!datasegment && (strcasecmp( directive, ".text" ) == 0)) ) {
	    char *qq;
	    
	    s = htonl( s );
	    //fvmc_printf( ";; writing string length %u\n", size - 4 );
	    fwrite( &s, 1, 4, f );

	    qq = startp;
	    while( *qq != '"' && *qq != '\0' ) {
	      if( *qq == '\\' ) {
		char c;
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
		//fvmc_printf( "writing char %c\n", c );
		fwrite( &c, 1, 1, f );
	      } else {
		//fvmc_printf( "writing char %c\n", *qq );
		fwrite( qq, 1, 1, f );
	      }
	      qq++;
	    }
	      
	    if( size % 4 ) {	      
	      uint8_t tmp[4];
	      //fvmc_printf( "adding string padding strlen=%u padding=%u\n", size, 4 - (size % 4) );
	      memset( tmp, 0, 4 );
	      fwrite( tmp, 1, 4 - (size % 4), f );
	      size += 4 - (size % 4);
	    }
	  }
	}

	if( size % 4 ) {
	  //fvmc_printf( "size %u not multiple of 4, adding padding\n", size );
	  size += 4 - (size % 4);
	}
	
      } else {
	/* integer or label always 4 bytes */
	char numstr[64];
        int isarray = 0;

	/* integer literal, label identifier or ARRAY */
	memset( numstr, 0, sizeof(numstr) );
	q = numstr;
	while( *p != ' ' && *p != '\t' && *p != '\0' ) {
	  *q = *p;
	  p++;
	  q++;
	}

	/* if value==ARRAY next arg is size of array */
	if( strcasecmp( numstr, "ARRAY" ) == 0 ) {
	  memset( numstr, 0, sizeof(numstr) );
	  p++;
	  while( *p == ' ' || *p == '\t' || *p == '\0' ) p++;
	  
	  q = numstr;
	  while( *p != ' ' && *p != '\t' && *p != '\0' ) {
	    *q = *p;
	    p++;
	    q++;
	  }
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
	    if( strcmp( numstr, "$") == 0 ) {
	      if( strcasecmp( directive, ".data" ) == 0 ) x = datasize;
	      else x = *addr;
	    } else {
	      x = getlabel( numstr );
	    }
	  }
	  
	  if( (datasegment && (strcasecmp( directive, ".data" ) == 0)) || 
	      (!datasegment && (strcasecmp( directive, ".text" ) == 0)) ) {
            if( isarray ) {
              char *b = malloc( size );
              memset( b, 0, size );
	      //fvmc_printf( "writing buffer size %u\n", size );
              fwrite( b, 1, size, f );
	      free( b );
             } else {
  	       x = htonl( x );
	       //fvmc_printf( "writing uint32 %x\n", x );
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
	  datasize += size;
	} else {
	  *addr = *addr + size;
	}
      }
    }
    return 0;
  } else if( strcasecmp( directive, ".CONST" ) == 0 ) {
    char str[64];

    while( *p == ' ' || *p == '\t' ) p++;    
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    q = name;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    }

    while( *p == ' ' || *p == '\t' ) p++;    
    if( *p == '\0' ) return 0;
    
    memset( str, 0, sizeof(str) );
    q = str;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    }
    if( f == NULL ) addlabel( name, strtoul( str, NULL, 0 ) );
    
    return 0;    
  } else if( strcasecmp( directive, ".EXPORT" ) == 0 ) {
    uint32_t symflags;
    char symtype[64];
    
    while( *p == ' ' || *p == '\t' ) p++;    
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    q = name;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    }

    symflags = 0;
    
    while( *p == ' ' || *p == '\t' ) p++;    
    memset( symtype, 0, sizeof(symtype) );
    q = symtype;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    } 
    if( strcasecmp( symtype, "PROC" ) == 0 ) symflags |= FVM_SYMBOL_PROC;
    else if( strcasecmp( symtype, "STRING" ) == 0 ) symflags |= FVM_SYMBOL_STRING;
    else if( strcasecmp( symtype, "UINT32" ) == 0 ) {
      symflags |= FVM_SYMBOL_UINT32;
      symflags |= 0x0004;
    } else if( strcasecmp( symtype, "UINT64" ) == 0 ) {
      symflags |= FVM_SYMBOL_UINT64;
      symflags |= 0x0008;
    }

    while( *p == ' ' || *p == '\t' ) p++;    
    memset( symtype, 0, sizeof(symtype) );
    q = symtype;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    }    
    if( symtype[0] ) {
      symflags |= (getlabel( symtype ) - getlabel( name )) & 0xffff;
    }
    
    if( exportlabel( name, symflags ) ) usage( "Unknown label \"%s\"", name );

    return 0;
  } else if( strcasecmp( directive, ".MODULE" ) == 0 ) {

    while( *p == ' ' || *p == '\t' ) p++;    
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    q = name;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    }

    fvmc_printf( "setting modulename to %s\n", name );
    strcpy( modulename, name );

    return 0;
  } else if( strcasecmp( directive, ".PROGRAM" ) == 0 ) {

    while( *p == ' ' || *p == '\t' ) p++;    
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    q = name;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    }

    modprogid = strtoul( name, NULL, 0 );

    while( *p == ' ' || *p == '\t' ) p++;    
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    q = name;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    }
    modversid = strtoul( name, NULL, 0 );
    
    return 0;
  } else if( strcasecmp( directive, ".LINK" ) == 0 ) {
    /* .LINK modulename symbolname */
    struct staticlib *sl;
    int j, found;
    uint32_t saddr, type, size;
    
    while( *p == ' ' || *p == '\t' ) p++;    
    if( *p == '\0' ) return 0;
    
    memset( name, 0, sizeof(name) );
    q = name;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    }

    sl = staticlib_by_name( name );
    if( !sl ) usage( "Unknown library module \"%s\"", name );

    while( *p == ' ' || *p == '\t' ) p++;    
    if( *p == '\0' ) usage( "Expected symbol name" );
    
    memset( name, 0, sizeof(name) );
    q = name;
    while( *p != ' ' && *p != '\t' && *p != '\0' ) {
      *q = *p;
      p++;
      q++;
    }

    found = 0;
    for( j = 0; j < sl->header.symcount; j++ ) {
      if( strcasecmp( sl->symbols[j].name, name ) == 0 ) {
	saddr = sl->symbols[j].addr;
	size = sl->symbols[j].flags & FVM_SYMBOL_SIZE_MASK;
	type = sl->symbols[j].flags & FVM_SYMBOL_TYPE_MASK;
	if( f ) {
	  if( datasegment && saddr >= FVM_ADDR_DATA && saddr < (FVM_ADDR_DATA + FVM_MAX_DATA) ) {
	    fwrite( sl->data + (saddr - FVM_ADDR_DATA), 1, size, f );
	  } else if( !datasegment && saddr >= FVM_ADDR_TEXT && saddr < (FVM_ADDR_TEXT + FVM_MAX_TEXT) ) {
	    fwrite( sl->text + (saddr - FVM_ADDR_TEXT), 1, size, f );
	  } else usage( "Bad symbol address" );
	} else {
	  if( saddr >= FVM_ADDR_DATA && saddr < (FVM_ADDR_DATA + FVM_MAX_DATA) ) {
	    datasize += size;
	  } else if( saddr >= FVM_ADDR_TEXT && saddr < (FVM_ADDR_TEXT + FVM_MAX_TEXT) ) {
	    *addr += size;
	  } else usage( "Bad symbol address" );	  
	}
	
	found = 1;
	break;
      }
    }
    if( !found ) usage( "Unresolved symbol %s", name );
    
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
	{ "CALLVIRT", 0x34, 0x00030000 }, /* CALLVIRT RX RY RZ. call a function in a remote module. rx=module name, ry=function. rz=argsize */
	{ "LDVIRT", 0x35, 0x00030000 },   /* LDVIRT RX RY RZ. load from remote module. rx=module name, ry=symbol rz=receives value */
	{ "STVIRT", 0x36, 0x00030000 },   /* STVIRT RX RY RZ. store into remote module. rx=module, ry=symbol rz=value */
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
	{ "CALLNAT", 0x44, 0x00020000 }, /* CALLVNAT RX RY. call native function, const is the proc identifier. RX receives result status */
	{ "CALLNAT", 0x45, 0x00020002 }, /* CALLVNAT RX const. call native function, const is the proc identifier. RX receives result status */
	
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
       { NULL, 0 }
};


static int encode_inst( char *str, uint32_t *opcode, uint32_t addr, int firstpass ) {
  char *p, *q, *startargs;
  char inst[64], arg[64];
  int i, nargs, j, k;
  
  p = str;
  while( *p == ' ' || *p == '\t' ) p++;
  if( *p == ';' ) {
    //printf( ";; comment\n" );
    return -1;
  }
  if( *p == '\0' || *p == '\n' ) {
    //printf( ";; empty line 1 \"%s\"\n", str );
    return -1;
  }
  
  memset( inst, 0, sizeof(inst) );
  q = inst;
  while( *p != ' ' && *p != '\t' && *p != '\n' ) {
    if( *p == '\0' || *p == '\n' ) {
      break;
    }
    *q = *p;
    p++;
    q++;
  }
  if( *(q - 1) == ':' ) {
    if( !firstpass ) return -1;
    
    /* last character is a : means this is a label identifier */
    *(q - 1)= '\0';

    addlabel( inst, addr );
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
	while( *p == ' ' || *p == '\t' ) p++;
	if( *p == '\0' || *p == '\n' ) {
	  //printf( ";; empty line 3\n" );
	  goto cont;
	}

	memset( arg, 0, sizeof(arg) );
	q = arg;
	while( *p != ' ' && *p != '\t' && *p != '\n' ) {
	  if( *p == '\0' || *p == '\n' ) {
	    //printf( ";; end of line\n" );
	    break;
	  }
	  *q = *p;
	  p++;
	  q++;
	}	  

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
	    fvmc_printf( ";; bad register reg=\"%s\" k=%d\n" , arg, k );
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
	      int b = 0, nl;
	      j = 0;
	      for( nl = 0; nl < nlabels; nl++ ) {
		if( strcasecmp( labels[nl].name, arg ) == 0 ) {
		  j = labels[nl].addr;
		  b = 1;
		  break;
		}
	      }
	      if( !b ) {
		fvmc_printf( "Bad constant or unknown label \"%s\"", arg );
		goto cont;
	      }
	    }
	  }
	  fvmc_printf( ";; constant = %x str=%s\n", j, arg );
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
  int nsyms, i;

  nsyms = 0;
  for( i = 0; i < nlabels; i++ ) {
    if( labels[i].flags & LABEL_EXPORT ) nsyms++;
  }

  if( !modprogid ) modprogid = 0x20000000 + (time( NULL ) % 0x20000000);
  if( !modulename[0] ) sprintf( modulename, "%08x", modprogid );
  
  memset( &header, 0, sizeof(header) );
  header.magic = FVM_MAGIC;
  header.version = FVM_VERSION;
  header.datasize = datasize;
  header.textsize = addr;
  header.symcount = nsyms;
  strcpy( header.name, modulename );
  header.progid = modprogid;
  header.versid = modversid;
  fwrite( &header, 1, sizeof(header), f );

  for( i = 0; i < nlabels; i++ ) {
    if( labels[i].flags & LABEL_EXPORT ) {
      struct fvm_symbol symbol;
      memset( &symbol, 0, sizeof(symbol) );
      strcpy( symbol.name, labels[i].name );
      symbol.addr = labels[i].addr;
      symbol.flags = labels[i].symflags;
      fwrite( &symbol, 1, sizeof(symbol), f );
    }
  }
  
}

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
    opcode = ntohl( opcode );
    j = 0;
    while( opcodes[j].inst ) {
      if( (opcode >> 24) == opcodes[j].opcode ) {
	printf( "%04x\t\t%-8s\t", FVM_ADDR_TEXT + i, opcodes[j].inst );
	for( k = 0; k < (opcodes[j].args >> 16); k++ ) {
	  if( opcodes[j].args & (0x1 << k) ) printf( "%04x\t", opcode & 0xffff );
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
