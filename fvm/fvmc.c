
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#define ADDR_RESERVED 0x0000  /* 4k bytes of reserved address space */
#define ADDR_DATA     0x1000  /* 16k reserved for data segment */
#define ADDR_TEXT     0x5000  /* 28k reserved for text segment */
#define ADDR_STACK    0xc000  /* 16k reserved for stack */

static void usage( char *fmt, ... ) {
  va_list args;
  
  printf( "fvmc OPTIONS file...\n"
	  "\n"
	  "\n OPTIONS:\n"
	  "   -o outfile\n" 
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

static int encode_inst( char *str, uint32_t *opcode, uint32_t addr, int firstpass );
static int parse_directive( char *buf, uint32_t *addr, FILE *f, int datasegment );
static void emit_header( FILE *f );

static uint32_t u32be( uint32_t u ) {
  uint8_t *u8 = (uint8_t *)&u;
  return ((uint32_t)u8[3] << 24) | ((uint32_t)u8[2] << 16) | ((uint32_t)u8[1] << 8) | (uint32_t)u8[0];
}

struct label {
  char name[64];
  uint32_t addr;
};

static int nlabels;
static struct label labels[1024];
static int addlabel( char *name, uint32_t addr ) {
  int i ;
  for( i = 0; i < nlabels; i++ ) {
    if( strcasecmp( labels[i].name, name ) == 0 ) {
      printf( ";; duplicate label %s\n", name );
      return -1;
    }
  }
    
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

static int datasize;

int main( int argc, char **argv ) {
  FILE *f, *outfile;
  char buf[256];
  uint32_t opcode;
  char *outfilename;
  int i, j, starti;
  char *p, *q;
  uint32_t addr;
  
  outfilename = "out.fvm";
  
  i = 1;
  while( i < argc ) {
    if( strcmp( argv[i], "-o" ) == 0 ) {
      i++;
      if( i >= argc ) usage( NULL );
      outfilename = argv[i];
    } else if( strcmp( argv[i], "-h" ) == 0 ) {
      usage( NULL );
    } else break;
    i++;
  }
  
  outfile = fopen( outfilename, "w" );
  if( !outfile ) usage( "Failed to open outfile" );
  addr = 0;

  printf( "\n ------ first pass ------- \n" );

  /* first pass collects labels and computes offets. does not emit opcodes */
  starti = i;
  while( i < argc ) {
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
      
      printf( ";; attempting to encode line \"%s\"\n", buf );
      if( encode_inst( buf, &opcode, ADDR_TEXT + addr, 1 ) != -1 ) {
	addr += 4;
      }
    }
    
    fclose( f );
    i++;
  }

  
  printf( "\n ------ label table ------- \n" );
  for( j = 0; j < nlabels; j++ ) {
    printf( ";; Label %-4d %-16s = 0x%08x\n", j, labels[j].name, labels[j].addr );
  }
  printf( "\n ------ second pass ------- \n" );

  /* second pass emits output */
  
  /* write header */
  emit_header( outfile );

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

      //      printf( ";; attempting to encode line \"%s\"\n", buf );
      if( encode_inst( buf, &opcode, 0, 0 ) != -1 ) {
	printf( ";; encoding \"%s\" = %08x\n", buf, opcode );
	opcode = u32be( opcode );
	fwrite( &opcode, 4, 1, outfile );
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
    
    if( f == NULL ) addlabel( name, strcasecmp( directive, ".data" ) == 0 ? ADDR_DATA + datasize : ADDR_TEXT + *addr );

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
	  if( *p == '\0' ) break;
	  if( *p == '\\' ) {
	    p++;
	    size++;
	    if( *p == '"' ) {
	      p++;
	      size++;
	    }	    
	  }
	  
	  p++;
	  size++;
	}
	p--;
	
	if( f != NULL ) {	  
	  uint32_t s = size - 4;
	  if( (datasegment && (strcasecmp( directive, ".data" ) == 0)) || 
	      (!datasegment && (strcasecmp( directive, ".text" ) == 0)) ) {
	    s = u32be( s );
	    fwrite( &s, 4, 1, f );
	    fwrite( startp, 1, s, f );
	  }
	}
      } else {
	/* integer or label always 4 bytes */
	char numstr[64];
        int isarray = 0;

	memset( numstr, 0, sizeof(numstr) );
	q = numstr;
	while( *p != ' ' && *p != '\t' && *p != '\0' ) {
	  *q = *p;
	  p++;
	  q++;
	}

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
	    x = getlabel( numstr );
	  }

	  if( (datasegment && (strcasecmp( directive, ".data" ) == 0)) || 
	      (!datasegment && (strcasecmp( directive, ".text" ) == 0)) ) {
            if( isarray ) {
              char *b = malloc( size );
              memset( b, 0, size );
              fwrite( b, 1, size, f );
              free( b );
             } else {
  	       x = u32be( x );
	       fwrite( &x, 4, 1, f );
             }
	  }
	  
	}
      }

      if( f == NULL ) {
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
    while( *p != ' ' && *p != '\t' ) {
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

  } else {
    /* unknown directive */
    printf( ";; unknown directive %s\n", directive );
    return -1;
  }
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
	{ "LD",   0x01, 0x00020000 },   /* LD RX RY */
	{ "LD",   0x01, 0x00020002 },   /* LD RX constant */
	{ "ST",   0x02, 0x00020000 },   /* ST RX RY */
	{ "ST",   0x02, 0x00020002 },   /* ST RX const */
	{ "LDI",  0x03, 0x00020002 },   /* LDI RX constant */
	{ "RESERVED",  0x04, 0x00000000 },   /* unused */
	{ "LEA",  0x05, 0x00020002 },   /* LEA RX constant */
	{ "PUSH", 0x06, 0x00010000 },   /* PUSH RX */
	{ "PUSH", 0x06, 0x00010001 },   /* PUSH const */
	{ "POP",  0x07, 0x00010000 },   /* POP RX */
	{ "RET",  0x08, 0x00000000 },   /* RET */
	{ "CALL", 0x09, 0x00010000 },   /* CALL RX */
	{ "CALL", 0x09, 0x00010001 },   /* CALL const */
	{ "JMP",  0x0a, 0x00010000 },   /* JMP RX */
	{ "JMP",  0x0a, 0x00010001 },   /* JMP const */
	{ "JZ",   0x0b, 0x00010000 },   /* JZ RX */
	{ "JZ",   0x0b, 0x00010001 },   /* JZ const */
	{ "JP",   0x0c, 0x00010000 },   /* JP RX */
	{ "JP",   0x0c, 0x00010001 },   /* JP const */
	{ "JN",   0x0d, 0x00010000 },   /* JN RX */
	{ "JN",   0x0d, 0x00010001 },   /* JN const */
	{ "JPZ",  0x0e, 0x00010000 },   /* JPZ RX */
	{ "JPZ",  0x0e, 0x00010001 },   /* JPZ const */
	{ "JPN",  0x0f, 0x00010000 },   /* JPN RX */
	{ "JPN",  0x0f, 0x00010001 },   /* JPN const */
	{ "JNZ",  0x10, 0x00010000 },   /* JNZ RX */
	{ "JNZ",  0x10, 0x00010001 },   /* JNZ const */
	{ "ADD",  0x11, 0x00020000 },   /* ADD RX RY */
	{ "ADD",  0x11, 0x00020002 },   /* ADD RX const */
	{ "SUB",  0x12, 0x00020000 },   /* SUB RX RY */
	{ "SUB",  0x12, 0x00020002 },   /* SUB RX const */
	{ "MUL",  0x13, 0x00020000 },   /* MUL RX RY */
	{ "MUL",  0x13, 0x00020002 },   /* MUL RX const */
	{ "DIV",  0x14, 0x00020000 },   /* DIV RX RY */
	{ "DIV",  0x14, 0x00020002 },   /* DIV RX const */
	{ "MOD",  0x15, 0x00020000 },   /* MOD RX RY */
	{ "MOD",  0x15, 0x00020002 },   /* MOD RX const */
	{ "AND",  0x16, 0x00020000 },   /* AND RX RY */
	{ "AND",  0x16, 0x00020002 },   /* AND RX const */
	{ "OR",   0x17, 0x00020000 },   /* OR RX RY */
	{ "OR",   0x17, 0x00020002 },   /* OR RX const */
	{ "XOR",  0x18, 0x00020000 },   /* XOR RX RY */
	{ "XOR",  0x18, 0x00020002 },   /* XOR RX const */
	{ "NOT",  0x19, 0x00010000 },   /* NOT RX */


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
       { "SP", 8 },
       { "PC", 9 },
       { "FLAGS", 10 },
       { NULL, 0 }
};


static int encode_inst( char *str, uint32_t *opcode, uint32_t addr, int firstpass ) {
  char *p, *q, *startargs;
  char inst[64], arg[64];;
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
	  printf( ";; empty line 3\n" );
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
		*opcode |= 0x00010000; // flag indicating 2nd arg is a register
		*opcode |= registers[j].regid;
	      }
	      break;
	    }
	    j++;
	  }
	  if( registers[j].reg == NULL ) {
	    //printf( ";; bad register reg=\"%s\" k=%d\n" , arg, k );
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
		printf( ";; bad constant or unknown label %s\n", arg );
		return -1;
	      }
	    }
	  }
	  printf( ";; constant = %x str=%s\n", j, arg );
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

  printf( ";; unknown opcode \"%s\"\n", inst ); 
  return -1;
}

struct header {
  uint32_t magic;
  uint32_t version;
  uint32_t datasize;
  uint32_t spare[13];
};
static void emit_header( FILE *f ) {
  struct header header;
  memset( &header, 0, sizeof(header) );
  header.magic = 0x12341234;
  header.version = 1;
  header.datasize = datasize;
  
  fwrite( &header, sizeof(header), 1, f );
}
