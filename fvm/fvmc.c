
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

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

struct label {
  char name[64];
  uint32_t addr;
};

static int nlabels;
static struct label labels[1024];

int main( int argc, char **argv ) {
  FILE *f, *outfile;
  char buf[256];
  uint32_t opcode;
  char *filename, *outfilename;
  int i, j;
  char *p;
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
  
  while( i < argc ) {
    f = fopen( argv[i], "r" );  
    if( !f ) usage( "Failed to open file \"%s\"", argv[i] );

    printf( "\n ------ first phase ------- \n" );
    memset( buf, 0, sizeof(buf) );
    while( fgets( buf, sizeof(buf) - 1, f ) ) {
      p = buf;
      while( *p != '\0' ) {
	if( *p == '\n' ) *p = '\0';
	p++;
      }
      //      printf( ";; attempting to encode line \"%s\"\n", buf );
      if( encode_inst( buf, &opcode, addr, 1 ) != -1 ) {
	addr += 4;
      }
    }
    fseek( f, 0, SEEK_SET );

    printf( "\n ------ label table ------- \n" );
    for( j = 0; j < nlabels; j++ ) {
      printf( ";; Label %-4d %-16s = 0x%08x\n", j, labels[j].name, labels[j].addr );
    }
    printf( "\n ------ second phase ------- \n" );
    
    memset( buf, 0, sizeof(buf) );
    addr = 0;
    while( fgets( buf, sizeof(buf) - 1, f ) ) {
      p = buf;
      while( *p != '\0' ) {
	if( *p == '\n' ) *p = '\0';
	p++;
      }
      //      printf( ";; attempting to encode line \"%s\"\n", buf );
      if( encode_inst( buf, &opcode, 0, 0 ) != -1 ) {
	printf( ";; encoding \"%s\" = %08x\n", buf, opcode );
	fwrite( &opcode, 4, 1, outfile );
      }
    }
    
    fclose( f );
    i++;
  }
  
  fclose( outfile );
    
  return 0;
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
	{ "LDI",  0x03, 0x00020002 },   /* LDI RX constant */ 
	{ "STI",  0x04, 0x00020002 },   /* STI RX constant */
	{ "LEA",  0x05, 0x00020002 },   /* LEA RX constant */
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
      printf( ";; empty line 2\n" );
      return -1;
    }
    *q = *p;
    p++;
    q++;
  }
  if( *(q - 1) == ':' ) {
    if( !firstpass ) return -1;
    
    /* last character is a : means this is a label identifier */
    *(q - 1)= '\0';

    for( i = 0; i < nlabels; i++ ) {
      if( strcmp( labels[i].name, inst ) == 0 ) {
	printf( ";; duplicate label %s\n", inst );
	return -1;
      }
    }
    
    strcpy( labels[nlabels].name, inst );
    labels[nlabels].addr = addr;
    nlabels++;
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
	  //printf( ";; constant = %x str=%s\n", j, arg );
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
