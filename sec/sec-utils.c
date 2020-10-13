
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#else
#include <unistd.h>  
#endif


#include <fju/sec.h>

int fju_readstdin( char *buf, int size ) {
  int offset = 0;
  int sts;

  while( offset < size ) {
#ifdef WIN32
    {
      int b;
      b = ReadFile( GetStdHandle( STD_INPUT_HANDLE ), buf + offset, size - offset, (DWORD *)&sts, NULL );
      if( !b ) sts = -1;
    }
#else
    sts = read( STDIN_FILENO, buf + offset, size - offset );
#endif
    
    if( sts <= 0 ) break;
    offset += sts;
  } 

  return offset;
}

int fju_writestdout( char *buf, int size ) {
#ifdef WIN32
  WriteFile( GetStdHandle( STD_OUTPUT_HANDLE ), buf, size, NULL, NULL );
#else
  write( STDOUT_FILENO, buf, size );
#endif
  return 0;
}



static size_t min(size_t x, size_t y) {
  return x < y ? x : y;
}

#define PADDING_CHAR '='

static void pad( unsigned char *buf, int len ) {
  int i;
  for( i = 0; i < len; i++ ) {
    buf[i] = PADDING_CHAR;
  }
}

static unsigned char encode_char( unsigned char c ) {
  static unsigned char base32[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
  return base32[c & 0x1F];
}

static int decode_char( unsigned char c ) {
  char retval = -1;
  
  if (c >= 'A' && c <= 'Z')
    retval = c - 'A';
  if (c >= '2' && c <= '7')
    retval = c - '2' + 26;
  
  return  retval;
}

static int get_octet( int block ) {
  return (block * 5) / 8;
}

static int get_offset( int block ) {
  return (8 - 5 - (5*block) % 8);
}

static unsigned char shift_right( unsigned char byte, char offset ) {
  if (offset > 0)
    return byte >>  offset;
  else
    return byte << -offset;
}

static unsigned char shift_left( unsigned char byte, char offset ) {
  return shift_right(byte, - offset);
}

static void encode_sequence( const unsigned char *plain, int len, unsigned char *coded ) {
  int block, octet, junk;
  unsigned char c;
  
  for( block = 0; block < 8; block++ ) {
    octet = get_octet(block); 
    junk = get_offset(block); 
    
    if( octet >= len ) { 
      pad( &coded[block], 8 - block );
      return;
    }
    
    c = shift_right( plain[octet], junk ); 
    
    if (junk < 0 && octet < len - 1 ) {
      c |= shift_right( plain[octet+1], 8 + junk );
    }
    coded[block] = encode_char(c);
  }
}

/* convert 5 bits of plain into a 8bit char so total length of coded must be (len*8) / 5 */
void base32_encode( char *plain, int len, char *coded ) {
  int i, j;
  
  for( i = 0, j = 0; i < len; i += 5, j += 8 ) {
    encode_sequence( (uint8_t *)&plain[i], min(len - i, 5), (uint8_t *)&coded[j] );
  }
  //coded[j] = '\0';
}

static int decode_sequence( unsigned char *coded, unsigned char *plain ) {
  int block, offset, octet, c;
  
  plain[0] = 0;
  for( block = 0; block < 8; block++ ) {
    offset = get_offset( block );
    octet = get_octet( block );

    c = decode_char( coded[block] );
    if (c < 0)  // invalid char, stop here
      return octet;
    
    plain[octet] |= shift_left( c, offset );
    if( offset < 0 ) {
      plain[octet + 1] = shift_left( c, 8 + offset );
    }
  }
  
  return 5;
}

/* covert each 8bit coded char into 5 bits of plain text. so plaintext size must be (strlen(coded) * 5) / 8 */
int base32_decode( char *coded, char *plain ) {
  int written = 0, i, j, n;

  for( i = 0, j = 0; ; i += 8, j += 5 ) {
    n = decode_sequence( (uint8_t *)&coded[i], (uint8_t *)&plain[j] );
    written += n;
    if (n < 5)
      return written;
    
  }

  return -1;
}

