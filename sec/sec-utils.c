
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#else
#include <unistd.h>  
#endif


#include <fju/sec.h>
#include <fju/hostreg.h>
#include <fju/lic.h>
#include <fju/freg.h>

#include <time.h>
#include <string.h>
#include <stdio.h>

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

int fju_check_license( char *licbuf, int size, struct lic_s *licp ) {
  static uint8_t pubkeybuf[] =
    {
     0xd6, 0xfd, 0x57, 0xa7, 0xbd, 0xff, 0xca, 0xc2, 0xe1, 0xc0, 0x63, 0xcb, 0xfb,
     0x72, 0x3f, 0x21, 0x02, 0x7e, 0x1f, 0x93, 0xf3, 0xb3, 0x65, 0xc2, 0x23, 0x69,
     0x50, 0x60, 0x5f, 0x6a, 0x52, 0xc7, 0x75, 0x7e, 0xe4, 0xb2, 0xdc, 0xfe, 0x9e,
     0x77, 0xbe, 0x19, 0x87, 0xbc, 0x4f, 0x22, 0x75, 0xeb, 0x7c, 0x6e, 0x63, 0xa3,
     0x19, 0x74, 0xe8, 0xb6, 0xa0, 0x28, 0xff, 0x54, 0x2a, 0xff, 0xa2, 0x51
    };
  static int initialized = 0;
  
  struct lic_s lic;
  int sts;
  struct sec_buf secret, pubkey, common, iov[1], sig;
  char commonbuf[SEC_ECDH_MAX_COMMON];
  struct hostreg_prop prop;

  if( !initialized ) {
    char licpubbuf[sizeof(pubkeybuf)];
    /* this provides a way of changing the license public key at runtime */
    sts = freg_get_by_name( NULL, 0, "/fju/licpubkey", FREG_TYPE_OPAQUE, licpubbuf, sizeof(licpubbuf), &size );
    if( !sts && size == sizeof(pubkeybuf) ) {
      memcpy( pubkeybuf, licpubbuf, sizeof(pubkeybuf) );
    }
  }
  
  if( licbuf == NULL && size == 0 ) {
    sts = freg_get_by_name( NULL, 0, "/fju/lic", FREG_TYPE_OPAQUE, (char *)&lic, sizeof(lic), &size );
    if( sts ) return -1;
  } else {
    if( size != sizeof(lic) ) return -1;
    memcpy( &lic, licbuf, sizeof(lic) );
  }
  
  hostreg_prop( &prop );
  secret.buf = (char *)prop.privkey;
  secret.len = sizeof(prop.privkey);
  pubkey.buf = (char *)pubkeybuf;
  pubkey.len = sizeof(pubkeybuf);    
  common.buf = commonbuf;
  common.len = sizeof(commonbuf);
  ecdh_common( &secret, &pubkey, &common );
  aes_decrypt( (uint8_t *)common.buf, (uint8_t *)&lic, sizeof(lic) );

  sts = -1;
  if( lic.hostid != hostreg_localid() ) {
    sts = -1;
  } else if( time( NULL ) > lic.expire ) {
    sts = -1;
  } else {
    iov[0].buf = (char *)&lic;
    iov[0].len = 32;
    sig.buf = lic.verf;
    sig.len = lic.nverf;
    sts = sec_verify( &pubkey, iov, 1, &sig );
  }

  if( !sts && licp ) *licp = lic;
  
  return sts;
}

/* ------------------------ base32 encoding ---------------------- */

#if 0

static void pad( unsigned char *buf, int len ) {
  int i;
  for( i = 0; i < len; i++ ) {
    buf[i] = '=';
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
  if( offset > 0 ) return byte >> offset;
  else return byte << -offset;
}

static unsigned char shift_left( unsigned char byte, char offset ) {
  return shift_right( byte, -offset );
}

static void encode_sequence( const unsigned char *plain, int len, unsigned char *coded ) {
  int block, octet, junk;
  unsigned char c;
  
  for( block = 0; block < 8; block++ ) {
    octet = get_octet( block ); 
    junk = get_offset( block ); 
    
    if( octet >= len ) { 
      pad( &coded[block], 8 - block );
      return;
    }
    
    c = shift_right( plain[octet], junk ); 
    
    if (junk < 0 && octet < len - 1 ) {
      c |= shift_right( plain[octet+1], 8 + junk );
    }
    coded[block] = encode_char( c );
  }
}

/* convert 5 bits of plain into a 8bit char so total length of coded must be (len*8) / 5 */
void base32_encode( char *plain, int len, char *coded ) {
  int i, j;
  
  for( i = 0, j = 0; i < len; i += 5, j += 8 ) {
    encode_sequence( (uint8_t *)&plain[i], (len - i) < 5 ? len - i : 5, (uint8_t *)&coded[j] );
  }

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
    if( n < 5 )
      return written;    
  }

  return -1;
}

#endif

/* --------------- base64 ------------------ */


static char b64chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static char *base64_encode_block( uint8_t *buf, int len, char *str ) {
  str[0] = b64chars[buf[0] >> 2];
  str[1] = b64chars[((buf[0] & 0x3) << 4) | ((buf[1] & 0xf0) >> 4)];
  str[2] = len > 1 ? b64chars[((buf[1] & 0x0f) << 2) | ((buf[2] & 0xc0) >> 6)] : '=';
  str[3] = len > 2 ? b64chars[buf[2] & 0x3f] : '=';
  return str + 4;
}

/* 
 * str must be a least 4*((blen / 3) + (blen % 3 ? 1 : 0)) + 1 bytes long
 */
int base64_encode( char *buf, int buflen, char *str ) {
  int i;
  
  for( i = 0; i < buflen / 3; i++ ) {
    str = base64_encode_block( (uint8_t *)&buf[3*i], 3, str );
  }
  if( buflen % 3 ) {
    str = base64_encode_block( (uint8_t *)&buf[3*(buflen / 3)], buflen % 3, str );
  }
  *str = '\0';
  
  return 0;
}


static void base64_decode_block( uint8_t *str, uint8_t *buf ) {
  /* assume the chars of str have been checked already that the high 2 bits are clear? */
  buf[0] = str[0] << 2 | str[1] >> 4;
  buf[1] = str[1] << 4 | str[2] >> 2;
  buf[2] = str[2] << 6 | str[3] >> 0;
}

static int base64_valid_char( char c ) {
  if( c >= 'A' && c <= 'Z' ) return c - 'A';
  if( c >= 'a' && c <= 'z' ) return 26 + (c - 'a');
  if( c >= '0' && c <= '9' ) return 52 + (c - '0');
  if( c == '+' ) return 62;
  if( c == '/' ) return 63;
  return -1;
}
    
/*
 * Return number of bytes decoded 
 */
int base64_decode( char *buf, int buflen, char *str ) {
  uint8_t tmp[4];
  int i, j, c, bidx, endp;

  bidx = 0;
  while( 1 ) {
    endp = -1;
    for( i = 0; i < 4; i++ ) {
      if( *str == '=' ) {
	if( i == 0 ) return -1; /* = not allowed in this position */
	
	endp = i;
	/* check the rest are also '=' chars */
	for( j = i; j < 4; j++ ) {
	  if( *str != '=' ) return -1; /* bad b64 string */
	  tmp[j] = 0;
	  str++;
	}
	break;
      } else {
	c = base64_valid_char( *str );
	if( c < 0 ) {
	  if( i == 0 ) goto done;
	  else return -1;
	}
	
	tmp[i] = c;
      }
      str++;
    }

    if( endp == -1 ) {
      base64_decode_block( tmp, (uint8_t *)buf );
      buf += 3;
      buflen -= 3;
      bidx += 3;
    } else {
      uint8_t tbuf[3];
      base64_decode_block( tmp, tbuf );

      c = endp == 1 ? 1 : endp == 2 ? 1 : endp == 3 ? 2 : 3;
      if( buflen >= c ) {
	memcpy( buf, tbuf, c );
	buf += c;
	buflen -= c;
      }
      bidx += c;
    }
    
    if( endp != -1 ) break;
  }
 done:
  
  return bidx;
}


