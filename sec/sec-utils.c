
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
