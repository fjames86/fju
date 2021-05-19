 
#define _CRT_SECURE_NO_WARNINGS 

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <fju/mmf.h>

struct _file {
  uint32_t magic;
  uint32_t n;
  char str[64];
};

int main( int argc, char **argv ) {
  struct mmf_s mmf;
  int sts;
  struct _file *f;

  sts = mmf_open( "test.dat", &mmf );
  mmf_remap( &mmf, 1024 );
  f = (struct _file *)mmf.file;

  mmf_lock( &mmf );
  f->magic = 0x12344321;
  printf( "n=%d str=\"%s\"\n", f->n, f->str );
  f->n++;
  sprintf( f->str, "hello %d!", f->n );
  mmf_unlock( &mmf );

  mmf_close( &mmf );

  return 0;
}




