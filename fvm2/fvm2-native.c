
#include "fvm2-private.h"

#include <fju/rpc.h>
#include <arpa/inet.h>

typedef int (*fvm2_native_cb)( struct fvm2_s *state );

struct fvm2_native_proc {
  uint32_t procid;
  fvm2_native_cb cb;
};

static int native_nop( struct fvm2_s *state ) {
  return 0;
}

static int native_puts( struct fvm2_s *state ) {
  /* pop stack to get address of string */
  uint32_t sp, len;
  char *str;
  
  sp = ntohl( fvm2_pop( state ) );
  len = ntohl( fvm2_read( state, sp ) );
  str = fvm2_getaddr( state, sp + 4 );
  if( !str ) return -1;

  printf( "%.*s\n", len, str );
  return 0;
}

static struct fvm2_native_proc native_procs[] =
  {
   { 0, native_nop },
   { 1, native_puts },
   { 0, NULL }
  };

int fvm2_native_call( struct fvm2_s *state, uint32_t procid ) {
  int i;
  i = 0;
  while( native_procs[i].cb ) {
    if( native_procs[i].procid == procid ) {
      return native_procs[i].cb( state );
    }
    i++;
  }
  
  return -1;
}

