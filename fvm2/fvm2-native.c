
#include "fvm2-private.h"

#include <fju/rpc.h>

typedef int (*fvm2_native_cb)( char *args, int argsize, char *res, int *ressize );

struct fvm2_native_proc {
  uint32_t procid;
  fvm2_native_cb cb;
};

static int native_nop( char *args, int argsize, char *res, int *ressize ) {
  *ressize = 0;
  return 0;
}

static int native_puts( char *args, int argsize, char *res, int *ressize ) {
  printf( "%s\n", args );
  *ressize = 0;
  return 0;
}

static struct fvm2_native_proc native_procs[] =
  {
   { 0, native_nop },
   { 1, native_puts },
   { 0, NULL }
  };

int fvm2_native_call( uint32_t procid, char *args, int argsize, char *res, int *ressize ) {
  int i;
  i = 0;
  while( native_procs[i].cb ) {
    if( native_procs[i].procid == procid ) {
      return native_procs[i].cb( args, argsize, res, ressize );
    }
    i++;
  }
  
  return -1;
}

