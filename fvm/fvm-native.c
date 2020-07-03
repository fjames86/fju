
#ifdef WIN32
#include <WinSock2.h>
#include <Windows.h>
#else
#include <arpa/inet.h>
#endif


#include "fvm-private.h"

#include <fju/rpc.h>
#include <fju/sec.h>
#include <fju/log.h>

typedef int (*fvm_native_cb)( struct fvm_s *state );

struct fvm_native_proc {
  uint32_t procid;
  fvm_native_cb cb;
};

static int native_nop( struct fvm_s *state ) {
  return 0;
}

static int native_puts( struct fvm_s *state ) {
  /* pop stack to get address of string */
  uint32_t sp, len;
  char *str;
  
  sp = ntohl( fvm_pop( state ) );
  len = ntohl( fvm_read( state, sp ) );
  str = fvm_getaddr( state, sp + 4 );
  if( !str ) return -1;

  printf( "%.*s", len, str );
  return 0;
}

static int native_rand( struct fvm_s *state ) {
  /* push random onto stack */
  fvm_push( state, sec_rand_uint32() );
  return 0;
}

static int native_now( struct fvm_s *state ) {
  /* push random onto stack */
  uint64_t now = rpc_now();
  fvm_push( state, htonl( now >> 32 ) );
  fvm_push( state, htonl( now & 0xffffffff ) );
  return 0;
}

static int native_logstr( struct fvm_s *state ) {  
  char *str;
  uint32_t addr;

  addr = ntohl( fvm_pop( state ) );
  str = fvm_getaddr( state, addr );

  if( str ) {
    struct xdr_s xdr;
    uint32_t len;
    int sts;
    
    xdr_init( &xdr, (uint8_t *)str, 4 );
    sts = xdr_decode_uint32( &xdr, &len );
    if( !sts ) log_writef( NULL, LOG_LVL_INFO, "%.*s", len, str + 4 );
  }
  
  return 0;
}

static int native_read( struct fvm_s *state ) {
  uint32_t progid, addr;
  struct fvm_module *m;
  
  progid = ntohl( fvm_pop( state ) );
  addr = ntohl( fvm_pop( state ) );
  m = fvm_module_by_progid( progid );
  if( !m ) {
    fvm_push( state, 0 );
    return 0;
  }
  
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + m->header.datasize) ) {
    fvm_push( state, *(uint32_t *)(m->data + (addr - FVM_ADDR_DATA)) );
  } else if( addr >= FVM_ADDR_TEXT && addr < (FVM_ADDR_TEXT + m->header.textsize) ) {
    fvm_push( state, *(uint32_t *)(m->text + (addr - FVM_ADDR_TEXT)) );
  } else {
    fvm_push( state, 0 );
  }

  return 0;
}

static int native_write( struct fvm_s *state ) {
  uint32_t progid, addr, val;
  struct fvm_module *m;
  
  progid = ntohl( fvm_pop( state ) );
  addr = ntohl( fvm_pop( state ) );
  val = fvm_pop( state );
  
  m = fvm_module_by_progid( progid );
  if( !m ) {
    return 0;
  }
  
  if( addr >= FVM_ADDR_DATA && addr < (FVM_ADDR_DATA + m->header.datasize) ) {
    *(uint32_t *)(m->data + (addr - FVM_ADDR_DATA)) = val;
  } else if( addr >= FVM_ADDR_TEXT && addr < (FVM_ADDR_TEXT + m->header.textsize) ) {
    *(uint32_t *)(m->text + (addr - FVM_ADDR_TEXT)) = val;
  } 

  return 0;
}

static int native_progid_by_name( struct fvm_s *state ) {
  int i, strlen;
  uint32_t nameaddr, *up, progid;
  char str[FVM_MAX_NAME];
  
  nameaddr = ntohl( fvm_pop( state ) );
  strlen = ntohl( fvm_read( state, nameaddr ) );
  if( strlen >= FVM_MAX_NAME - 1 ) strlen = FVM_MAX_NAME - 1;

  memset( str, 0, sizeof(str) );
  nameaddr += 4;
  up = (uint32_t *)str;
  for( i = 0; i < strlen; i += 4 ) {
    *up = fvm_read( state, nameaddr );
    up++;
    nameaddr += 4;
  }

  progid = fvm_progid_by_name( str );
  
  fvm_push( state, htonl( progid ) );
  
  return 0;
}


static struct fvm_native_proc native_procs[] =
  {
   { 0, native_nop },
   { 1, native_puts },
   { 2, native_rand },
   { 3, native_now },
   { 4, native_logstr },
   { 5, native_read },
   { 6, native_write },
   { 7, native_progid_by_name },
 
   { 0, NULL }
  };

int fvm_native_call( struct fvm_s *state, uint32_t procid ) {
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

