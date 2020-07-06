
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

static uint32_t fvm_stack_read( struct fvm_s *state, int depth ) {
  return fvm_read( state, state->reg[FVM_REG_SP] - depth );
}

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
  
  sp = ntohl( fvm_stack_read( state, 4 ) );
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

  addr = ntohl( fvm_stack_read( state, 4 ) );
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
  
  progid = ntohl( fvm_stack_read( state, 4 ) );
  addr = ntohl( fvm_stack_read( state, 8 ) );
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
  
  progid = ntohl( fvm_stack_read( state, 4 ) );
  addr = ntohl( fvm_stack_read( state, 8 ) );
  val = fvm_stack_read( state, 12 );
  
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

static int native_readstr( struct fvm_s *state, uint32_t addr, char *str, int size ) {
  int strlen, i;
  uint32_t *up;
    
  strlen = ntohl( fvm_read( state, addr ) );
  if( strlen >= size - 1 ) strlen = size - 1;
  
  memset( str, 0, size );
  addr += 4;
  up = (uint32_t *)str;
  for( i = 0; i < strlen; i += 4 ) {
    *up = fvm_read( state, addr );
    up++;
    addr += 4;
  }

  return strlen;
}

static int native_progid_by_name( struct fvm_s *state ) {
  uint32_t progid;
  char str[FVM_MAX_NAME];

  native_readstr( state, ntohl( fvm_stack_read( state, 4 ) ), str, sizeof(str) );
  progid = fvm_progid_by_name( str );  
  fvm_push( state, htonl( progid ) );
  
  return 0;
}

static int native_writelog( struct fvm_s *state ) {
  int sts;
  struct log_s log;
  uint32_t bufcount, bufaddr;
  char *bufp;
  char str[FVM_MAX_NAME];
  
  /* int writelog(logname, buf, count */
  
  bufcount = ntohl( fvm_stack_read( state, 4 ) );
  bufaddr = ntohl( fvm_stack_read( state, 8 ) );
  native_readstr( state, ntohl( fvm_stack_read( state, 12 ) ), str, sizeof(str) );
  strcat( str, ".log" );
  
  bufp = fvm_getaddr( state, bufaddr );
  if( !bufp ) return -1;
  
  sts = log_open( mmf_default_path( str, NULL ), NULL, &log );
  if( sts ) return -1;
  log_write_buf( &log, LOG_LVL_INFO, bufp, bufcount, NULL );
  log_close( &log );

  return 0;
}

static int native_readlog( struct fvm_s *state ) {
  int sts, msglen;
  struct log_s log;
  uint32_t bufcount, bufaddr;
  char *bufp;
  char str[FVM_MAX_NAME];
  uint64_t id;
  
  /* readlog(logname, id, buf, count */
  
  bufcount = ntohl( fvm_stack_read( state, 4 ) );
  bufaddr = ntohl( fvm_stack_read( state, 8 ) );
  id = ntohl( fvm_stack_read( state, 12 ) );
  id = (id << 32) | ntohl( fvm_stack_read( state, 16 ) );
  native_readstr( state, ntohl( fvm_stack_read( state, 20 ) ), str, sizeof(str) );
  strcat( str, ".log" );
  
  bufp = fvm_getaddr( state, bufaddr );
  if( !bufp ) return -1;
  
  sts = log_open( mmf_default_path( str, NULL ), NULL, &log );
  if( sts ) return -1;
  log_read_buf( &log, id, bufp, bufcount, &msglen );
  log_close( &log );

  return msglen;
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
   { 8, native_writelog },
   { 9, native_readlog },
   
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

