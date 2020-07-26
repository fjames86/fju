
#ifdef WIN32
#include <WinSock2.h>
#include <Windows.h>
#else
#include <arpa/inet.h>
#endif


#include "fvm-private.h"

#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/sec.h>
#include <fju/log.h>
#include <fju/freg.h>


/*
 * TODO: go through and sort out the calling convention. 
 * These are written assuming cdecl but we seem to be settling on pascal calling convention.
 * cdecl: push args from right to left, R0 contains return value.
 * pascal: push args from left to right. No return value. Any return values are passed as pointers.
 * note that this means variable length parameters are not possible e.g. sprintf 
 */

static uint32_t fvm_stack_read( struct fvm_s *state, int depth ) {
  return fvm_read( state, state->reg[FVM_REG_SP] - depth );
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

typedef int (*fvm_native_cb)( struct fvm_s *state );

struct fvm_native_proc {
  uint32_t procid;
  fvm_native_cb cb;
};

static int native_nop( struct fvm_s *state ) {
  return 0;
}

/* procedure puts(str : string ) */
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

/* procedure rand(var r : integer) */
static int native_rand( struct fvm_s *state ) {
  /* return random number */
  uint32_t addr = ntohl( fvm_stack_read( state, 4 ) );
  fvm_write( state, addr, sec_rand_uint32() );
  return 0;
}

/* procedure now(var nowhigh : integer, var nowlow : integer) */
static int native_now( struct fvm_s *state ) {
  /* return current time */
  uint32_t addrl, addrh;
  uint64_t now = rpc_now();
  
  /* now(uint32_t *now) */  
  addrh = ntohl( fvm_stack_read( state, 8 ) );
  addrl = ntohl( fvm_stack_read( state, 4 ) );  

  fvm_write( state, addrh, htonl( now >> 32 ) );
  fvm_write( state, addrl, htonl( now & 0xffffffff ) );
  
  return 0;
}

/* procedure logstr(str : string) */
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

/* procedure ProgidByName( str : string, var progid : integer ) */
static int native_progid_by_name( struct fvm_s *state ) {
  uint32_t progid, addr;
  char str[FVM_MAX_NAME];

  addr = ntohl( fvm_stack_read( state, 4 ) );  
  native_readstr( state, ntohl( fvm_stack_read( state, 8 ) ), str, sizeof(str) );
  
  progid = fvm_progid_by_name( str );
  fvm_write( state, addr, htonl( progid ) );
  return 0;
}

/* procedure ProcidByName( progid : integer, str : string, var procid : integer ) */
static int native_procid_by_name( struct fvm_s *state ) {
  uint32_t progid, addr, procid;
  char str[FVM_MAX_NAME];
  struct fvm_module *m;
  
  addr = ntohl( fvm_stack_read( state, 4 ) );  
  native_readstr( state, ntohl( fvm_stack_read( state, 8 ) ), str, sizeof(str) );
  progid = ntohl( fvm_stack_read( state, 12 ) );

  m = fvm_module_by_progid( progid );
  if( !m ) return -1;
  
  procid = fvm_symbol_index( m, str );
  fvm_write( state, addr, htonl( procid ) );
  return 0;
}

static struct log_s prevlog;
static char prevlogname[64];

/* procedure writelog(logname : string, buf : opaque, count : integer) */
static int native_writelog( struct fvm_s *state ) {
  int sts;
  uint32_t bufcount, bufaddr;
  char *bufp;
  char str[FVM_MAX_NAME];

  native_readstr( state, ntohl( fvm_stack_read( state, 12 ) ), str, sizeof(str) );
  bufaddr = ntohl( fvm_stack_read( state, 8 ) );
  bufp = fvm_getaddr( state, bufaddr );
  if( !bufp ) return -1;  
  bufcount = ntohl( fvm_stack_read( state, 4 ) );

  if( strcmp( prevlogname, str ) != 0 ) {
    if( prevlog.pid ) log_close( &prevlog );
    memset( &prevlog, 0, sizeof(prevlog) );
    strcpy( prevlogname, str );  
    strcat( str, ".log" );

    fvm_printf( "native_writelog opening file \"%s\" count=%u\n", str, bufcount );
    sts = log_open( mmf_default_path( str, NULL ), NULL, &prevlog );
    if( sts ) {
      strcpy( prevlogname, "" );
      return -1;
    }
  }
    
  log_write_buf( &prevlog, LOG_LVL_INFO, bufp, bufcount, NULL );

  return 0;
}

/* procedure readlog(logname : string, idhigh : integer, idlow : integer, buf : opaque, var count : integer ) */
static int native_readlog( struct fvm_s *state ) {
  int sts, msglen;
  uint32_t bufaddr, countaddr, count;
  char *bufp;
  char str[FVM_MAX_NAME];
  uint64_t id;
  struct log_entry entry;
  struct log_iov iov[1];

  native_readstr( state, ntohl( fvm_stack_read( state, 20 ) ), str, sizeof(str) );
  id = ntohl( fvm_stack_read( state, 16 ) );
  id = (id << 32) | ntohl( fvm_stack_read( state, 12 ) );
  bufaddr = ntohl( fvm_stack_read( state, 8 ) );
  bufp = fvm_getaddr( state, bufaddr );
  if( !bufp ) return -1;  
  countaddr = ntohl( fvm_stack_read( state, 4 ) );
  count = ntohl( fvm_read( state, countaddr ) );
  
  if( strcmp( prevlogname, str ) != 0 ) {
    if( prevlog.pid ) log_close( &prevlog );
    memset( &prevlog, 0, sizeof(prevlog) );
    strcpy( prevlogname, str );  
    strcat( str, ".log" );

    fvm_printf( "native_writelog opening file \"%s\" count=%u\n", str, count );
    sts = log_open( mmf_default_path( str, NULL ), NULL, &prevlog );
    if( sts ) {
      strcpy( prevlogname, "" );
      return -1;
    }
  }
  
  
  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = bufp;
  iov[0].len = count;
  entry.iov = iov;
  entry.niov = 1;
  msglen = log_read_entry( &prevlog, id, &entry );
  if( msglen == 0 ) {
    msglen = entry.msglen;
  }

  fvm_write( state, countaddr, htonl( msglen ) );
  return 0;
}

/* declare procdure(dest :string, destsize : integer, fmt : string, args : opaque) */
static int native_sprintf( struct fvm_s *state ) {
  uint32_t bufaddr, fmtaddr, bufsize, argsaddr;
  char fmt[1024];
  char argstr[256];
  char *p, *q, *buf;
  int n, argi, blen;
  
  /* sprintf( buf, bufsize, fmt, ... ) */
  bufaddr = ntohl( fvm_stack_read( state, 16 ) );
  buf = fvm_getaddr( state, bufaddr );
  if( !buf ) return -1;  
  bufsize = ntohl( fvm_stack_read( state, 12 ) );
  
  /* check buffer */
  if( !((bufaddr >= FVM_ADDR_DATA && bufaddr < (FVM_ADDR_DATA + state->datasize - bufsize)) ||
	(bufaddr >= FVM_ADDR_STACK && bufaddr < (FVM_ADDR_STACK + FVM_MAX_STACK - bufsize))) ) {
    fvm_printf( "Bad buffer out of range or too large addr=%x size=%u\n", bufaddr, bufsize );
    return -1;
  }
  
  fmtaddr = ntohl( fvm_stack_read( state, 8 ) );
  native_readstr( state, fmtaddr, fmt, sizeof(fmt) );

  argsaddr = ntohl( fvm_stack_read( state, 4 ) );

  /* parse format string, getting args when required */
  p = fmt;
  q = buf + 4;
  blen = 0;
  argi = 0;
  while( *p ) {
    if( *p == '%' ) {
      p++;
      switch( *p ) {
      case 's':
	native_readstr( state, ntohl( fvm_read( state, argsaddr + argi*4 ) ), argstr, sizeof(argstr) );
	n = sprintf( q, "%s", argstr );
	blen += n;
	q += n;
	p++;
	break;
      case 'd':
	n = sprintf( q, "%d", ntohl( fvm_read( state, argsaddr + argi*4 ) ) );
	blen += n;
	q += n;
	p++;	
	break;
      case 'u':
	n = sprintf( q, "%u", ntohl( fvm_read( state, argsaddr + argi*4 ) ) );
	blen += n;
	q += n;
	p++;		
	break;
      case 'x':
	n = sprintf( q, "%x", ntohl( fvm_read( state, argsaddr + argi*4 ) ) );
	blen += n;
	q += n;
	p++;		
	break;
      default:
	break;
      }
      argi++;
    } else {
      *q = *p;
      p++;
      q++;
      blen++;
    }
  }

  *((uint32_t *)buf) = htonl( blen );
  return 0;
}

struct fvm_yield_iterator {
  struct rpc_iterator iter;
  struct fvm_s state;
};

static void fvm_yield_cb( struct rpc_iterator *iter ) {
  struct fvm_yield_iterator *it = (struct fvm_yield_iterator *)iter;
  
  /* unregister ourselves */
  rpc_iterator_unregister( iter );

  fvm_log( LOG_LVL_DEBUG, "Running yielded state" );
  fvm_run( &it->state, 0 );
  
  free( it );  
}

/* procedure yield( timeout : integer, flags : integer, var result ) */
static int native_yield( struct fvm_s *state ) {
  /* save state to an interator, which gets scheduled to run immediately. */
  struct fvm_yield_iterator *iter;
  uint32_t flags, timeout, resultaddr;

  /* yield(timeout, flags) */
  
  /* No op if not running as service */
  if( !rpcdp() ) {
    fvm_log( LOG_LVL_DEBUG, "Yield not rpcd" );
    return 0;
  }

  resultaddr = ntohl( fvm_stack_read( state, 4 ) );
  flags = ntohl( fvm_stack_read( state, 8 ) );
  timeout = ntohl( fvm_stack_read( state, 12 ) );
  
  fvm_log( LOG_LVL_DEBUG, "Yielding state timeout=%u flags=%x", timeout, flags ); 
  
  iter = malloc( sizeof(*iter) );
  memset( iter, 0, sizeof(*iter) );
  memcpy( &iter->state, state, sizeof(*state) );
  /* result register receives 1 if resuming from yield */  
  fvm_write( &iter->state, resultaddr, htonl( 1 ) );
  
  iter->iter.timeout = timeout ? rpc_now() + timeout : 0;
  iter->iter.period = timeout;
  iter->iter.cb = fvm_yield_cb;

  rpc_iterator_register( &iter->iter );

  if( flags & 1 ) {
    /* fork flag - continue execution i.e. don't yield */
    fvm_log( LOG_LVL_DEBUG, "Yield fork" );
  } else {
    /* don't fork, terminate execution immediately */
    state->flags |= FVM_STATE_YIELD;
    fvm_log( LOG_LVL_DEBUG, "Yield no fork" );
  }

  /* if forking, execution will continue and result register receives 0, as opposed to child that receives 1 */
  fvm_write( state, resultaddr, 0 );
  
  return 0;
}



static struct fvm_native_proc native_procs[] =
  {
   { 0, native_nop },
   { 1, native_puts },
   { 2, native_rand },
   { 3, native_now },
   { 4, native_logstr },
   { 5, native_progid_by_name },
   { 6, native_procid_by_name },   
   { 7, native_writelog },
   { 8, native_readlog },
   { 9, native_sprintf },   
   { 10, native_yield },
   
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

