
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
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
#include <fju/cht.h>
#include <fju/raft.h>


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

static int native_logstrlvl( struct fvm_s *state, int lvl ) {  
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
    if( !sts ) log_writef( NULL, lvl, "%.*s", len, str + 4 );
  }
  
  return 0;
}


/* procedure logstr(str : string) */
static int native_logstr( struct fvm_s *state ) {
  return native_logstrlvl( state, LOG_LVL_INFO );
}
/* procedure logdebug(str : string) */
static int native_logdebug( struct fvm_s *state ) {
  return native_logstrlvl( state, LOG_LVL_DEBUG );
}
/* procedure logwarn(str : string) */
static int native_logwarn( struct fvm_s *state ) {
  return native_logstrlvl( state, LOG_LVL_WARN );
}
/* procedure logerror(str : string) */
static int native_logerror( struct fvm_s *state ) {
  return native_logstrlvl( state, LOG_LVL_ERROR );
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
  bufp = fvm_getaddr_writable( state, bufaddr );
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
  state->flags |= FVM_STATE_DIRTY;
  
  return 0;
}

/* procedure nextlogentry(logname : string, var idhigh : integer, var idlow : integer, result : integer) */
static int native_nextlogentry( struct fvm_s *state ) {
  uint32_t resultaddr, idlowaddr, idhighaddr, idlow, idhigh;
  uint64_t id;
  int sts, ne;
  char str[256];
  struct log_entry entry;
  
  native_readstr( state, ntohl( fvm_stack_read( state, 16 ) ), str, sizeof(str) );
  resultaddr = ntohl( fvm_stack_read( state, 4 ) );
  idlowaddr = ntohl( fvm_stack_read( state, 8 ) );
  idhighaddr = ntohl( fvm_stack_read( state, 12 ) );
  idhigh = ntohl( fvm_read( state, idhighaddr ) );
  idlow = ntohl( fvm_read( state, idlowaddr ) );
  
  if( strcmp( prevlogname, str ) != 0 ) {
    if( prevlog.pid ) log_close( &prevlog );
    memset( &prevlog, 0, sizeof(prevlog) );
    strcpy( prevlogname, str );  
    strcat( str, ".log" );

    fvm_printf( "native_nextlogentry opening file \"%s\"\n", str );
    sts = log_open( mmf_default_path( str, NULL ), NULL, &prevlog );
    if( sts ) {
      strcpy( prevlogname, "" );
      return -1;
    }
  }

  id = (((uint64_t)idhigh) << 32) | (uint64_t)idlow;
  sts = log_read( &prevlog, id, &entry, 1, &ne );
  if( sts || !ne ) {
    fvm_write( state, resultaddr, htonl( 0 ) );
  } else {
    fvm_write( state, idlowaddr, htonl( entry.id & 0xffffffff ) );
    fvm_write( state, idhighaddr, htonl( entry.id >> 32 ) );
    fvm_write( state, resultaddr, htonl( 1 ) );
  }
  
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
  buf = fvm_getaddr_writable( state, bufaddr );
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

/* procedure invoke(progid : integer, procid : integer, args : opaque, argcount : integer, res : opaque, var rescount : integer); */
static int native_invoke( struct fvm_s *state ) {
  uint32_t progid, procid, argaddr, argcount, resaddr, rescountaddr, rescount, ressize;
  char *argp = NULL, *resp = NULL, *p;
  int sts;
  struct fvm_s state2;
  
  rescountaddr = ntohl( fvm_stack_read( state, 4 ) );
  rescount = ntohl( fvm_read( state, rescountaddr ) );
  resaddr = ntohl( fvm_stack_read( state, 8 ) );
  if( rescount > 0 ) {
    resp = fvm_getaddr( state, resaddr );
    if( !resp ) goto bad;
  }
  argcount = ntohl( fvm_stack_read( state, 12 ) );
  argaddr = ntohl( fvm_stack_read( state, 16 ) );
  if( argcount > 0 ) {
    argp = fvm_getaddr( state, argaddr );
    if( !argp ) goto bad;
  }
  procid = ntohl( fvm_stack_read( state, 20 ) );
  progid = ntohl( fvm_stack_read( state, 24 ) );

  sts = fvm_state_init( &state2, progid, procid );
  if( sts ) goto bad;
  state2.nsteps = state->nsteps;
  if( argcount > 0 ) {
    fvm_set_args( &state2, argp, argcount );
  }
  sts = fvm_run( &state2, 0 );
  state->nsteps = state2.nsteps;
  if( sts ) goto bad;

  ressize = fvm_get_res( &state2, &p );
  if( ressize != -1 ) {
    if( ressize > rescount ) ressize = rescount;
    memcpy( resp, p, ressize );
  }
  fvm_write( state, rescountaddr, htonl( ressize ) );
  
  return 0;
  
 bad:
  fvm_write( state, rescountaddr, htonl( -1 ) );
  return 0;
}

/* Procedure NextRegEntry(path : str, name : string, var type : integer); */
static int native_nextregentry( struct fvm_s *state ) {
  uint32_t pathaddr, nameaddr, typeaddr;
  char path[256], name[64];
  int sts;
  struct freg_entry entry;
  uint64_t id;
  char *nstr;
  
  pathaddr = ntohl( fvm_stack_read( state, 12 ) );
  nameaddr = ntohl( fvm_stack_read( state, 8 ) );
  typeaddr = ntohl( fvm_stack_read( state, 4 ) );
  native_readstr( state, pathaddr, path, sizeof(path) );
  native_readstr( state, nameaddr, name, sizeof(name) );
  
  sts = freg_subkey( NULL, 0, path, 0, &id );
  if( sts ) goto done;
  if( strcmp( name, "" ) == 0 ) {
    sts = freg_next( NULL, id, 0, &entry );
    if( sts ) goto done;    
  } else {
    sts = freg_entry_by_name( NULL, id, name, &entry, NULL );
    if( !sts ) sts = freg_next( NULL, id, entry.id, &entry );
    if( sts ) goto done;    
  }

 done:
  if( sts ) {
    fvm_write( state, nameaddr, 0 );
    fvm_write( state, typeaddr, 0 );
  } else {
    fvm_write( state, nameaddr, htonl( strlen( entry.name ) ) );
    if( !sts ) {
      nstr = fvm_getaddr_writable( state, nameaddr + 4 );
      if( nstr ) memcpy( nstr, entry.name, strlen( entry.name ) );
    }
    fvm_write( state, typeaddr, htonl( entry.flags & FREG_TYPE_MASK ) );
  }
  
  return 0;
}

static int native_readregint( struct fvm_s *state ) {
  uint32_t pathaddr, intaddr, u;
  char path[256];
  int sts;
  
  pathaddr = ntohl( fvm_stack_read( state, 8 ) );
  intaddr = ntohl( fvm_stack_read( state, 4 ) );  
  native_readstr( state, pathaddr, path, sizeof(path) );

  u = 0;
  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_UINT32, (char *)&u, sizeof(u), NULL );
  (void)(sts);
  fvm_write( state, intaddr, htonl( u ) );

  return 0;
}

static int native_readregstring( struct fvm_s *state ) {
  uint32_t pathaddr, straddr, strsize;
  char path[256];
  char *strp;
  int sts;
  
  pathaddr = ntohl( fvm_stack_read( state, 12 ) );
  straddr = ntohl( fvm_stack_read( state, 8 ) );
  strp = fvm_getaddr_writable( state, straddr );
  if( !strp ) return 0;
	      
  strsize = ntohl( fvm_stack_read( state, 4 ) );    
  native_readstr( state, pathaddr, path, sizeof(path) );

  sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_STRING, strp + 4, strsize, NULL );
  if( sts ) {
    fvm_write( state, straddr, 0 );
  } else {
    fvm_write( state, straddr, htonl( strlen( strp + 4 ) ) );
  }
	       
  return 0;
}

static int native_writeregint( struct fvm_s *state ) {
  uint32_t pathaddr, intval;
  char path[256];
  int sts;
  
  pathaddr = ntohl( fvm_stack_read( state, 8 ) );
  intval = ntohl( fvm_stack_read( state, 4 ) );  
  native_readstr( state, pathaddr, path, sizeof(path) );

  sts = freg_ensure( NULL, 0, path, FREG_TYPE_UINT32, (char *)&intval, sizeof(intval), NULL );
  (void)(sts);
  
  return 0;
}

static int native_writeregstring( struct fvm_s *state ) {
  uint32_t pathaddr, straddr, strsize;
  char path[256];
  char *strp;
  int sts;
  
  pathaddr = ntohl( fvm_stack_read( state, 8 ) );
  straddr = ntohl( fvm_stack_read( state, 4 ) );
  strp = fvm_getaddr( state, straddr );
  if( !strp ) return 0;
	      
  strsize = ntohl( fvm_read( state, straddr ) );
  native_readstr( state, pathaddr, path, sizeof(path) );

  sts = freg_ensure( NULL, 0, path, FREG_TYPE_STRING, strp + 4, strsize, NULL );
  (void)(sts);
  freg_put( NULL, 0, path, FREG_TYPE_STRING, strp + 4, strsize, NULL );
	       
  return 0;
}

static int native_readcht( struct fvm_s *state ) {
  uint32_t keyaddr, bufaddr, sizeaddr;
  int sts, size;
  char *keybuf, *buf;
  struct cht_entry entry;
  struct cht_s cht;

  sts = cht_open( NULL, &cht, NULL );
  if( sts ) {
    return -1;
  }
  
  keyaddr = ntohl( fvm_stack_read( state, 12 ) );
  keybuf = fvm_getaddr( state, keyaddr );
  
  bufaddr = ntohl( fvm_stack_read( state, 8 ) );
  buf = fvm_getaddr_writable( state, bufaddr );
  
  sizeaddr = ntohl( fvm_stack_read( state, 4 ) );  
  size = ntohl( fvm_read( state, sizeaddr ) );

  if( !buf ) size = 0;

  memset( &entry, 0, sizeof(entry) );
  sts = cht_read( &cht, keybuf, buf, size, &entry );
  if( sts ) {
    fvm_write( state, sizeaddr, htonl( 0 ) );
  } else {
    fvm_write( state, sizeaddr, htonl( entry.flags & CHT_SIZE_MASK ) );
  }

  cht_close( &cht );
  
  return 0;
}

static int native_writecht( struct fvm_s *state ) {
  uint32_t keyaddr, bufaddr, size;
  int sts;
  char *keybuf, *buf;
  struct cht_entry entry;
  struct cht_s cht;

  sts = cht_open( NULL, &cht, NULL );
  if( sts ) return -1;
  
  keyaddr = ntohl( fvm_stack_read( state, 12 ) );
  keybuf = fvm_getaddr( state, keyaddr );
  
  bufaddr = ntohl( fvm_stack_read( state, 8 ) );
  buf = fvm_getaddr_writable( state, bufaddr );
  
  size = ntohl( fvm_stack_read( state, 4 ) );  

  if( !buf ) size = 0;

  memset( &entry, 0, sizeof(entry) );
  memcpy( entry.key, keybuf, CHT_KEY_SIZE );
  sts = cht_write( &cht, &entry, buf, size );
  (void)(sts);
  
  cht_close( &cht );
  
  return 0;
}

static int native_raftcommand( struct fvm_s *state ) {
  int sts;
  uint32_t clidlow, clidhigh, bufaddr, buflen;
  uint64_t clid;
  char *buf;
  
  clidhigh = ntohl( fvm_stack_read( state, 16 ) );
  clidlow = ntohl( fvm_stack_read( state, 12 ) );
  clid = (((uint64_t)clidhigh) << 32) | (uint64_t)clidlow;
  bufaddr = ntohl( fvm_stack_read( state, 8 ) );
  buf = fvm_getaddr( state, bufaddr );  
  buflen = ntohl( fvm_stack_read( state, 4 ) );

  sts = raft_cluster_command( clid, buf, buf ? buflen : 0, NULL );
  (void)sts;
  
  return 0;
}

static int native_fvmclrun( struct fvm_s *state ) {
  uint32_t progid, procid, bufaddr, len;
  char *buf;
  
  len = ntohl( fvm_stack_read( state, 4 ) );
  bufaddr = ntohl( fvm_stack_read( state, 8 ) );
  procid = ntohl( fvm_stack_read( state, 12 ) );
  progid = ntohl( fvm_stack_read( state, 16 ) );
  buf = fvm_getaddr( state, bufaddr );
  
  fvm_cluster_run( 0, progid, procid, buf, buf ? len : 0 );
  
  return 0;
}

static int native_rpcdeventpublish( struct fvm_s *state ) {
  uint32_t evtid, argaddr, arglen;
  char *buf;
  struct xdr_s args;

  arglen = ntohl( fvm_stack_read( state, 4 ) );
  argaddr = ntohl( fvm_stack_read( state, 8 ) );
  evtid = ntohl( fvm_stack_read( state, 12 ) );
  buf = fvm_getaddr( state, argaddr );

  xdr_init( &args, (uint8_t *)(buf ? buf : NULL), buf ? arglen : 0 );
  rpcd_event_publish( evtid, &args );
  
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
   { 11, native_invoke },
   { 12, native_readregint },
   { 13, native_readregstring },   
   { 14, native_writeregint },
   { 15, native_writeregstring },   
   { 16, native_readcht },
   { 17, native_writecht },
   { 18, native_nextlogentry },
   { 19, native_logdebug },
   { 20, native_logwarn },   
   { 21, native_logerror },
   { 22, native_nextregentry },
   { 23, native_raftcommand },
   { 24, native_fvmclrun },
   { 25, native_rpcdeventpublish },
   
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

