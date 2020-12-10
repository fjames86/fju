

#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <Winsock2.h>
#include <Windows.h>
#define strcasecmp _stricmp
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

#include <fju/fvm.h>
#include <fju/mmf.h>
#include <fju/rpc.h>
#include <fju/log.h>
#include <fju/raft.h>
#include <fju/hostreg.h>
#include <fju/rpcd.h>
#include <fju/programs.h>
#include <fju/freg.h>
#include <fju/sec.h>
#include <fju/cht.h>

#include "fvm-private.h"

static struct log_s *openlogfile( struct fvm_state *state, uint32_t addr, struct log_s *log ) {
  struct log_s *logp;
  char logname[64];
  char *strp;
  int sts;
  
  logp = NULL;
  memset( logname, 0, sizeof(logname) );
  if( addr ) {
    strp = fvm_getptr( state, addr, 0 );
    if( strp ) {
      strncpy( logname, strp, sizeof(logname) - 8 );
      strcat( logname, ".log" );
      sts = log_open( mmf_default_path( logname, NULL ), NULL, log );
      if( !sts ) logp = log;
    }
  }
  return logp;  
}

int fvm_syscall( struct fvm_state *state, uint16_t syscallid ) {
  switch( syscallid ) {
  case 1:
    /* LogWrite(name,flags,len,buf) */
    {
      char *buf;
      uint32_t len, flags;
      struct log_s log, *logp;
      struct log_entry entry;
      struct log_iov iov[1];
      uint32_t addr;
      
      addr = fvm_stack_read( state, 4 ); /* bufaddr */
      buf = fvm_getptr( state, addr, 0 );
      len = fvm_stack_read( state, 8 ); /* buflen */
      flags = fvm_stack_read( state, 12 );
      addr = fvm_stack_read( state, 16 ); /* name address */
      logp = openlogfile( state, addr, &log );
      
      memset( &entry, 0, sizeof(entry) );
      iov[0].buf = buf;
      iov[0].len = buf ? len : 0;
      entry.iov = iov;
      entry.niov = 1;
      entry.flags = flags;
      log_write( logp, &entry );

      if( logp ) log_close( logp );
    }
    break;
  case 2:
    /* LogNext(name,prevHigh,prevLow,var high,var low); */
    {
      struct log_s log, *logp;
      struct log_entry entry;
      uint32_t addr, idlow, idhigh, addrlow, addrhigh;
      uint64_t id;
      int ne, sts;
      
      addrlow = fvm_stack_read( state, 4 );
      addrhigh = fvm_stack_read( state, 8 );

      idlow = fvm_stack_read( state, 12 );
      idhigh = fvm_stack_read( state, 16 );
      id = (((uint64_t)idhigh) << 32) | (uint64_t)idlow;
    
      addr = fvm_stack_read( state, 20 );
      logp = openlogfile( state, addr, &log );
      
      memset( &entry, 0, sizeof(entry) );
      sts = log_read( logp, id, &entry, 1, &ne );
      if( sts || !ne ) {
	fvm_write_u32( state, addrlow, 0 );
	fvm_write_u32( state, addrhigh, 0 );
      } else {
	fvm_write_u32( state, addrlow, entry.id & 0xffffffff );
	fvm_write_u32( state, addrhigh, (entry.id >> 32) & 0xffffffff );
      }

      if( logp ) log_close( logp );
    }
    break;
  case 3:
    /* LogRead(logname,idhigh,idlow,len,buf, var lenp) */
    {
      struct log_s log, *logp;
      uint32_t idlow, idhigh, lenpaddr, nameaddr, bufaddr;
      uint64_t id;
      int sts, len, lenp;
      char *bufp;
	
      lenpaddr = fvm_stack_read( state, 4 );
      bufaddr = fvm_stack_read( state, 8 );
      bufp = fvm_getptr( state, bufaddr, 1 );
      len = fvm_stack_read( state, 12 );
      idlow = fvm_stack_read( state, 16 );
      idhigh = fvm_stack_read( state, 20 );
      id = (((uint64_t)idhigh) << 32) | (uint64_t)idlow;
      
      nameaddr = fvm_stack_read( state, 24 );
      logp = openlogfile( state, nameaddr, &log );

      sts = log_read_buf( logp, id, bufp, len, &lenp );
      fvm_write_u32( state, lenpaddr, sts ? 0 : lenp );

      if( logp ) log_close( logp );
    }
    break;
  case 4:
    /*  FregNext(path,name,entryname,entrytype,result) */
    break;
  case 5:
    /* FregReadInt(path,var int) */
    break;
  case 6:
    /* FregReadString(path,str) */
    break;
  case 7:
    /* FregReadOpaque(path,res,reslen) */
    break;
  case 8:
    /* FregWriteInt(path,int) */
    break;
  case 9:
    /* FregWritString(path,string)*/
    break;
  case 10:
    /* FregWrwiteOpaque(path,len,vva)*/
    break;
  case 11:
    /* FregSubKey(path,name) */
    break;
  case 12:
    /* FregReadU64 */
    break;
  case 13:
    /* FregWriteU64 */
    break;
  case 14:
    {
      /* HostRegLocalId(var h : u32, var l : u32) */
      uint64_t id;
      uint32_t haddr, laddr;

      laddr = fvm_stack_read( state, 4 );
      haddr = fvm_stack_read( state, 8 );      
      id = hostreg_localid();
      fvm_write_u32( state, laddr, id & 0xffffffff );
      fvm_write_u32( state, haddr, id >> 32 );
    }
    break;
  case 15:
    /* HostRegNameById */
    break;
  case 16:
    /* HostregIdByName */
    break;
  case 17:
    {
      /* RpcNow(var high:int,var low :int ) */
      uint64_t now;
      uint32_t haddr, laddr;

      laddr = fvm_stack_read( state, 4 );
      haddr = fvm_stack_read( state, 8 );      
      now = rpc_now();
      fvm_write_u32( state, laddr, now & 0xffffffff );
      fvm_write_u32( state, haddr, now >> 32 );
    }
    break;    
  case 18:    
    {
      /* SecRandU32(var r : int); */
      uint32_t raddr, r;

      raddr = fvm_stack_read( state, 4 );
      r = sec_rand_uint32();
      fvm_write_u32( state, raddr, r );
    }
    break;
  case 19:
    {
      /* Sprintf(fmt:string,arg1:int,arg2:int,arg3:int,arg4:int,result:string,resultlen:int) */
      uint32_t reslen, resaddr, argaddr[4];
      uint32_t fmtaddr;
      char *p, *q, *fmt, *result, *strp;
      int iarg;
      
      reslen = fvm_stack_read( state, 4 );
      resaddr = fvm_stack_read( state, 8 );
      argaddr[3] = fvm_stack_read( state, 12 );
      argaddr[2] = fvm_stack_read( state, 16 );
      argaddr[1] = fvm_stack_read( state, 20 );
      argaddr[0] = fvm_stack_read( state, 24 );
      fmtaddr = fvm_stack_read( state, 28 );

      fmt = fvm_getptr( state, fmtaddr, 0 );
      result = fvm_getptr( state, resaddr, 1 );
      if( !result ) reslen = 0;
      p = fmt;
      q = result;
      iarg = 0;
      while( reslen > 0 ) {
	if( !*p ) break;
	if( *p == '%' ) {
	  p++;
	  switch( *p ) {
	  case 's':
	    strp = fvm_getptr( state, argaddr[iarg], 0 );
	    sprintf( q, "%s", strp ? strp : "" );
	    iarg++;
	    reslen -= strlen( q );
	    q += strlen( q );
	    p++;
	    break;
	  case 'u':
	    sprintf( q, "%u", argaddr[iarg] );
	    iarg++;
	    reslen -= strlen( q );
	    q += strlen( q );
	    p++;	    
	    break;
	  case 'd':
	    sprintf( q, "%d", argaddr[iarg] );
	    iarg++;
	    reslen -= strlen( q );
	    q += strlen( q );
	    p++;	    
	    break;
	  case 'x':
	    sprintf( q, "%x", argaddr[iarg] );
	    iarg++;
	    reslen -= strlen( q );
	    q += strlen( q );
	    p++;	    
	    break;
	  case '%':
	    *q = '%';
	    q++;
	    p++;
	    break;
	  }
	} else {
	  *q = *p;
	  p++;
	  q++;
	  reslen--;
	}
      }
      
    }
    break;
  case 20:
    /* XdrDecodeU32 */
    break;
  case 21:
    /* XdrDecodeU64 */
    break;
  case 22:
    /* XdrDecodeString */
    break;
  case 23:
    /* XdrDecodeOpaque */
    break;
  case 24:
    /* XdrEncodeInt */
    break;
  case 25:
    /* XdrEncodeU64 */
    break;
  case 26:
    /* XdrEncodeString */
    break;
  case 27:
    /* XdrEncodeOpaque */
    break;    
  case 28:
    /* LogLastId(logname,var idHigh,var idLow) */
    {
      struct log_s log, *logp;
      uint32_t idlowaddr, idhighaddr, nameaddr;
      struct log_prop prop;
      
      idlowaddr = fvm_stack_read( state, 4 );
      idhighaddr = fvm_stack_read( state, 8 );
      nameaddr = fvm_stack_read( state, 12 );
      logp = openlogfile( state, nameaddr, &log );

      log_prop( logp, &prop );
      fvm_write_u32( state, idlowaddr, prop.last_id & 0xffffffff );
      fvm_write_u32( state, idhighaddr, prop.last_id >> 32 );
      
      if( logp ) log_close( logp );      
    }
    
    break;
  case 29:
    /* ChtRead(keylen,keybuf,reslen,resbuf,var len) */
    {
      uint32_t keylen,keyaddr,reslen,resaddr,rlenaddr;
      char *keybuf, *resbuf;
      struct cht_entry entry;
      
      rlenaddr = fvm_stack_read( state, 4 );
      resaddr = fvm_stack_read( state, 8 );
      reslen = fvm_stack_read( state, 12 );      
      keyaddr = fvm_stack_read( state, 16 );
      keylen  = fvm_stack_read( state, 20 );
      resbuf = fvm_getptr( state, resaddr, 1 );
      keybuf = fvm_getptr( state, keyaddr, 0 );

      memset( &entry, 0, sizeof(entry) );
      if( keylen < 16 ) keybuf = NULL;
      if( keybuf ) {
	cht_read( NULL, keybuf, resbuf, resbuf ? reslen : 0, &entry );
      }
      fvm_write_u32( state, rlenaddr, entry.flags & CHT_SIZE_MASK );
    }      
    break;
  case 30:
    /* ChtWrite(keylen,keybuf,reslen,resbuf) */
    {
      uint32_t keylen,keyaddr,reslen,resaddr;
      char *keybuf, *resbuf;
      struct cht_entry entry;
      
      resaddr = fvm_stack_read( state, 4 );
      reslen = fvm_stack_read( state, 8 );      
      keyaddr = fvm_stack_read( state, 12 );
      keylen  = fvm_stack_read( state, 16 );
      resbuf = fvm_getptr( state, resaddr, 0 );
      keybuf = fvm_getptr( state, keyaddr, 0 );
      if( keylen < CHT_KEY_SIZE ) keybuf = NULL;
      
      memset( &entry, 0, sizeof(entry) );
      if( keybuf ) memcpy( entry.key, keybuf, CHT_KEY_SIZE );
      cht_write( NULL, &entry, resbuf, resbuf ? reslen : 0 );
    }
    break;
  case 31:
    /* ChtDelete(keylen,keybuf) */
    {
      uint32_t keylen,keyaddr;
      char *keybuf;
      
      keyaddr = fvm_stack_read( state, 4 );
      keylen  = fvm_stack_read( state, 8 );
      keybuf = fvm_getptr( state, keyaddr, 0 );
      if( keylen < CHT_KEY_SIZE ) keybuf = NULL;            
      if( keybuf ) cht_delete( NULL, keybuf );
    }
    break;
    /* TODO: lots of syscalls required to be implemented */
  default:
    return -1;
  }
  return 0;
}
