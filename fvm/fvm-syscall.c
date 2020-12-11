

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
    strp = fvm_getptr( state, addr, 0, 0 );
    if( strp ) {
      strncpy( logname, strp, sizeof(logname) - 8 );
      strcat( logname, ".log" );
      sts = log_open( mmf_default_path( logname, NULL ), NULL, log );
      if( !sts ) logp = log;
    }
  }
  return logp;  
}

static void read_pars( struct fvm_state *state, uint32_t *pars, int n ) {
  int i;
  for( i = 0; i < n; i++ ) {
    pars[n - i - 1] = fvm_stack_read( state, 4 + 4*i );
  }
}

int fvm_syscall( struct fvm_state *state, uint16_t syscallid ) {
  switch( syscallid ) {
  case 1:
    /* LogWrite(name,flags,len,buf) */
    {
      char *buf;
      struct log_s log, *logp;
      struct log_entry entry;
      struct log_iov iov[1];
      uint32_t pars[4];

      read_pars( state, pars, 4 );
      logp = openlogfile( state, pars[0], &log );
      buf = fvm_getptr( state, pars[3], pars[2], 0 );
      
      memset( &entry, 0, sizeof(entry) );
      iov[0].buf = buf;
      iov[0].len = buf ? pars[2] : 0;
      entry.iov = iov;
      entry.niov = 1;
      entry.flags = pars[1];
      log_write( logp, &entry );

      if( logp ) log_close( logp );
    }
    break;
  case 2:
    /* LogNext(name,prevHigh,prevLow,var high,var low); */
    {
      struct log_s log, *logp;
      struct log_entry entry;
      uint64_t id;
      int ne, sts;
      uint32_t pars[5];

      read_pars( state, pars, 5 );
      logp = openlogfile( state, pars[0], &log );      
      id = (((uint64_t)pars[1]) << 32) | (uint64_t)pars[2];
      
      memset( &entry, 0, sizeof(entry) );
      sts = log_read( logp, id, &entry, 1, &ne );
      if( sts || !ne ) {
	fvm_write_u32( state, pars[4], 0 );
	fvm_write_u32( state, pars[3], 0 );
      } else {
	fvm_write_u32( state, pars[4], entry.id & 0xffffffff );
	fvm_write_u32( state, pars[3], (entry.id >> 32) & 0xffffffff );
      }

      if( logp ) log_close( logp );
    }
    break;
  case 3:
    /* LogRead(logname,idhigh,idlow,len,buf, var lenp) */
    {
      struct log_s log, *logp;
      uint64_t id;
      int sts, lenp;
      char *bufp;
      uint32_t pars[6];

      read_pars( state, pars, 6 );
      logp = openlogfile( state, pars[0], &log );
      id = (((uint64_t)pars[1]) << 32) | (uint64_t)pars[2];

      bufp = fvm_getptr( state, pars[4], pars[3], 0 );
      
      sts = log_read_buf( logp, id, bufp, pars[3], &lenp );
      fvm_write_u32( state, pars[5], sts ? 0 : lenp );

      if( logp ) log_close( logp );
    }
    break;
  case 4:
    /*  FregNext(path,name,entryname,entrytype,result) */
    {
      int sts;
      struct freg_entry entry;
      uint64_t parentid;
      char *name, *path, *ename;      
      uint32_t pars[5];
      uint64_t id;
      
      read_pars( state, pars, 5 );
      path = fvm_getptr( state, pars[0], 0, 0 );
      name = fvm_getptr( state, pars[1], 0, 0 );
      ename = fvm_getptr( state, pars[2], 0, 1 );

      id = 0;
      sts = -1;
      parentid = freg_id_by_name( NULL, path, NULL );
      if( !parentid || !name || !path || !ename || (strcmp( name, "" ) == 0) ) {
	id = 0;
      } else {
	sts = freg_entry_by_name( NULL, parentid, name, &entry, NULL );
      }

      if( !sts ) sts = freg_next( NULL, parentid, id, &entry );
      if( sts ) {
	strcpy( ename, "" );
	fvm_write_u32( state, pars[3], 0 );
	fvm_write_u32( state, pars[4], 0 );
      } else {
	strcpy( ename, entry.name );
	fvm_write_u32( state, pars[3], entry.flags );
	fvm_write_u32( state, pars[4], 1 );
      }
       
    }
    break;
  case 5:
    /* FregReadInt(path,var int, var result) */
    {
      int sts;
      char *path;
      uint32_t pars[3], u32;
      read_pars( state, pars, 3 );
      path = fvm_getptr( state, pars[0], 0, 0 );
      sts = -1;
      if( path ) {
	sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_UINT32, (char *)&u32, sizeof(u32), NULL );
      }
      if( sts ) {
	fvm_write_u32( state, pars[1], 0 );
	fvm_write_u32( state, pars[2], 0 );
      } else {
	fvm_write_u32( state, pars[1], u32 );
	fvm_write_u32( state, pars[2], 1 );
      }
					     
    }
    break;
  case 6:
    /* FregReadString(path,str,len,var result) */
    {
      int sts;
      uint32_t pars[4];
      char *path, *str;
      
      read_pars( state, pars, 4 );
      path = fvm_getptr( state, pars[0], 0, 0 );
      str = fvm_getptr( state, pars[1], pars[2], 1 );
      sts = -1;
      if( path && str ) {
	sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_STRING, str, pars[2], NULL );
      }
      if( sts ) {
	strcpy( str, "" );
	fvm_write_u32( state, pars[3], 0 );
      } else {
	fvm_write_u32( state, pars[3], 1 );
      }
    }
    break;
  case 7:
    /* FregReadOpaque(path,len,res,reslen) */
    {
      uint32_t pars[4];
      int lenp;
      char *path, *res;
      
      read_pars( state, pars, 4 );
      path = fvm_getptr( state, pars[0], 0, 0 );
      res = fvm_getptr( state, pars[2], pars[1], 1 );
      lenp = 0;
      if( path ) {
	freg_get_by_name( NULL, 0, path, FREG_TYPE_OPAQUE, res, pars[1], &lenp );
      }
      fvm_write_u32( state, pars[3], lenp );
    }
    break;
  case 8:
    /* FregWriteInt(path,int) */
    {
      uint32_t pars[2];
      char *path;
      read_pars( state, pars, 2 );
      path = fvm_getptr( state, pars[0], 0, 0 );
      if( path ) freg_put( NULL, 0, path, FREG_TYPE_UINT32, (char *)&pars[1], 4, NULL );
    }
    break;
  case 9:
    /* FregWriteString(path,string)*/
    {
      uint32_t pars[2];
      char *path, *str;
      read_pars( state, pars, 2 );
      path = fvm_getptr( state, pars[0], 0, 0 );
      str = fvm_getptr( state, pars[1], 0, 0 );
      if( path ) freg_put( NULL, 0, path, FREG_TYPE_STRING, (char *)(str ? str : ""), str ? strlen( str ) + 1 : 1, NULL );
    }    
    break;
  case 10:
    /* FregWriteOpaque(path,len,buf)*/
    {
      uint32_t pars[3];
      char *path, *buf;
      read_pars( state, pars, 3 );
      path = fvm_getptr( state, pars[0], 0, 0 );
      buf = fvm_getptr( state, pars[2], 0, 0 );
      if( path ) freg_put( NULL, 0, path, FREG_TYPE_OPAQUE, buf, buf ? pars[1] : 0, NULL );
    }        
    break;
  case 11:
    /* FregSubKey(path) */
    {
      uint32_t pars[1];
      char *path;
      read_pars( state, pars, 1 );
      path = fvm_getptr( state, pars[0], 0, 0 );
      if( path ) freg_subkey( NULL, 0, path, FREG_CREATE, NULL );
    }
    break;
  case 12:
    /* FregReadU64(path,var high, var low) */
    {
      uint32_t pars[3];
      char *path;
      int sts;
      uint64_t val;
      
      read_pars( state, pars, 3 );
      path = fvm_getptr( state, pars[0], 0, 0 );

      sts = -1;
      if( path ) {
	sts = freg_get_by_name( NULL, 0, path, FREG_TYPE_UINT64, (char *)&val, 8, NULL );
      }
      if( sts ) {
	fvm_write_u32( state, pars[1], 0 );
	fvm_write_u32( state, pars[2], 0 );
      } else {
	fvm_write_u32( state, pars[1], val >> 32 );
	fvm_write_u32( state, pars[2], val & 0xffffffff );
      }      
      
    }
    break;
  case 13:
    /* FregWriteU64(path,high,low) */
    {
      uint32_t pars[3];
      char *path;
      uint64_t val;
      
      read_pars( state, pars, 3 );
      path = fvm_getptr( state, pars[0], 0, 0 );
      val = ((uint64_t)pars[1] << 32) | pars[2];
      if( path ) freg_put( NULL, 0, path, FREG_TYPE_UINT64, (char *)&val, 8, NULL );
    }
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
      uint32_t pars[6];
      char *str, *fmt;
      char *p, *q, *strp;
      int iarg;

      read_pars( state, pars, 6 );
      str = fvm_getptr( state, pars[0], 0, 0 );
      fmt = fvm_getptr( state, pars[1], 0, 0 );

      if( !str ) break;
      
      p = fmt;
      q = str;
      iarg = 0;
      while( 1 ) {
	if( !*p ) break;
	if( *p == '%' ) {
	  p++;
	  switch( *p ) {
	  case 's':
	    strp = fvm_getptr( state, pars[2 + iarg], 0, 0 );
	    sprintf( q, "%s", strp ? strp : "" );
	    iarg++;
	    q += strlen( q );
	    p++;
	    break;
	  case 'u':
	    sprintf( q, "%u", pars[2 + iarg] );
	    iarg++;
	    q += strlen( q );
	    p++;	    
	    break;
	  case 'd':
	    sprintf( q, "%d", pars[2 + iarg] );
	    iarg++;
	    q += strlen( q );
	    p++;	    
	    break;
	  case 'x':
	    sprintf( q, "%x", pars[2 + iarg] );
	    iarg++;
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
	}
      }
      *q = '\0';
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
      resbuf = fvm_getptr( state, resaddr, 0, 1 );
      keybuf = fvm_getptr( state, keyaddr, 0, 0 );

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
      resbuf = fvm_getptr( state, resaddr, 0, 0 );
      keybuf = fvm_getptr( state, keyaddr, 0, 0 );
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
      keybuf = fvm_getptr( state, keyaddr, 0, 0 );
      if( keylen < CHT_KEY_SIZE ) keybuf = NULL;            
      if( keybuf ) cht_delete( NULL, keybuf );
    }
    break;
    /* TODO: lots of syscalls required to be implemented */
  case 32:
    /* Puts(str) */
    {
      uint32_t pars[1];
      char *str;
      read_pars( state, pars, 1 );
      str = fvm_getptr( state, pars[0], 0, 0 );
      if( str ) puts( str );	
    }
    break;
  default:
    return -1;
  }
  return 0;
}
