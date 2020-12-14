

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
    strp = fvm_getstr( state, addr );
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

static void fvm_xcall( struct fvm_state *state ) {
  /* 
   * Special syscall for calling procedures in other modules. 
   * Effectively the same as FvmRun syscall but allows us to 
   * implement Call modname/procname(...) syntactic sugar 
   */
  uint32_t pars[FVM_MAX_PARAM + 2];
  char *modname, *procname;
  struct fvm_module *m;
  int sts, procid, nargs, i, isvar;
  var_t vartype;
  uint32_t u32, sp;
  uint64_t siginfo;
  struct rpc_conn *conn = NULL;
  char *strp, *bufp;
  struct xdr_s args, res;
  char *tmpbufp = NULL;
  
  read_pars( state, pars, FVM_MAX_PARAM + 2 );
  modname = fvm_getstr( state, pars[FVM_MAX_PARAM] );
  if( !modname ) return;
  procname = fvm_getstr( state, pars[FVM_MAX_PARAM + 1] );
  if( !procname ) return;
  m = fvm_module_by_name( modname );
  if( !m ) return;
  procid = fvm_procid_by_name( m, procname );
  if( procid < 0 ) return;

  if( rpcdp() ) {
    conn = rpc_conn_acquire();
    if( !conn ) return;
    tmpbufp = (char *)conn->buf;
  } else {
    tmpbufp = malloc( 32*1024 );
  }
  
  xdr_init( &args, (uint8_t *)tmpbufp, 16*1024 );
  xdr_init( &res, (uint8_t *)tmpbufp + 16*1024, 16*1024 );

  siginfo = m->procs[procid].siginfo;
  nargs = FVM_SIGINFO_NARGS(siginfo);
  for( i = 0; i < nargs; i++ ) {
    vartype = FVM_SIGINFO_VARTYPE(siginfo,i);
    isvar = FVM_SIGINFO_ISVAR(siginfo,i);
    if( !isvar ) {
      switch( vartype ) {
      case VAR_TYPE_U32:
	if( (i < (nargs - 1)) && (FVM_SIGINFO_VARTYPE(siginfo,i + 1) == VAR_TYPE_OPAQUE) ) {
	} else {
	  xdr_encode_uint32( &args, pars[FVM_MAX_PARAM - nargs + i] );
	}
	break;
      case VAR_TYPE_STRING:
	strp = fvm_getstr( state, pars[FVM_MAX_PARAM - nargs + i] );
	xdr_encode_string( &args, strp ? strp : "" );
	break;
      case VAR_TYPE_OPAQUE:
	bufp = fvm_getptr( state, pars[FVM_MAX_PARAM - nargs + i], pars[FVM_MAX_PARAM - nargs + i - 1], 0 );
	xdr_encode_opaque( &args, (uint8_t *)bufp, bufp ? pars[FVM_MAX_PARAM - nargs + i - 1] : 0 );
	break;
      default:
	break;
      }
    }
  }
  
  sts = fvm_run( m, procid, &args, &res );
  if( sts ) goto done;

  sp = state->sp;
  for( i = 0; i < nargs; i++ ) {
    vartype = FVM_SIGINFO_VARTYPE(siginfo,i);
    isvar = FVM_SIGINFO_ISVAR(siginfo,i);
    if( isvar ) {
      switch( vartype ) {
      case VAR_TYPE_U32:
	if( (i < (nargs - 1)) && (FVM_SIGINFO_VARTYPE(siginfo,i + 1) == VAR_TYPE_OPAQUE) ) {
	} else {
	  xdr_decode_uint32( &res, &u32 );
	  fvm_write_u32( state, pars[FVM_MAX_PARAM - nargs + i], u32 );
	}
	break;
      case VAR_TYPE_STRING:
	strp = fvm_getptr( state, FVM_ADDR_STACK + sp, 0, 1 );
	xdr_decode_string( &res, strp, FVM_MAX_STACK - sp );
	fvm_write_u32( state, pars[FVM_MAX_PARAM - nargs + i], FVM_ADDR_STACK + sp );
	u32 = strlen( strp ) + 1;
	if( u32 % 4 ) u32 += 4 - (u32 % 4);
	sp += u32;
	break;
      case VAR_TYPE_OPAQUE:
	bufp = fvm_getptr( state, FVM_ADDR_STACK + sp, 0, 1 );
	u32 = FVM_MAX_STACK - sp;
	xdr_decode_opaque( &res, (uint8_t *)bufp, (int *)&u32 );
	fvm_write_u32( state, pars[FVM_MAX_PARAM - nargs + i - 1], u32 );
	fvm_write_u32( state, pars[FVM_MAX_PARAM - nargs + i], FVM_ADDR_STACK + sp );
	if( u32 % 4 ) u32 += 4 - (u32 % 4);
	sp += u32;
	break;
      default:
	break;
      }
    }
  }

 done:
  if( conn ) rpc_conn_release( conn );
  if( !rpcdp() ) free( tmpbufp );
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
  case 5:
    /*  FregNext(path,name,entryname,var entrytype,var result) */
    {
      int sts;
      struct freg_entry entry;
      uint64_t parentid;
      char *name, *path, *ename;      
      uint32_t pars[5];
      uint64_t id;
      
      read_pars( state, pars, 5 );
      path = fvm_getstr( state, pars[0] );
      name = fvm_getstr( state, pars[1] );
      ename = fvm_getptr( state, pars[2], 0, 1 );

      id = 0;
      sts = -1;
      parentid = freg_id_by_name( NULL, path, NULL );
      if( !parentid || !name || !path || !ename || (strcmp( name, "" ) == 0) ) {	
	id = 0;
	sts = 0;
      } else {	
	sts = freg_entry_by_name( NULL, parentid, name, &entry, NULL );
	if( !sts ) id = entry.id;
      }

      if( !sts ) sts = freg_next( NULL, parentid, id, &entry );
      if( sts ) {
	if( ename ) strcpy( ename, "" );
	fvm_write_u32( state, pars[3], 0 );
	fvm_write_u32( state, pars[4], 0 );
      } else {
	if( ename ) strcpy( ename, entry.name );
	fvm_write_u32( state, pars[3], entry.flags );
	fvm_write_u32( state, pars[4], 1 );
      }
       
    }
    break;
  case 6:
    /* FregReadInt(path,var int, var result) */
    {
      int sts;
      char *path;
      uint32_t pars[3], u32;
      read_pars( state, pars, 3 );
      path = fvm_getstr( state, pars[0] );
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
  case 7:
    /* FregReadString(path,str,len,var result) */
    {
      int sts;
      uint32_t pars[4];
      char *path, *str;
      
      read_pars( state, pars, 4 );
      path = fvm_getstr( state, pars[0] );
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
  case 8:
    /* FregReadOpaque(path,len,res,reslen) */
    {
      uint32_t pars[4];
      int lenp;
      char *path, *res;
      
      read_pars( state, pars, 4 );
      path = fvm_getstr( state, pars[0] );
      res = fvm_getptr( state, pars[2], pars[1], 1 );
      lenp = 0;
      if( path ) {
	freg_get_by_name( NULL, 0, path, FREG_TYPE_OPAQUE, res, pars[1], &lenp );
      }
      fvm_write_u32( state, pars[3], lenp );
    }
    break;
  case 9:
    /* FregWriteInt(path,int) */
    {
      uint32_t pars[2];
      char *path;
      read_pars( state, pars, 2 );
      path = fvm_getstr( state, pars[0] );
      if( path ) freg_put( NULL, 0, path, FREG_TYPE_UINT32, (char *)&pars[1], 4, NULL );
    }
    break;
  case 10:
    /* FregWriteString(path,string)*/
    {
      uint32_t pars[2];
      char *path, *str;
      read_pars( state, pars, 2 );
      path = fvm_getstr( state, pars[0] );
      str = fvm_getstr( state, pars[1] );
      if( path ) freg_put( NULL, 0, path, FREG_TYPE_STRING, (char *)(str ? str : ""), str ? strlen( str ) + 1 : 1, NULL );
    }    
    break;
  case 11:
    /* FregWriteOpaque(path,len,buf)*/
    {
      uint32_t pars[3];
      char *path, *buf;
      read_pars( state, pars, 3 );
      path = fvm_getstr( state, pars[0] );
      buf = fvm_getptr( state, pars[2], pars[1], 0 );
      if( path ) freg_put( NULL, 0, path, FREG_TYPE_OPAQUE, buf, buf ? pars[1] : 0, NULL );
    }        
    break;
  case 12:
    /* FregSubKey(path) */
    {
      uint32_t pars[1];
      char *path;
      read_pars( state, pars, 1 );
      path = fvm_getstr( state, pars[0] );
      if( path ) freg_subkey( NULL, 0, path, FREG_CREATE, NULL );
    }
    break;
  case 13:
    /* FregReadU64(path,var high, var low) */
    {
      uint32_t pars[3];
      char *path;
      int sts;
      uint64_t val;
      
      read_pars( state, pars, 3 );
      path = fvm_getstr( state, pars[0] );

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
  case 14:
    /* FregWriteU64(path,high,low) */
    {
      uint32_t pars[3];
      char *path;
      uint64_t val;
      
      read_pars( state, pars, 3 );
      path = fvm_getstr( state, pars[0] );
      val = ((uint64_t)pars[1] << 32) | pars[2];
      if( path ) freg_put( NULL, 0, path, FREG_TYPE_UINT64, (char *)&val, 8, NULL );
    }
    break;
  case 15:
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
  case 16:
    /* HostRegNameById */
    /* TODO */
    break;
  case 17:
    /* HostregIdByName */
    /* TODO */
    break;
  case 18:
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
  case 19:    
    {
      /* SecRandU32(var r : int); */
      uint32_t raddr, r;

      raddr = fvm_stack_read( state, 4 );
      r = sec_rand_uint32();
      fvm_write_u32( state, raddr, r );
    }
    break;
  case 20:
    {
      /* Sprintf(dest:string,fmt:string,arg1:int,arg2:int,arg3:int,arg4:int) */
      uint32_t pars[6];
      char *str, *fmt;
      char *p, *q, *strp;
      int iarg;

      read_pars( state, pars, 6 );
      str = fvm_getptr( state, pars[0], 0, 0 );
      fmt = fvm_getstr( state, pars[1] );

      if( !str ) break;
      if( !fmt ) break;
      
      p = fmt;
      q = str;
      iarg = 0;
      while( 1 ) {
	if( !*p ) break;
	if( *p == '%' ) {
	  p++;
	  switch( *p ) {
	  case 's':
	    strp = fvm_getstr( state, pars[2 + iarg] );
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
  case 21:
    /* ChtRead(keybuf,reslen,resbuf,var len) */
    {
      uint32_t pars[4];
      char *keybuf, *resbuf;
      struct cht_entry entry;

      read_pars( state, pars, 4 );
      
      resbuf = fvm_getptr( state, pars[2], pars[1], 1 );
      keybuf = fvm_getptr( state, pars[0], CHT_KEY_SIZE, 0 );

      memset( &entry, 0, sizeof(entry) );
      if( keybuf ) {
	cht_read( NULL, keybuf, resbuf, resbuf ? pars[1] : 0, &entry );
      }
      fvm_write_u32( state, pars[3], entry.flags & CHT_SIZE_MASK );
    }      
    break;
  case 22:
    /* ChtWrite(keybuf,reslen,resbuf) */
    {
      uint32_t pars[3];
      char *keybuf, *resbuf;
      struct cht_entry entry;

      read_pars( state, pars, 3 );

      keybuf = fvm_getptr( state, pars[0], CHT_KEY_SIZE, 0 );
      resbuf = fvm_getptr( state, pars[2], pars[1], 1 );
      
      memset( &entry, 0, sizeof(entry) );
      if( keybuf ) memcpy( entry.key, keybuf, CHT_KEY_SIZE );
      if( resbuf ) cht_write( NULL, &entry, resbuf, resbuf ? pars[1] : 0 );
    }
    break;
  case 23:
    /* ChtDelete(keybuf) */
    {
      uint32_t pars[1];
      char *keybuf;
      
      read_pars( state, pars, 1 );
      
      keybuf = fvm_getptr( state, pars[0], CHT_KEY_SIZE, 0 );
      if( keybuf ) cht_delete( NULL, keybuf );
    }
    break;
  case 24:
    /* Puts(str) */
    {
      uint32_t pars[1];
      char *str;
      read_pars( state, pars, 1 );
      str = fvm_getstr( state, pars[0] );
      if( str ) puts( str );	
    }
    break;
  case 25:
    /* FvmRun(modname,procname,arglen,argbuf,reslen,resbuf,var rlen) */
    {
      struct xdr_s args, res;
      int procid;
      struct fvm_module *m;
      uint32_t pars[7];
      char *modname, *procname;
      char *argbuf, *resbuf;
      int sts;
      
      read_pars( state, pars, 7 );
      modname = fvm_getstr( state, pars[0] );
      m = NULL;
      if( modname ) m = fvm_module_by_name( modname );
      procname = fvm_getstr( state, pars[1] );

      procid = -1;
      if( m && procname ) procid = fvm_procid_by_name( m, procname );
      
      argbuf = fvm_getptr( state, pars[3], pars[2], 0 );
      resbuf = fvm_getptr( state, pars[5], pars[4], 1 );
      xdr_init( &args, (uint8_t *)argbuf, argbuf ? pars[2] : 0 );
      xdr_init( &res, (uint8_t *)resbuf, resbuf ? pars[4] : 0 );
      sts = fvm_run( m, procid, &args, &res );
      fvm_write_u32( state, pars[6], sts ? 0 : res.count );
    }
    break;
  case 26:
    /* RaftCommand(idhigh,idlow,len,buf) */
    {
      uint32_t pars[4];
      uint64_t id;
      char *buf;

      read_pars( state, pars, 4 );
      id = (((uint64_t)(pars[0])) << 32) | pars[1];
      buf = fvm_getptr( state, pars[3], 0, 0 );
      raft_cluster_command( id, buf, buf ? pars[2] : 0, NULL );
    }
    break;
  case 27:
    /* FvmClRun(idHigh,idLow,modname,procname,len,buf) */
    {
      uint32_t pars[6];
      uint64_t id;
      char *modname, *procname, *buf;
      read_pars( state, pars, 6 );
      id = (((uint64_t)(pars[0])) << 32) | pars[1];
      modname = fvm_getstr( state, pars[2] );
      procname = fvm_getstr( state, pars[3] );
      buf = fvm_getptr( state, pars[5], 0, 0 );
      if( modname && procname ) {
	fvm_cluster_run( id, modname, procname, buf, buf ? pars[4] : 0 );
      }
    }
  case 0xffff:
    fvm_xcall( state );
    break;
  default:
    return -1;
  }
  return 0;
}
