

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
#include <fju/dmb.h>
#include <fju/dlm.h>
#include <fju/hrauth.h>
#include <fju/hostreg.h>

#include "fvm-private.h"

int cht_entry_by_index( struct cht_s *cht, int idx, uint32_t seq, struct cht_entry *entry );

struct fvmlog {
  int handle;
  int refcount;
  char name[64];  
  struct log_s log;
};
#define FVM_MAX_LOGHANDLE 16 
static struct fvmlog loghandles[FVM_MAX_LOGHANDLE];

static struct fvmlog *fvmlog_get( int handle ) {
  int i;
  
  if( handle == 0 ) return NULL;
  
  for( i = 0; i < FVM_MAX_LOGHANDLE; i++ ) {
    if( loghandles[i].handle == handle ) return &loghandles[i];
  }
  return NULL;
}

static struct fvmlog *fvmlog_open( char *name ) {
  int i;
  for( i = 0; i < FVM_MAX_LOGHANDLE; i++ ) {
    if( strcmp( loghandles[i].name, name ) == 0 ) {
      loghandles[i].refcount++;
      return &loghandles[i];
    }
  }
  for( i = 0; i < FVM_MAX_LOGHANDLE; i++ ) {
    if( loghandles[i].handle == 0 ) {
      char path[256];
      int sts;
  
      sprintf( path, "%s.log", name );
      sts = log_open( mmf_default_path( path, NULL ), NULL, &loghandles[i].log );
      if( sts ) return NULL;

      loghandles[i].refcount = 1;
      loghandles[i].handle = sec_rand_uint32();
      strncpy( loghandles[i].name, name, sizeof(loghandles[i].name) - 1 );
      return &loghandles[i];
    }
  }
  return NULL;
}
static void fvmlog_close( int handle ) {
  int i;
  for( i = 0; i < FVM_MAX_LOGHANDLE; i++ ) {
    if( loghandles[i].handle == handle ) {
      loghandles[i].refcount--;
      if( loghandles[i].refcount == 0 ) {
	log_close( &loghandles[i].log );
	memset( &loghandles[i], 0, sizeof(loghandles[i]) );
	return;
      }
    }
  }
}
	

#define FVM_MAX_FD 256
struct fvm_fd {
  int fd;
  uint32_t refcount;
  char path[256];
  struct mmf_s mmf;
};
static struct fvm_fd filehandles[FVM_MAX_FD];
static struct fvm_fd *fvmfd_open( char *path ) {
  int i, sts;

  for( i = 0; i < FVM_MAX_FD; i++ ) {
    if( filehandles[i].fd && (strcmp( filehandles[i].path, path ) == 0) ) {
      filehandles[i].refcount++;
      fvm_log( LOG_LVL_TRACE, "fvmfd_open returning existing filedecsriptor %u rcount=%u", filehandles[i].fd, filehandles[i].refcount );
      return &filehandles[i];
    }
  }

  for( i = 0; i < FVM_MAX_FD; i++ ) {
    if( filehandles[i].fd == 0 ) {
      sts = mmf_open( mmf_default_path( path, NULL ), &filehandles[i].mmf );
      if( sts ) return NULL;

      do {
	filehandles[i].fd = sec_rand_uint32();
      } while( filehandles[i].fd == 0 );
      
      filehandles[i].refcount = 1;
      strncpy( filehandles[i].path, path, sizeof(filehandles[i].path) - 1 );

      fvm_log( LOG_LVL_TRACE, "fvmfd_open returning new handle %u", filehandles[i].fd );
      return &filehandles[i];
    }
  }
  
  return NULL;
}
static void fvmfd_close( int fd ) {
  int i;

  if( fd == 0 ) return;

  for( i = 0; i < FVM_MAX_FD; i++ ) {
    if( filehandles[i].fd == fd ) {
      filehandles[i].refcount--;
      if( filehandles[i].refcount == 0 ) {
	mmf_close( &filehandles[i].mmf );
	memset( &filehandles[i], 0, sizeof(filehandles[i]) );
	
	fvm_log( LOG_LVL_TRACE, "fvmfd_close closing file handle %u", fd );
	return;
      }
    }
  }

  fvm_log( LOG_LVL_WARN, "fvmfd_close unknown file descriptor" );
}

static struct fvm_fd *fvmfd_get( int fd ) {
  int i;
  for( i = 0; i < FVM_MAX_FD; i++ ) {
    if( filehandles[i].fd == fd ) return &filehandles[i];
  }
  return NULL;
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

  fvm_log( LOG_LVL_TRACE, "fvm_xcall %s/%s args=%u", m->name, m->procs[procid].name, args.offset );
  sts = fvm_run( m, procid, &args, &res );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "fvm_xcall failed to run %s/%s", m->name, m->procs[procid].name );
    goto done;
  }

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
	  if( fvm_write_u32( state, pars[FVM_MAX_PARAM - nargs + i], u32 ) < 0 ) {
	    sts = -1;
	    goto done;
	  }
	}
	break;
      case VAR_TYPE_STRING:
	strp = fvm_getptr( state, FVM_ADDR_STACK + sp, 0, 1 );
	xdr_decode_string( &res, strp, FVM_MAX_STACK - sp );
	if( fvm_write_u32( state, pars[FVM_MAX_PARAM - nargs + i], FVM_ADDR_STACK + sp ) < 0 ) {
	  sts = -1;
	  goto done;
	}
	  
	u32 = strlen( strp ) + 1;
	if( u32 % 4 ) u32 += 4 - (u32 % 4);
	sp += u32;
	break;
      case VAR_TYPE_OPAQUE:
	bufp = fvm_getptr( state, FVM_ADDR_STACK + sp, 0, 1 );
	u32 = FVM_MAX_STACK - sp;
	xdr_decode_opaque( &res, (uint8_t *)bufp, (int *)&u32 );
	if( fvm_write_u32( state, pars[FVM_MAX_PARAM - nargs + i - 1], u32 ) ) {
	  sts = -1;
	  goto done;
	}
	    
	if( fvm_write_u32( state, pars[FVM_MAX_PARAM - nargs + i], FVM_ADDR_STACK + sp ) < 0 ) {
	  sts = -1;
	  goto done;
	}
	
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

struct fvm_sc_iterator {
  struct rpc_iterator iter;
  uint32_t prochandle;
  uint32_t private;
};

static void fvm_syscall_iter_cb( struct rpc_iterator *iter ) {
  struct fvm_sc_iterator *sciter;
  struct fvm_module *m;
  int procid, sts;
  struct xdr_s args;
  uint8_t argbuf[4];
  
  sciter = (struct fvm_sc_iterator *)iter;

  sts = fvm_proc_by_handle( sciter->prochandle, &m, &procid );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "fvm_syscall_iter_cb bad proc handle" );
    goto done;
  }
  
  xdr_init( &args, argbuf, sizeof(argbuf) );
  xdr_encode_uint32( &args, sciter->private );
  args.offset = 0;

  fvm_run( m, procid, &args, NULL );

 done:
  rpc_iterator_unregister( iter );
  free( iter );  
}
  
  
  
static void fvm_rpccall_donecb( struct xdr_s *res, struct hrauth_call *hcallp ) {
  uint32_t resultprocid;
  int sts;
  struct fvm_module *m;
  struct xdr_s xdr;
  
  fvm_log( LOG_LVL_INFO, "fvm_rpccall_donecb %s", res ? "success" : "failure" );
  if( !res ) return;

  m = fvm_module_by_tag( hcallp->cxt[0] );
  if( !m ) {
    fvm_log( LOG_LVL_ERROR, "fvm_rpccall_donecb unknown module" );
    return;
  }
  
  resultprocid = (uint32_t)hcallp->cxt[1];
  
  if( (res->count + 8) >= res->buf_size ) {
    fvm_log( LOG_LVL_ERROR, "fvm_rpccall_donecb out of buffer space" );
    return;
  }

  memmove( res->buf + res->offset + 4, res->buf + res->offset, res->count - res->offset );
  
  xdr_init( &xdr, res->buf + res->offset, 4 );
  xdr_encode_uint32( &xdr, res->count - res->offset );
  res->count += 4;
  xdr_init( &xdr, res->buf + res->count, 4 );
  xdr_encode_uint32( &xdr, hcallp->cxt[2] );
  res->count += 4;
  
  /* Signature is always proc(len : int, buf : opaque, private : int) */
  sts = fvm_run( m, resultprocid, res, NULL );
  if( sts ) {
    fvm_log( LOG_LVL_ERROR, "fvm_rpccall_donecb failed to run callback" );
  }
}


struct fvm_raft_app {
  struct raft_app app;
  uint32_t command;
  uint32_t snapsave;
  uint32_t snapload;
};
static void fvm_raft_command_cb( struct raft_app *rapp, struct raft_cluster *cl, uint64_t cmdseq, char *buf, int len ) {
  struct fvm_raft_app *app = (struct fvm_raft_app *)rapp;
  int sts, procid;
  struct fvm_module *m;
  struct xdr_s args;
  char *argbuf;
  
  /* get proc */
  sts = fvm_proc_by_handle( app->command, &m, &procid );
  if( sts ) return;
  
  /* encode args */
  argbuf = malloc( len + 32 );
  xdr_init( &args, (uint8_t *)argbuf, len + 32 );
  xdr_encode_uint64( &args, cl->clid );
  xdr_encode_uint64( &args, cmdseq );
  xdr_encode_opaque( &args, (uint8_t *)buf, len );
  
  /* run */
  sts = fvm_run( m, procid, &args, NULL );

  free( argbuf );
}

static void fvm_raft_snapsave_cb( struct raft_app *rapp, struct raft_cluster *cl, uint64_t term, uint64_t seq ) {
  struct fvm_raft_app *app = (struct fvm_raft_app *)rapp;
  int sts, procid;
  struct fvm_module *m;
  struct xdr_s args;
  char argbuf[32];
  
  /* get proc */
  sts = fvm_proc_by_handle( app->snapsave, &m, &procid );
  if( sts ) return;
  
  /* encode args */
  xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
  xdr_encode_uint64( &args, cl->clid );
  xdr_encode_uint64( &args, term );  
  xdr_encode_uint64( &args, seq );

  /* run */
  sts = fvm_run( m, procid, &args, NULL );
}
static void fvm_raft_snapload_cb( struct raft_app *rapp, struct raft_cluster *cl, char *buf, int len ) {
  struct fvm_raft_app *app = (struct fvm_raft_app *)rapp;
  int sts, procid;
  struct fvm_module *m;
  struct xdr_s args;
  char *argbuf;
  
  /* get proc */
  sts = fvm_proc_by_handle( app->snapload, &m, &procid );
  if( sts ) return;
  
  /* encode args */
  argbuf = malloc( len + 32 );
  xdr_init( &args, (uint8_t *)argbuf, len + 32 );
  xdr_encode_uint64( &args, cl->clid );
  xdr_encode_opaque( &args, (uint8_t *)buf, len );
  
  /* run */
  sts = fvm_run( m, procid, &args, NULL );

  free( argbuf );  
}

int fvm_syscall( struct fvm_state *state, uint16_t syscallid ) {
  switch( syscallid ) {
  case 1:
    /* LogWrite(handle,flags,len,buf) */
    {
      char *buf;
      struct log_entry entry;
      struct log_iov iov[1];
      uint32_t pars[4];
      struct fvmlog *logp;
      
      read_pars( state, pars, 4 );
      logp = fvmlog_get( pars[0] );
      buf = fvm_getptr( state, pars[3], pars[2], 0 );

      memset( &entry, 0, sizeof(entry) );
      iov[0].buf = buf;
      iov[0].len = buf ? pars[2] : 0;
      entry.iov = iov;
      entry.niov = 1;
      entry.flags = pars[1];
      strncpy( (char *)&entry.ltag, "FVMS", 4 );
      log_write( logp ? &logp->log : NULL, &entry );
    }
    break;
  case 2:
    /* LogNext(handle,prevHigh,prevLow,var high,var low); */
    {
      struct fvmlog *logp;
      struct log_entry entry;
      uint64_t id;
      int ne, sts;
      uint32_t pars[5];

      read_pars( state, pars, 5 );
      logp = fvmlog_get( pars[0] );
      id = (((uint64_t)pars[1]) << 32) | (uint64_t)pars[2];

      memset( &entry, 0, sizeof(entry) );
      sts = log_read( logp ? &logp->log : NULL, id, &entry, 1, &ne );
      if( sts || !ne ) {
	fvm_write_u32( state, pars[4], 0 );
	fvm_write_u32( state, pars[3], 0 );
      } else {
	fvm_write_u32( state, pars[4], entry.id & 0xffffffff );
	fvm_write_u32( state, pars[3], (entry.id >> 32) & 0xffffffff );
      }

    }
    break;
  case 3:
    /* LogRead(handle,idhigh,idlow,len,buf, var flags, var lenp) */
    {
      struct fvmlog *logp;
      uint64_t id;
      int sts;
      char *bufp;
      uint32_t pars[7];
      struct log_entry entry;
      struct log_iov iov[1];
      
      read_pars( state, pars, 7 );
      logp = fvmlog_get( pars[0] );
      id = (((uint64_t)pars[1]) << 32) | (uint64_t)pars[2];

      bufp = fvm_getptr( state, pars[4], pars[3], 0 );

      memset( &entry, 0, sizeof(entry) );
      iov[0].buf = bufp;
      iov[0].len = bufp && (pars[3] > 0) ? pars[3] : 0;
      entry.iov = iov;
      entry.niov = 1;	
      sts = log_read_entry( logp ? &logp->log : NULL, id, &entry );
      fvm_write_u32( state, pars[5], sts ? 0 : entry.flags );
      fvm_write_u32( state, pars[6], sts ? 0 : entry.msglen );
    }
    break;
  case 4:
    /* LogLastId(logname,var idHigh,var idLow) */
    {
      struct fvmlog *logp;
      uint32_t pars[3];
      struct log_prop prop;

      read_pars( state, pars, 3 );
      logp = fvmlog_get( pars[0] );

      log_prop( logp ? &logp->log : NULL, &prop );
      fvm_write_u32( state, pars[2], prop.last_id & 0xffffffff );
      fvm_write_u32( state, pars[1], prop.last_id >> 32 );
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

      /* write default failure value to result so we can exit early on failure */
      fvm_write_u32( state, pars[4], 0 ); 
      if( !path ) break;
      
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
	if( str ) strcpy( str, "" );
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
	freg_get_by_name( NULL, 0, path, FREG_TYPE_OPAQUE, res, res ? pars[1] : 0, &lenp );
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
    /* HostregNameById(idHigh : u32, idLow : u32, name : string) */
    {
      uint32_t pars[3];
      char *name;
      uint64_t hostid;
      
      read_pars( state, pars, 3 );
      hostid = (((uint64_t)pars[0]) << 32) | (uint64_t)pars[1];
      name = fvm_getptr( state, pars[2], HOSTREG_MAX_NAME, 1 );
      hostreg_name_by_hostid( hostid, name );
    }
    break;
  case 17:
    /* Declare Syscall HostregIdByName(name : string, var idHigh : u32, idLow : u32 ) */
    {
      uint32_t pars[3];
      char *name;
      uint64_t hostid;
      
      read_pars( state, pars, 3 );
      name = fvm_getstr( state, pars[0] );
      if( name ) hostid = hostreg_hostid_by_name( name );
      else hostid = 0;
      fvm_write_u32( state, pars[1], hostid >> 32 );
      fvm_write_u32( state, pars[2], hostid & 0xffffffff );
    }
    break;
  case 18:
    {
      /* RpcNow(var high:int,var low :int ) */
      uint32_t pars[2];
      uint64_t now;

      read_pars( state, pars, 2 );
      
      now = rpc_now();
      fvm_write_u32( state, pars[1], now & 0xffffffff );
      fvm_write_u32( state, pars[0], now >> 32 );
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
      char *p, *q;
      int iarg;
      char tmpfmt[64], *tmpfmtp;
      
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
	  memset( tmpfmt, 0, sizeof(tmpfmt) );
	  tmpfmtp = p;
	  p++;
	  while( *p ) {
	    if( *p == 's' ) {
	      memcpy( tmpfmt, tmpfmtp, p - tmpfmtp + 1 );
	      sprintf( q, tmpfmt, fvm_getstr( state, pars[2 + iarg] ) );
	      q += strlen( q );	      
	      iarg++;
	      p++;
	      break;
	    }
	    if( *p == 'd' || *p == 'u' || *p == 'x' || *p == 'o' ) {
	      memcpy( tmpfmt, tmpfmtp, p - tmpfmtp + 1 );
	      sprintf( q, tmpfmt, pars[2 + iarg] );
	      q += strlen( q );
	      iarg++;
	      p++;
	      break;
	    }

	    if( *p == '%' ) {
	      *q = '%';
	      q++;
	      break;
	    }

	    p++;
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

      /* write rlen=0 as default value so we can break early */
      fvm_write_u32( state, pars[6], 0 );
      
      modname = fvm_getstr( state, pars[0] );
      if( !modname ) break;

      m = fvm_module_by_name( modname );
      if( !m ) break;
      
      procname = fvm_getstr( state, pars[1] );
      if( !procname ) break;
      
      procid = fvm_procid_by_name( m, procname );
      if( procid < 0 ) break;
      
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
      raft_command( id, buf, buf ? pars[2] : 0, NULL );
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
	fvm_log( LOG_LVL_TRACE, "FvmClRun %s/%s len=%u %s", modname, procname, pars[4], buf ? "" : "NoBuf" );
	fvm_cluster_run( id, modname, procname, buf, buf ? pars[4] : 0 );
      }
    }
  case 28:
    /* FvmClRunOthers(idHigh,idLow,modname,procname,len,buf) */
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
	fvm_log( LOG_LVL_TRACE, "FvmClRunOthers %s/%s len=%u %s", modname, procname, pars[4], buf ? "" : "NoBuf" );

	/* schedule to run on all nodes in cluster except local node */
	fvm_cluster_run2( id, modname, procname, buf, buf ? pars[4] : 0, 0, hostreg_localid() );
      }
    }
    break;
  case 29:
    /* LogReadInfo(fd : int, idh : int, idl : int, info : opaque) */
    {
      struct fvmlog *logp;
      uint64_t id;
      int sts;
      struct log_entry entry;
      uint32_t pars[4];
      uint32_t addr;
      
      read_pars( state, pars, 2 );
      logp = fvmlog_get( pars[0] );      
      id = (((uint64_t)pars[1]) << 32) | (uint64_t)pars[2];
      addr = pars[3];
      
      memset( &entry, 0, sizeof(entry) );
      entry.niov = 0;
      sts = log_read_entry( logp ? &logp->log : NULL, id, &entry );
      if( !sts ) {
	fvm_write_u32( state, addr, entry.seq >> 32 );
	addr += 4;
	fvm_write_u32( state, addr, entry.seq & 0xffffffff );
	addr += 4;
	fvm_write_u32( state, addr, entry.timestamp >> 32 );
	addr += 4;
	fvm_write_u32( state, addr, entry.timestamp & 0xffffffff );
	addr += 4;
	fvm_write_u32( state, addr, entry.msglen );
	addr += 4;
	fvm_write_u32( state, addr, entry.flags );
	addr += 4;	
      }
            
    }
    break;
  case 30:
    /* LogPrev(logname,idhigh,idlow,var high, var low) */
    {
      struct fvmlog *logp;
      struct log_entry entry;
      struct log_prop prop;
      uint64_t id;
      uint32_t pars[5];

      read_pars( state, pars, 5 );
      logp = fvmlog_get( pars[0] );
      id = (((uint64_t)pars[1]) << 32) | (uint64_t)pars[2];

      if( id == 0 ) {
	/* get last id written */
	log_prop( logp ? &logp->log : NULL, &prop );
	id = prop.last_id;
      } else {
	/* get prev id of this entry */
	memset( &entry, 0, sizeof(entry) );
	log_read_entry( logp ? &logp->log : NULL, id, &entry );
	id = entry.prev_id;
      }
      fvm_write_u32( state, pars[4], id & 0xffffffff );
      fvm_write_u32( state, pars[3], (id >> 32) & 0xffffffff );
    }
    break;
  case 31:
    /* ChtList(startkey,keybuf,nkeybuf,var nkeys) */
    {
      uint32_t pars[5];
      int i, nk, reading;
      char startkey[CHT_KEY_SIZE];
      char *p, *keybuf;
      int nkeys, firstkey, sts;
      struct cht_entry entry;
      struct cht_prop prop;

      read_pars( state, pars, 4 );
      
      memset( startkey, 0, sizeof(startkey) );
      firstkey = 1;

      nkeys = pars[2];
      keybuf = fvm_getptr( state, pars[1], CHT_KEY_SIZE * nkeys, 1 );
      if( !keybuf ) nkeys = 0;
      
      p = fvm_getptr( state, pars[0], CHT_KEY_SIZE, 0 );
      if( p ) {
	memcpy( startkey, p, CHT_KEY_SIZE );
	if( (*(uint64_t *)(startkey)) || (*(uint64_t *)(startkey + 8)) ) {
	  firstkey = 0;
	}
      }

      reading = firstkey;
      nk = 0;
      cht_prop( NULL, &prop );
      for( i = 0; i < prop.count; i++ ) {
	sts = cht_entry_by_index( NULL, i, 0, &entry );
	if( sts == 0 ) {
	  if( !reading && (memcmp( entry.key, startkey, CHT_KEY_SIZE ) == 0) ) {
	    reading = 1;
	  }
	  
	  if( reading ) {
	    memcpy( keybuf, entry.key, CHT_KEY_SIZE );
	    keybuf += CHT_KEY_SIZE;
	    nkeys--;
	    nk++;
	    if( nkeys == 0 ) break;
	  }
	}
      }

      fvm_write_u32( state, pars[3], nk );
    }
    break;
  case 32:
    /* DmbPublish(msgid : int, flags : int, len : int, buf : opaque) */
    {
      uint32_t pars[6];
      char *bufp;
      int len;
      uint64_t seq;
      
      read_pars( state, pars, 6 );
      len = pars[2];
      if( pars[3] > 0 ) {
	bufp = fvm_getptr( state, pars[3], len, 0 );
	if( !bufp ) len = 0;
      } else {
	bufp = NULL;
	len = 0;
      }

      dmb_publish( pars[0], pars[1], bufp, len, &seq );
      fvm_write_u32( state, pars[4], seq >> 32 );
      fvm_write_u32( state, pars[5], seq & 0xffffffff );
    }
    break;
  case 33:
    /* DmbSubscribe(procaddr : int, msgid : int, flags : int) */
    {
      uint32_t pars[3];
      char *procname;
      int procid;
      
      read_pars( state, pars, 3 );
      procid = fvm_procid_by_addr( state->module, pars[0] );      
      if( procid >= 0 ) {
	procname = state->module->procs[procid].name;

	if( (pars[2] & DMB_FVMSC_APPLY) == 0 ) {
	  /* raw mode: check procedure parameters match int,int,opaque */
	  uint64_t siginfo = 0;
	  FVM_SIGINFO_SETPARAM(siginfo,2,VAR_TYPE_OPAQUE,0);
	  FVM_SIGINFO_SETNPARS(siginfo,3);
	  if( state->module->procs[procid].siginfo != siginfo ) {
	    fvm_log( LOG_LVL_WARN, "DmbSubscribe %s/%s bad signature", state->module->name, procname );
	  }
	}
	
	dmb_subscribe_fvm( state->module->name, procname, pars[1], pars[2] );
      }
    }    
    break;
  case 34:
    /* DmbUnsubscribe(procaddr : int) */
    {
      uint32_t pars[1];
      int procid;
      char *procname;
      
      read_pars( state, pars, 1 );
      procid = fvm_procid_by_addr( state->module, pars[0] );
      if( procid >= 0 ) {
	procname = state->module->procs[procid].name;
	dmb_unsubscribe_fvm( state->module->name, procname );
      }
    }
    break;
  case 35:
    /* DmbHostInfo(hostH : int, hostL : int, var seqH : int, var seqL : int) */
    {
      uint32_t pars[4];
      uint64_t hostid, seq;
      
      read_pars( state, pars, 4 );
      hostid = (((uint64_t)pars[0]) << 32) | (uint64_t)pars[1];

      dmb_host_info( hostid, NULL, &seq );

      fvm_write_u32( state, pars[2], (uint32_t)((seq >> 32) & 0xffffffff) );
      fvm_write_u32( state, pars[3], (uint32_t)(seq & 0xffffffff) );
    }
    break;
  case 36:
    /* DmbMsgInfo(var hostH : int, var hostL : Int, var seqH : int, var seqL : int) */
    {
      uint32_t pars[4];
      uint64_t hostid, seq;

      read_pars( state, pars, 4 );
      dmb_msginfo( &hostid, &seq, NULL, NULL, NULL );

      fvm_write_u32( state, pars[0], hostid >> 32 );
      fvm_write_u32( state, pars[1], hostid & 0xffffffff );
      fvm_write_u32( state, pars[2], seq >> 32 );
      fvm_write_u32( state, pars[3], seq & 0xffffffff );
    }
    break;
  case 37:
    /* RpcCall(info,len,buf,resultproc,private) */
    {
      uint32_t pars[5];
      uint64_t hostid;
      uint32_t len, progid,versid,procid, bufaddr, addr, timeout;
      char *bufp;
      struct hrauth_call hcall;
      struct xdr_s args;
      int mprocid;
      
      read_pars( state, pars, 5 );
      addr = pars[0];
      hostid = ((uint64_t)fvm_read_u32( state, addr ) << 32) | fvm_read_u32( state, addr + 4);
      addr += 8;
      progid = fvm_read_u32( state, addr );
      addr += 4;
      versid = fvm_read_u32( state, addr );
      addr += 4;
      procid = fvm_read_u32( state, addr );
      addr += 4;
      timeout = fvm_read_u32( state, addr );
      addr += 4;
      
      len = pars[1];
      bufaddr = pars[2];
      bufp = len ? fvm_getptr( state, bufaddr, len, 0 ) : NULL;

      memset( &hcall, 0, sizeof(hcall) );
      hcall.hostid = hostid;
      hcall.prog = progid;
      hcall.vers = versid;
      hcall.proc = procid;
      hcall.timeout = timeout;
      if( pars[3] ) {
	hcall.donecb = fvm_rpccall_donecb;
	hcall.cxt[0] = state->module->tag;
	mprocid = fvm_procid_by_addr( state->module, pars[3] );
	if( mprocid < 0 ) {
	  fvm_log( LOG_LVL_ERROR, "Bad procid %x for RpcCall", pars[3] );
	} else {
	  uint64_t siginfo = 0;
	  FVM_SIGINFO_SETPARAM(siginfo,0,VAR_TYPE_U32,0);
	  FVM_SIGINFO_SETPARAM(siginfo,1,VAR_TYPE_OPAQUE,0);
	  FVM_SIGINFO_SETPARAM(siginfo,2,VAR_TYPE_U32,0);
	  FVM_SIGINFO_SETNPARS(siginfo,3);
	  if( state->module->procs[mprocid].siginfo != siginfo ) {
	    fvm_log( LOG_LVL_ERROR, "Bad proc signature for RpcCall" );
	  }
	}
	hcall.cxt[1] = mprocid;
	hcall.cxt[2] = pars[4];
      }
      
      memset( &args, 0, sizeof(args) );
      xdr_init( &args, (uint8_t *)bufp, len );
      args.offset = len;
      
      hrauth_call_async( &hcall, &args, 1 );
    }
    break;
  case 38:
    /* Sleep(milliseconds : int, cb : int, private : int) */
    {
      uint32_t pars[3];
      struct fvm_sc_iterator *iter;
      uint64_t siginfo;
      int procid, sts;
      
      read_pars( state, pars, 3 );

      iter = malloc( sizeof(*iter) );
      memset( iter, 0, sizeof(*iter) );
      iter->iter.timeout = rpc_now() + pars[0];
      iter->iter.period = pars[0];
      iter->iter.cb = fvm_syscall_iter_cb;
      procid = fvm_procid_by_addr( state->module, pars[1] );
      if( procid < 0 ) {
	fvm_log( LOG_LVL_ERROR, "Bad procaddr %x", pars[1] );
	return -1;
      }

      sts = fvm_handle_by_procid( state->module->name, procid, &iter->prochandle );
      if( sts ) {
	fvm_log( LOG_LVL_ERROR, "Failed to get proc handle" );
	return -1;
      }
      
      siginfo = 0;
      FVM_SIGINFO_SETPARAM(siginfo,0,VAR_TYPE_U32,0);
      FVM_SIGINFO_SETNPARS(siginfo,1);
      if( state->module->procs[procid].siginfo != siginfo ) {
	fvm_log( LOG_LVL_WARN, "Bad sleep signature" );
      }
      
      iter->private = pars[2];
      
      rpc_iterator_register( (struct rpc_iterator *)iter );
    }
    break;
  case 39:
    /* sha1(len,buf,hash) */
    {
      uint32_t pars[3];
      char *hash;
      struct sec_buf iov[16];
      int i, niov, gotbuf, iovaddr, addr;

      memset( iov, 0, sizeof(iov) );
      
      read_pars( state, pars, 3 );
      niov = pars[0];
      if( niov > 16 ) niov = 16;

      gotbuf = 1;
      iovaddr = pars[1];
      for( i = 0; i < niov; i++ ) {
	iov[i].len = fvm_read_u32( state, iovaddr );
	iovaddr += 4;
	addr = fvm_read_u32( state, iovaddr );
	iov[i].buf = fvm_getptr( state, addr, iov[i].len, 0 );
	iovaddr += 4;
	if( (iov[i].buf == NULL) && iov[i].len ) {
	  gotbuf = 0;
	  break;
	}
      }
      
      hash = fvm_getptr( state, pars[2], SEC_SHA1_MAX_HASH, 1 );

      if( gotbuf && hash ) {
	sha1( (uint8_t *)hash, iov, niov );
      }
    }
    break;
  case 40:
    /* aesencrypt( len, buf, key ) */
    {
      uint32_t pars[3];
      char *buf, *key;

      read_pars( state, pars, 3 );
      
      buf = fvm_getptr( state, pars[1], pars[0], 1 );
      key = fvm_getptr( state, pars[2], SEC_AES_MAX_KEY, 0 );

      if( buf && key ) {
	aes_encrypt( (uint8_t *)key, (uint8_t *)buf, pars[0] );
      }
    }
    break;
  case 41:
    /* aesdecrypt( len, buf, key ) */
    {
      uint32_t pars[3];
      char *buf, *key;

      read_pars( state, pars, 3 );

      buf = fvm_getptr( state, pars[1], pars[0], 1 );
      key = fvm_getptr( state, pars[2], SEC_AES_MAX_KEY, 0 );

      if( buf && key ) {
	aes_decrypt( (uint8_t *)key, (uint8_t *)buf, pars[0] );
      }
      
    }
    break;
  case 42:
    /* open(path,var fd) */
    {
      uint32_t pars[2];
      struct fvm_fd *fvmfd;
      char *path;
      
      read_pars( state, pars, 2 );
      path = fvm_getstr( state, pars[0] );
      if( path ) {
	fvmfd = fvmfd_open( path );
	fvm_write_u32( state, pars[1], fvmfd ? fvmfd->fd : 0 );
      } else {
	fvm_write_u32( state, pars[1], 0 );
      }
    }
    break;
  case 43:
    /* close(fd) */
    {
      uint32_t pars[1];

      read_pars( state, pars, 1 );
      fvmfd_close( pars[0] );
    }
    break;
  case 44:
    /* read(fd,len,buf,offset) */
    {
      uint32_t pars[4];
      char *buf;
      struct fvm_fd *fvmfd;
      
      read_pars( state, pars, 4 );

      fvmfd = fvmfd_get( pars[0] );
      if( fvmfd ) {
	buf = fvm_getptr( state, pars[2], pars[1], 1 );
	if( buf ) mmf_read( &fvmfd->mmf, buf, pars[1], pars[3] );
	else fvm_log( LOG_LVL_ERROR, "syscall read bad args len=%u addr=0x%x", pars[1], pars[2] );
      } else {
	fvm_log( LOG_LVL_ERROR, "syscall read bad file descriptor" );
      }
    }
    break;
  case 45:
    /* write(fd,len,buf,offset) */
    {
      uint32_t pars[4];
      char *buf;
      struct fvm_fd *fvmfd;
      
      read_pars( state, pars, 4 );

      fvmfd = fvmfd_get( pars[0] );
      if( fvmfd ) {
	buf = fvm_getptr( state, pars[2], pars[1], 0 );
	if( buf ) mmf_write( &fvmfd->mmf, buf, pars[1], pars[3] );
	else fvm_log( LOG_LVL_ERROR, "syscall write bad args len=%u addr=0x%x", pars[1], pars[2] );
      } else {
	fvm_log( LOG_LVL_ERROR, "syscall write bad file descriptor" );
      }
    }
    break;
  case 46:
    /* filesize(fd, var len) */
    {
      uint32_t pars[2];
      struct fvm_fd *fvmfd;
      
      read_pars( state, pars, 2 );

      fvmfd = fvmfd_get( pars[0] );
      fvm_write_u32( state, pars[1], fvmfd ? mmf_fsize( &fvmfd->mmf ) : 0 );
    }
    break;
  case 47:
    /* LogOpen(name,var handle) */
    {
      uint32_t pars[2];
      char *name;
      struct fvmlog *logp;
      
      read_pars( state, pars, 2 );
      name = fvm_getstr( state, pars[0] );
      logp = fvmlog_open( name );
      fvm_write_u32( state, pars[1], logp ? logp->handle : 0 );
    }
    break;
  case 48:
    /* LogClose(handle) */
    {
      uint32_t pars[1];
      read_pars( state, pars, 1 );
      fvmlog_close( pars[0] );
    }
    break;
  case 49:
    /* Timestr(high,low,str) */
    {
      uint32_t pars[3];
      char *str;
      read_pars( state, pars, 3 );
      str = fvm_getptr( state, pars[2], 64, 1 );
      if( str ) sec_timestr( (((uint64_t)pars[0]) << 32) | (uint64_t)pars[1], str );
    }
    break;
  case 50:
    /* TimeNow(high,low) */
    {
      uint32_t pars[2];
      uint64_t now;
      
      read_pars( state, pars, 2 );
      now = time( NULL );
      fvm_write_u32( state, pars[0], now >> 32 );
      fvm_write_u32( state, pars[1], now & 0xffffffff );
    }
    break;
  case 51:
    /* RaftAppRegister(appid,&command,&snapsave,&snapload) */
    {
      uint32_t pars[4];
      struct fvm_raft_app *app;
      int sts;
      
      read_pars( state, pars, 4 );

      app = malloc( sizeof(*app) );
      memset( app, 0, sizeof(*app) );
      app->app.appid = pars[0];
      app->app.command = fvm_raft_command_cb;
      sts = fvm_handle_by_procid( state->module->name, fvm_procid_by_addr( state->module, pars[1] ), &app->command );
      if( pars[2] ) {
	app->app.snapsave = fvm_raft_snapsave_cb;
	sts = fvm_handle_by_procid( state->module->name, fvm_procid_by_addr( state->module, pars[2] ), &app->snapsave );
      }
      if( pars[3] ) {
	app->app.snapload = fvm_raft_snapload_cb;
	sts = fvm_handle_by_procid( state->module->name, fvm_procid_by_addr( state->module, pars[3] ), &app->snapload );
      }
      
      raft_app_register( &app->app );
    }
    break;
  case 52:
    /* RaftAppUnregister(appid) */
    {
      uint32_t pars[1];
      struct raft_app *app;
      
      read_pars( state, pars, 1 );
      app = raft_app_by_appid( pars[0] );
      if( app ) {
	raft_app_unregister( app );
	free( app );
      }
    }
    break;
  case 0xffff:
    fvm_xcall( state );
    break;
  default:
    return -1;
  }
  return 0;
}
