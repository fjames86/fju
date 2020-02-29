 /*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
*/

#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/fvm.h>
#include <fju/sec.h>
#include <fju/log.h>
#include <fju/nls.h>
#include <fju/freg.h>

#include "fvm-private.h"

/* maximum number of concurrently running programs */
#define FVM_MAX_PROGRAM 8

struct loaded_fvm {
  struct loaded_fvm *next;
  uint32_t id;
  uint32_t flags;
  struct fvm_state fvm;
  uint64_t inlogid;
  struct log_s inlog;
  uint64_t outlogid;
  struct log_s outlog;
  uint64_t runtime;
  char name[64];
  void (*donecb)( struct loaded_fvm *fvm );  /* completion callback */
  void *donecxt;
};

struct event_prog {
  uint32_t category;
  uint32_t eventid;
  char name[64];  
};

static struct {
  struct loaded_fvm *progs;  /* active programs */
  struct loaded_fvm *flist;  /* freelist of program descriptors */
  struct rpcd_subscriber subsc;
  int nevtprogs;
#define FVM_MAX_EVTPROGS 64
  struct event_prog evtprogs[FVM_MAX_EVTPROGS];
  uint32_t idseq;
} glob;

static void fvm_iter_cb( struct rpc_iterator *iter );
static void reload_evtprogs( struct rpc_iterator *iter );
static struct loaded_fvm *load_prog_by_name( char *name );
static struct loaded_fvm *lf_by_id( uint32_t id );

static struct rpc_iterator fvm_iter = {
    NULL,
    0,
    1000,
    fvm_iter_cb,
    NULL
};


static int fvm_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct loaded_fvm *fvm_load_prog( uint8_t *bufp, int buflen, uint32_t flags, uint64_t inid, uint64_t outid, char *name ) {
    int sts;
    struct loaded_fvm *lf;
    struct nls_share nls;
    
    lf = glob.flist;
    if( !lf ) {
      /* freelist exhausted - allocate new instance? */
      lf = malloc( sizeof(*lf) );
    } else {
      glob.flist = glob.flist->next;
    }

    memset( lf, 0, sizeof(*lf) );
    lf->id = ++glob.idseq;
    strncpy( lf->name, name, sizeof(lf->name) - 1 );
    sts = fvm_load( &lf->fvm, (char *)bufp, buflen );
    if( sts ) {
        lf->next = glob.flist;
	glob.flist = lf;
	return NULL;
    }

    
    if( inid ) {
	sts = nls_share_by_hshare( inid, &nls );
	if( !sts ) sts = nls_share_open( &nls, &lf->inlog );
	if( sts ) {
	  inid = 0;
	  lf->fvm.inlogid = 0;
	  lf->fvm.inlog = NULL;
	} else {
	    lf->fvm.inlog = &lf->inlog;
	    lf->inlogid = inid;
	}
    }
    
    if( outid ) {
	sts = nls_share_by_hshare( outid, &nls );
	if( !sts ) sts = nls_share_open( &nls, &lf->outlog );
	if( sts ) {
	  outid = 0;
	  lf->outlogid = 0;
	  lf->fvm.outlog = NULL;
	} else {
	    lf->fvm.outlog = &lf->outlog;
	    lf->outlogid = outid;
	}
    } else {
      /* use default log file otherwise */
      sts = log_open( NULL, NULL, &lf->outlog );
      if( !sts ) {
	lf->fvm.outlog = &lf->outlog;
	lf->outlogid = -1;
      }
    }
    
    lf->flags = flags;
    lf->fvm.id = lf->id;
    
    /* push onto list */
    lf->next = glob.progs;
    glob.progs = lf;
    
    /* ensure the iterator runs immediately */
    fvm_iter.timeout = 0;
    if( flags & FVM_RPC_START ) lf->fvm.flags |= FVM_FLAG_RUNNING;
    else lf->fvm.flags &= ~FVM_FLAG_RUNNING;

    return lf;
}

static int fvm_proc_load( struct rpc_inc *inc ) {
  int handle, buflen, sts;
  char *bufp;
  struct loaded_fvm *lf;
  uint32_t flags;
  uint64_t inid, outid;
  char name[64];
	    
  sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &buflen );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &flags );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &inid );
  if( !sts ) sts = xdr_decode_uint64( &inc->xdr, &outid );
  if( !sts ) sts = xdr_decode_string( &inc->xdr, name, sizeof(name) );
  if( sts ) {
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  }

  lf = load_prog_by_name( name );
  if( !lf ) {
    lf = fvm_load_prog( (uint8_t *)bufp, buflen, flags, inid, outid, name );
    if( lf && (buflen == 0) ) lf->fvm.flags &= ~FVM_FLAG_RUNNING;
  }

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_uint32( &inc->xdr, lf ? lf->id : 0);
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_unload( struct rpc_inc *inc ) {
  int handle, sts;
  uint32_t id;
  struct loaded_fvm *lf, *prev;
  
  sts = xdr_decode_uint32( &inc->xdr, &id );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  lf = glob.progs;
  prev = NULL;
  while( lf ) {
    if( lf->id == id ) {
      if( prev ) prev = lf->next;
      else glob.progs = lf->next;

      if( lf->inlogid ) log_close( &lf->inlog );
      if( lf->outlogid ) log_close( &lf->outlog );

      /* push onto freelist */
      lf->next = glob.flist;
      glob.flist = lf;
      
      break;
    }
    prev = lf;
    lf = lf->next;    
  }

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}


static int fvm_proc_list( struct rpc_inc *inc ) {
  int handle;
  struct loaded_fvm *lf;

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  
  lf = glob.progs;
  while( lf ) {
    xdr_encode_boolean( &inc->xdr, 1 );
    xdr_encode_uint32( &inc->xdr, lf->id );
    xdr_encode_uint32( &inc->xdr, lf->fvm.flags );
    xdr_encode_uint64( &inc->xdr, lf->fvm.tickcount );
    xdr_encode_uint64( &inc->xdr, lf->runtime );
    xdr_encode_uint64( &inc->xdr, lf->inlogid );
    xdr_encode_uint64( &inc->xdr, lf->outlogid );
    xdr_encode_string( &inc->xdr, lf->name );
    lf = lf->next;    
  }
  xdr_encode_boolean( &inc->xdr, 0 );

  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}


static int fvm_proc_pause( struct rpc_inc *inc ) {
  int handle, sts;
  struct loaded_fvm *lf;
  uint32_t id, stop;

  sts = xdr_decode_uint32( &inc->xdr, &id );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &stop );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  lf = lf_by_id( id );
  if( lf ) {
    if( stop == 0 ) {
      /* continue */
      lf->fvm.flags |= FVM_FLAG_RUNNING;
    } else if( stop == 1 ) {
      /* stop running */
      lf->fvm.flags &= ~FVM_FLAG_RUNNING;
    }
    else if( stop == 2 ) {
      /* reset */
      fvm_reset( &lf->fvm );
      lf->fvm.flags |= FVM_FLAG_RUNNING;
    } else {
      /* other */
    }
  }
  
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_interrupt( struct rpc_inc *inc ) {
  int handle, sts;
  struct loaded_fvm *lf;
  uint32_t id, ivec, priority;

  sts = xdr_decode_uint32( &inc->xdr, &id );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &ivec );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &priority );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  lf = lf_by_id( id );
  if( lf ) {
    fvm_interrupt( &lf->fvm, (uint16_t)ivec, (uint16_t)priority );
  }

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle ); 
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_msg( struct rpc_inc *inc ) {
  int handle, sts;
  struct loaded_fvm *lf;
  uint32_t id, msgid;
  char *bufp = NULL;
  int buflen = 0;

  sts = xdr_decode_uint32( &inc->xdr, &id );
  if( !sts ) sts = xdr_decode_uint32( &inc->xdr, &msgid );
  if( !sts ) sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &buflen );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  lf = lf_by_id( id );
  if( lf ) {
    /* signal interrupt */
    sts = fvm_interrupt( &lf->fvm, FVM_INT_MSG, FVM_INT_MSG_PL );
    if( sts ) {
      log_writef( NULL, LOG_LVL_INFO, "fvm_proc_msg interrupt failed" );
    } else {
      /* if successfully jumped to ISR then copy message and set params */
      log_writef( NULL, LOG_LVL_INFO, "fvm_proc_msg msglen=%d", buflen );
      
      if( buflen > 1024 ) buflen = 1024;
      fvm_write_mem( &lf->fvm, bufp, buflen, lf->fvm.bos );
      lf->fvm.reg[FVM_REG_R0] = msgid;         /* R0 = msgid */
      lf->fvm.reg[FVM_REG_R1] = lf->fvm.bos;   /* R1 = address of msg */
      lf->fvm.reg[FVM_REG_R2] = buflen;        /* R2 = length of msg */
    }
  }

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle ); 
  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}


static struct loaded_fvm *lf_by_id( uint32_t id ) {
    struct loaded_fvm *lf;
    lf = glob.progs;
    while( lf ) {
	if( lf->id == id ) {
	    break;
	}
	lf = lf->next;
    }
    return lf;
}

static int fvm_proc_shmemread( struct rpc_inc *inc ) {
    int handle, sts, len, offset;
    char shmem[1024];
    struct loaded_fvm *lf;
    uint32_t id;
    
    sts = xdr_decode_uint32( &inc->xdr, &id );
    if( !sts ) sts = xdr_decode_uint32( &inc->xdr, (uint32_t *)&len );
    if( !sts ) sts = xdr_decode_uint32( &inc->xdr, (uint32_t *)&offset );
    if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

    lf = lf_by_id( id );
    if( lf ) fvm_shmem_read( &lf->fvm, shmem, len, offset );
    
    rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
    xdr_encode_boolean( &inc->xdr, lf ? 1 : 0 );
    if( lf ) xdr_encode_opaque( &inc->xdr, (uint8_t *)shmem, len );
    rpc_complete_accept_reply( inc, handle );
    
    return 0;
}

static int fvm_proc_shmemwrite( struct rpc_inc *inc ) {
    int handle, sts, len, interruptp, offset;
    char shmem[1024];
    struct loaded_fvm *lf;
    uint32_t id;
    
    sts = xdr_decode_uint32( &inc->xdr, &id );
    if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
    len = sizeof(shmem);
    if( !sts ) sts = xdr_decode_uint32( &inc->xdr, (uint32_t *)&offset );
    if( !sts ) sts = xdr_decode_opaque( &inc->xdr, (uint8_t *)shmem, &len );
    if( !sts ) sts = xdr_decode_boolean( &inc->xdr, &interruptp );
    if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
    
    lf = lf_by_id( id );
    if( lf ) {
	fvm_shmem_write( &lf->fvm, shmem, len, offset );
	if( interruptp ) fvm_interrupt( &lf->fvm, FVM_INT_SMW, FVM_INT_SMW_PL );
    }
    
    rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
    xdr_encode_boolean( &inc->xdr, lf ? 1 : 0 );
    rpc_complete_accept_reply( inc, handle );
    
    return 0;
}

static int fvm_proc_read_dirty( struct rpc_inc *inc ) {
  int handle, sts, nd, i;
  struct loaded_fvm *lf;
  uint32_t id;
  struct fvm_dirty dirty[32];

  sts = xdr_decode_uint32( &inc->xdr, &id );
  if( sts ) return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );

  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );

  lf = lf_by_id( id );
  if( lf ) {
      xdr_encode_boolean( &inc->xdr, 1 );
      /* encode normal memory changes */
      nd = fvm_dirty_regions( &lf->fvm, dirty, 32 );
      for( i = 0; i < nd; i++ ) {
	  xdr_encode_boolean( &inc->xdr, 1 );
	  xdr_encode_uint32( &inc->xdr, dirty[i].offset );
	  xdr_encode_opaque( &inc->xdr, (uint8_t *)&lf->fvm.mem[dirty[i].offset], 2 * dirty[i].count );
      }
      /* registers */
      xdr_encode_boolean( &inc->xdr, 1 );
      xdr_encode_uint32( &inc->xdr, 0x00010000 );
      xdr_encode_opaque( &inc->xdr, (uint8_t *)lf->fvm.reg, sizeof(lf->fvm.reg) );
      /* other state */
      xdr_encode_boolean( &inc->xdr, 1 );
      xdr_encode_uint32( &inc->xdr, 0x00020000 );
      xdr_encode_uint32( &inc->xdr, 52 );
      xdr_encode_uint32( &inc->xdr, lf->fvm.flags );
      xdr_encode_uint64( &inc->xdr, lf->fvm.tickcount );
      xdr_encode_uint32( &inc->xdr, lf->fvm.bos );
      xdr_encode_uint64( &inc->xdr, lf->fvm.sleep_timeout );
      xdr_encode_uint32( &inc->xdr, lf->fvm.rpc.bufaddr );
      xdr_encode_uint32( &inc->xdr, lf->fvm.rpc.buf.offset );
      xdr_encode_uint32( &inc->xdr, lf->fvm.rpc.buf.count );
      xdr_encode_uint32( &inc->xdr, lf->fvm.rpc.timeout );
      xdr_encode_uint32( &inc->xdr, lf->fvm.rpc.service );
      xdr_encode_uint64( &inc->xdr, lf->fvm.rpc.hostid );
	
      xdr_encode_boolean( &inc->xdr, 0 );
  } else {
      xdr_encode_boolean( &inc->xdr, 0 );
  }

  rpc_complete_accept_reply( inc, handle );
  
  return 0;
}

static int fvm_proc_write_dirty( struct rpc_inc *inc ) {
  int handle, sts;
  int lenp, b, offset;
  uint32_t id;
  char *bufp;
  struct loaded_fvm *lf;
  
  sts = xdr_decode_uint32( &inc->xdr, &id );
  if( sts ) goto badargs;

  lf = lf_by_id( id );
  if( !lf ) goto badargs;
  
  sts = xdr_decode_boolean( &inc->xdr, &b );
  if( sts ) goto badargs;
  while( b ) {
    sts = xdr_decode_uint32( &inc->xdr, (uint32_t *)&offset );
    if( sts ) goto badargs;

    switch( offset ) {
    case 0x00010000:
      /* registers */
      lenp = sizeof(lf->fvm.reg);
      sts = xdr_decode_opaque( &inc->xdr, (uint8_t *)lf->fvm.reg, &lenp );
      if( sts ) goto badargs;
      break;
    case 0x00020000:
      /* misc state */
      sts = xdr_decode_uint32( &inc->xdr, (uint32_t *)&lenp );
      if( lenp != 52 ) goto badargs;

      xdr_decode_uint32( &inc->xdr, &lf->fvm.flags );
      xdr_decode_uint64( &inc->xdr, &lf->fvm.tickcount );
      xdr_decode_uint32( &inc->xdr, (uint32_t *)&lenp );
      lf->fvm.bos = lenp;
      xdr_decode_uint64( &inc->xdr, &lf->fvm.sleep_timeout );
      xdr_decode_uint32( &inc->xdr, (uint32_t *)&lenp );
      lf->fvm.rpc.bufaddr = lenp;  
      xdr_decode_uint32( &inc->xdr, (uint32_t *)&lf->fvm.rpc.buf.offset );
      xdr_decode_uint32( &inc->xdr, (uint32_t *)&lf->fvm.rpc.buf.count );
      xdr_decode_uint32( &inc->xdr, (uint32_t *)&lenp );
      lf->fvm.rpc.timeout = lenp;
      xdr_decode_uint32( &inc->xdr, (uint32_t *)&lenp );
      lf->fvm.rpc.service = lenp;
      xdr_decode_uint64( &inc->xdr, &lf->fvm.rpc.hostid );
			 
      break;
    default:
      /* memory */
      sts = xdr_decode_opaque_ref( &inc->xdr, (uint8_t **)&bufp, &lenp );
      if( sts ) goto badargs;

      fvm_write_mem( &lf->fvm, bufp, lenp, offset );
      
      break;
    }
    
    sts = xdr_decode_boolean( &inc->xdr, &b );
    if( sts ) goto badargs;
  }
  
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  xdr_encode_boolean( &inc->xdr, 1 );
  rpc_complete_accept_reply( inc, handle );
  
  return 0;

 badargs:
  return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
}

  




static struct rpc_proc fvm_procs[] = {
  { 0, fvm_proc_null },
  { 1, fvm_proc_load },
  { 2, fvm_proc_unload },
  { 3, fvm_proc_list },
  { 4, fvm_proc_pause },
  { 5, fvm_proc_interrupt },
  { 6, fvm_proc_msg },
  { 7, fvm_proc_shmemread },
  { 8, fvm_proc_shmemwrite },
  { 9, fvm_proc_read_dirty },
  { 10, fvm_proc_write_dirty },  
  { 0, NULL }
};

static struct rpc_version fvm_vers = {
  NULL, FVM_RPC_VERS, fvm_procs
};

static struct rpc_program fvm_prog = {
  NULL, FVM_RPC_PROG, &fvm_vers
};

void fvm_rpc_force_iter( void ) {
  fvm_iter.timeout = 0;
}

static void fvm_iter_cb( struct rpc_iterator *iter ) {
  struct loaded_fvm *lf, *prev, *next;
  uint64_t timeout, start;

  timeout = iter->timeout;
  lf = glob.progs;
  prev = NULL;
  while( lf ) {
    next = lf->next;

    start = rpc_now();
    fvm_run_nsteps( &lf->fvm, 1000 );
    lf->runtime += rpc_now() - start;
    if( lf->fvm.flags & FVM_FLAG_RUNNING ) {
      /* program still needs to run, but we yield here and schedule ourselves to be run again ASAP */
      timeout = 0;
      prev = lf;
    } else if( (lf->fvm.flags & FVM_FLAG_DONE) ) {
	/* invoke completion callback */
	if( lf->donecb ) lf->donecb( lf );

	/* publish completion event */
	rpcd_event_publish( FVM_EVENT_CATEGORY, FVM_EVENT_PROGDONE, &lf->id, sizeof(lf->id) );

	/* unload */
	if( lf->flags & FVM_RPC_AUTOUNLOAD ) {
	    if( prev ) prev->next = next;
	    else glob.progs = next;
	    if( lf->inlogid ) log_close( &lf->inlog );
	    if( lf->outlogid ) log_close( &lf->outlog );

	    /* push onto freelist */
	    lf->next = glob.flist;
	    glob.flist = lf;
	}
    } else if( lf->fvm.sleep_timeout && (rpc_now() >= lf->fvm.sleep_timeout) ) {
	lf->fvm.flags |= FVM_FLAG_RUNNING;
	lf->fvm.sleep_timeout = 0;
	timeout = 0;
    } else {
	/* stopped running but not set to auto unload when stopped */
	prev = lf;
    }
    lf = next;
  }

  iter->timeout = timeout;
}

/*
 * /fju/fvm/programs/NAME/
 *   progdata opaque   program data
 *   program string    path to file storing program
 *   flags   uint32
 *   inlog   uint64 
 *   outlog  uint64 
 */
static uint64_t fvm_root_handle( void ) {
    static uint64_t hreg = 0;
    if( !hreg ) {
	freg_subkey( NULL, 0, "/fju/fvm", 0, &hreg );
    }
    return hreg;
}
static uint64_t fvm_prog_handle( void ) {
    static uint64_t hreg = 0;
    if( !hreg ) {
        freg_subkey( NULL, fvm_root_handle(), "programs", 0, &hreg );
    }
    return hreg;
}
	
static struct loaded_fvm *load_freg_prog( struct freg_entry *entry ) {
    int sts, proglen, len;
    char *progdata;
    char path[256];
    struct mmf_s mmf;
    uint32_t flags;
    uint64_t inlog, outlog, id;
    struct loaded_fvm *lf;
    
    id = entry->id;
    progdata = NULL;
    
    sts = freg_get_by_name( NULL, id, "progdata", FREG_TYPE_OPAQUE, NULL, 0, &len );
    if( sts ) {
	/* try loading by path */
	sts = freg_get_by_name( NULL, id, "path", FREG_TYPE_STRING, path, sizeof(path), NULL );
	if( sts ) return NULL;

	sts = mmf_open2( path, &mmf, MMF_OPEN_EXISTING );
	if( sts ) {
	  log_writef( NULL, LOG_LVL_ERROR, "mmf_open2 failed \"%s\"", path );
	} else {
	  sts = mmf_remap( &mmf, mmf.fsize );
	  if( sts ) {
	    log_writef( NULL, LOG_LVL_ERROR, "mmf_remap failed" );
	  } else {
	    progdata = malloc( mmf.fsize );
	    proglen = mmf.fsize;
	    
	    memcpy( progdata, mmf.file, mmf.fsize );
	  }
	  
	  mmf_close( &mmf );
	}
    } else {
	/* load from program data directly */
        proglen = len;
	progdata = malloc( proglen );
	sts = freg_get_by_name( NULL, id, "progdata", FREG_TYPE_OPAQUE, progdata, proglen, &proglen );
	if( sts ) {
	  log_writef( NULL, LOG_LVL_ERROR, "Unexpected failure reading progdata" );
	  free( progdata );
	  return NULL;
	}
    }
    if( !progdata ) return NULL;
    
    /* get other params or use defaults */
    flags = FVM_RPC_AUTOUNLOAD|FVM_RPC_START;
    inlog = 0;
    outlog = 0;
    
    freg_get_by_name( NULL, id, "flags", FREG_TYPE_UINT32, (char *)&flags, sizeof(flags), NULL );
    freg_get_by_name( NULL, id, "inlog", FREG_TYPE_UINT64, (char *)&inlog, sizeof(inlog), NULL );
    freg_get_by_name( NULL, id, "outlog", FREG_TYPE_UINT64, (char *)&outlog, sizeof(outlog), NULL );

    lf = fvm_load_prog( (uint8_t *)progdata, proglen, flags, inlog, outlog, entry->name );
    log_writef( NULL, LOG_LVL_INFO, "Loaded program \"%s\" len=%d flags=%x inlog=%"PRIx64" outlog=%"PRIx64"",
		entry->name, proglen, flags, inlog, outlog );
    
    free( progdata );
    progdata = NULL;
    
    return lf;    
}

static struct loaded_fvm *load_prog_by_name( char *name ) {
    int sts;
    struct freg_entry entry;
    
    sts = freg_entry_by_name( NULL, fvm_prog_handle(), name, &entry, NULL );
    if( sts ) return NULL;
    return load_freg_prog( &entry );
}


static void load_startup_progs( void ) {
    int sts;
    uint64_t fvmid, id;
    struct freg_entry entry;
    char name[64];
    
    sts = freg_subkey( NULL, fvm_root_handle(), "startup", FREG_CREATE, &fvmid );
    if( sts ) return;

    id = 0;
    do {
	sts = freg_next( NULL, fvmid, id, &entry );
	if( sts ) break;
	if( (entry.flags & FREG_TYPE_MASK) != FREG_TYPE_STRING ) goto cont;

	sts = freg_get( NULL, entry.id, NULL, name, sizeof(name), NULL );
	if( !sts ) load_prog_by_name( name );

    cont:
	id = entry.id;
    } while( 1 );
}


static void fvm_evt_cb( struct rpcd_subscriber *sc, uint32_t category, uint32_t id, void *parm, int parmsize ) {
  struct event_prog *ep;
  int i;
  struct loaded_fvm *lf;
  
  for( i = 0; i < glob.nevtprogs; i++ ) {
    ep = &glob.evtprogs[i];
    
    if( (ep->category == category) &&
	((ep->eventid == -1) || (ep->eventid == id)) ) {
      log_writef( NULL, LOG_LVL_INFO, "fvm_evt_cb category=%u eventid=%u program=%s", category, id, ep->name );
      lf = load_prog_by_name( ep->name );
      if( lf ) {
	  /* copy into memory and set dirty pages */
	  fvm_write_mem( &lf->fvm, parm, parmsize, lf->fvm.bos );
	  
	  lf->fvm.reg[FVM_REG_R0] = (uint16_t)parmsize;
	  fvm_push_value( &lf->fvm, category >> 16 );
	  fvm_push_value( &lf->fvm, category & 0xffff );
	  fvm_push_value( &lf->fvm, id >> 16 );
	  fvm_push_value( &lf->fvm, id & 0xffff );
      }
    }
    
  }
  
}

static void reload_evtprogs( struct rpc_iterator *iter ) {
  int sts, i;
  uint64_t fvmid, id;
  struct freg_entry entry, tlentry;
  
  (void)(iter);
  
  glob.nevtprogs = 0;
  
  sts = freg_subkey( NULL, fvm_root_handle(), "event", FREG_CREATE, &fvmid );
  if( sts ) return;

  id = 0;
  do {
    sts = freg_next( NULL, fvmid, id, &entry );
    if( sts ) break;
    if( (entry.flags & FREG_TYPE_MASK) != FREG_TYPE_KEY ) goto cont;
    tlentry = entry;
    
    log_writef( NULL, LOG_LVL_TRACE, "Loading event trigger program \"%s\"", entry.name );

    i = glob.nevtprogs;
    sts = freg_get_by_name( NULL, tlentry.id, "category", FREG_TYPE_UINT32, (char *)&glob.evtprogs[i].category, sizeof(uint32_t), NULL );
    if( !sts ) {
	glob.evtprogs[i].eventid = -1;
	freg_get_by_name( NULL, tlentry.id, "eventid", FREG_TYPE_UINT32, (char *)&glob.evtprogs[i].eventid, sizeof(uint32_t), NULL );
    }
    strncpy( glob.evtprogs[i].name, tlentry.name, sizeof(glob.evtprogs[i].name) - 1 );
    
    if( sts ) {
	log_writef( NULL, LOG_LVL_ERROR, "Failed to load event program \"%s\" category=%u eventid=%d",
		    tlentry.name, glob.evtprogs[i].category, glob.evtprogs[i].eventid );
      goto cont;
    }

    log_writef( NULL, LOG_LVL_TRACE, "Loaded event program \"%s\"", tlentry.name );
    glob.nevtprogs++;
    if( glob.nevtprogs >= FVM_MAX_EVTPROGS ) break;
						    
  cont:
    id = entry.id;
  } while( 1 );

}

static struct rpc_iterator fvm_evtprog_iter = {
    NULL,
    0,
    30000,
    reload_evtprogs,
    NULL
};


void fvm_register( void ) {
  struct loaded_fvm *lf;
  int i;
  
  /* allocate program descriptors */
  lf = malloc( sizeof(*lf) * FVM_MAX_PROGRAM );
  for( i = 0; i < FVM_MAX_PROGRAM; i++ ) {
    memset( &lf[i], 0, sizeof(*lf) );
    lf[i].next = glob.flist;
    glob.flist = &lf[i];
  }
  
  rpc_program_register( &fvm_prog );
  rpc_iterator_register( &fvm_iter );
  load_startup_progs();

  glob.subsc.cb = fvm_evt_cb;
  rpcd_event_subscribe( &glob.subsc );

  rpc_iterator_register( &fvm_evtprog_iter );
}

#if 0

/*
 * Is this something that's actually useful? 
 * Typically you will actually want an fvm program to be running before 
 * it receives the rpc call so this probably won't be very useful. 
 * It would be better / more useful to instead send a message (fvm_proc_msg) to a running 
 * program and arrange for it to send an rpc back when it's done processing.
 * But all this would have to be done from within the fvm program itself.
 */

int fvm_proc_handler( char *progname, int timeout, struct rpc_inc *inc ) {
  int handle;
  struct loaded_fvm *lf;
  
  lf = load_prog_by_name( progname );
  if( !lf ) {
    return rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_GARBAGE_ARGS, NULL, NULL );
  }

  /* copy arg data into xdr buffer */
  memcpy( lf->fvm.rpc.buf.buf, inc->xdr.buf + inc->xdr.offset, inc->xdr.count - inc->xdr.offset );
  lf->fvm.rpc.buf.count = inc->xdr.count - inc->xdr.offset;
  lf->fvm.rpc.buf.offset = 0;

  /* run until it halts (up to some timeout). no sleeps or rpcs allowed. need an asynchronous version for that */
  fvm_run_timeout( &lf->fvm, timeout ? timeout : 1000 );

  /* reply with the generated xdr */
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  memcpy( inc->xdr.buf + inc->xdr.offset, lf->fvm.rpc.buf.buf, lf->fvm.rpc.buf.offset );
  inc->xdr.offset += lf->fvm.rpc.buf.offset;
  rpc_complete_accept_reply( inc, handle );
  
  /* make it get unloaded immediately */
  lf->flags |= FVM_RPC_AUTOUNLOAD;
  lf->fvm.flags |= FVM_FLAG_DONE;

  return 0;
}

#endif
