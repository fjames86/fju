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

/*
 * This is really an example program showing how to use the api to implement an rpc service.
 * It should be simple to add other services if required. For now it is the only service host.
 * 
 */


#include <fju/rpcd.h>
#include <fju/shauth.h>
#include <fju/hrauth.h>
#include <fju/raft.h>
#include <fju/log.h>
#include <fju/nls.h>
#include <fju/freg.h>
#include <fju/fvm.h>
#include <fju/cht-rsync.h>
#include <fju/sec.h>
#include <fju/lic.h>
#include <fju/hostreg.h>

#include "rpc-private.h"


static void init_cb( void ) {
  /* 
   * Register programs, auth providers and other initialization. 
   */
  rpcbind_register();
  shauth_register( NULL );
  hrauth_register();
  
  /* 
   * These could be moved out to separate modules and dynamically loaded but for now 
   * they are included in libfju so can be called directly from here.
   */     
  raft_register();
  nls_register();
  freg_register();
  fvm_rpc_register();
  cht_rsync_initialize();
}

/* -------- Setup logging ---------------- */

static int logger_open = 0;
static struct log_s logger;
static struct rpc_logger rpcd_loggers[1];

static void rpcd_logger_cb( int lvl, char *fmt, va_list args ) {
  static int mypid = 0;
  int loglvl = 0;

  if( !mypid ) {
#ifdef WIN32
      mypid = GetCurrentProcessId();
#else
      mypid = getpid();
#endif
  }
  logger.pid = mypid;
  
  if( lvl == RPC_LOG_TRACE ) loglvl = LOG_LVL_TRACE;
  else if( lvl == RPC_LOG_INFO ) loglvl = LOG_LVL_INFO;
  else if( lvl == RPC_LOG_DEBUG ) loglvl = LOG_LVL_DEBUG;
  else if( lvl == RPC_LOG_WARN ) loglvl = LOG_LVL_WARN;
  else if( lvl == RPC_LOG_ERROR ) loglvl = LOG_LVL_ERROR;
  else if( lvl == RPC_LOG_FATAL ) loglvl = LOG_LVL_FATAL;
  
  log_writev( &logger, loglvl, fmt, args );
}

static void close_cb( void ) {
  rpc_log( RPC_LOG_INFO, "shutting down" );
  
  if( logger_open ) {
    log_close( &logger );
  }
}

static void main_cb( rpcd_evt_t evt, void *arg, void *cxt ) {
  switch( evt ) {
  case RPCD_EVT_INIT:
    init_cb();
    break;
  case RPCD_EVT_CLOSE:
    close_cb();
    break;
  default:
    break;
  }
}

static void check_license( struct rpc_iterator *iter ) {
  static uint8_t pubkeybuf[] =
    {
     0xd6, 0xfd, 0x57, 0xa7, 0xbd, 0xff, 0xca, 0xc2, 0xe1, 0xc0, 0x63, 0xcb, 0xfb,
     0x72, 0x3f, 0x21, 0x02, 0x7e, 0x1f, 0x93, 0xf3, 0xb3, 0x65, 0xc2, 0x23, 0x69,
     0x50, 0x60, 0x5f, 0x6a, 0x52, 0xc7, 0x75, 0x7e, 0xe4, 0xb2, 0xdc, 0xfe, 0x9e,
     0x77, 0xbe, 0x19, 0x87, 0xbc, 0x4f, 0x22, 0x75, 0xeb, 0x7c, 0x6e, 0x63, 0xa3,
     0x19, 0x74, 0xe8, 0xb6, 0xa0, 0x28, 0xff, 0x54, 0x2a, 0xff, 0xa2, 0x51
    };
  
  struct lic_s lic;
  int len, sts;
  struct sec_buf secret, pubkey, common, iov[1], sig;
  char commonbuf[SEC_ECDH_MAX_COMMON];
  struct hostreg_prop prop;
  
  sts = freg_get_by_name( NULL, 0, "/fju/lic", FREG_TYPE_OPAQUE, (char *)&lic, sizeof(lic), &len );
  if( !sts ) {
    hostreg_prop( &prop );
    secret.buf = (char *)prop.privkey;
    secret.len = sizeof(prop.privkey);
    pubkey.buf = (char *)pubkeybuf;
    pubkey.len = sizeof(pubkeybuf);    
    common.buf = commonbuf;
    common.len = sizeof(commonbuf);
    ecdh_common( &secret, &pubkey, &common );
    aes_decrypt( (uint8_t *)common.buf, (uint8_t *)&lic, sizeof(lic) );
          
    if( len != sizeof(lic) ) sts = -1;
    else if( lic.hostid != hostreg_localid() ) {
      rpc_log( RPC_LOG_ERROR, "lic bad hostid" );
      sts = -1;
    } else if( time( NULL ) > lic.expire ) {
      rpc_log( RPC_LOG_ERROR, "lic expired" );
      sts = -1;
    } else {
      iov[0].buf = (char *)&lic;
      iov[0].len = 32;
      sig.buf = lic.verf;
      sig.len = lic.nverf;
      sts = sec_verify( &pubkey, iov, 1, &sig );
      if( sts ) {
	rpc_log( RPC_LOG_ERROR, "lic verify failed" );
      }
    }
  }
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "Licensing failed, exiting." );
    rpcd_stop();
  }  
}

static struct rpc_iterator liciter =
  {
   NULL,
   0,
   1000*60*60*24,
   check_license,
   NULL
  };

int main( int argc, char **argv ) {
  int sts;

  /* open log */
  sts = log_open( NULL, NULL, &logger );
  if( sts ) printf( "Warning: failed to open default log file\n" );
  else { 
    logger_open = 1;
    logger.ltag = RPC_LOG_LTAG;
    rpcd_loggers[0].cb = rpcd_logger_cb;
    rpc_add_logger( &rpcd_loggers[0] );
  }

  /* check license */
  rpc_iterator_register( &liciter );
  
  /* run daemon. */
  rpcd_main( argc, argv, main_cb, NULL );
 
  return 0;
}

