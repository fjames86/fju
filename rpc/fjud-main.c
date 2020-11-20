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
#include <fju/log.h>
#include <fju/nls.h>
#include <fju/freg.h>
#include <fju/fvm.h>
#include <fju/cht-rsync.h>
#include <fju/sec.h>
#include <fju/lic.h>
#include <fju/hostreg.h>
#include <fju/lic.h>
#include <fju/raft.h>

#include "rpc-private.h"

#include <time.h>

int cmdprog_register( void );
void rex_register( void );

#if 0

#include <sys/resource.h>
static void fjud_rusage_iter( struct rpc_iterator *iter ) {
  struct rusage ru;
  getrusage( RUSAGE_SELF, &ru );
  rpc_log( RPC_LOG_INFO, "data rss %"PRIu64"", ru.ru_idrss );
}
static struct rpc_iterator rusage_iter =
  {
   NULL,
   0,
   1000,
   fjud_rusage_iter,
   NULL
  };
#endif  

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
  nls_register();
  freg_register();
  fvm_rpc_register();
  cht_rsync_initialize();
  cmdprog_register();
  raft_register();
  rex_register();
  
  //rpc_iterator_register( &rusage_iter );
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

#define FJUD_LICEXPIRE_WARN 30

static void check_license( struct rpc_iterator *iter ) {
  struct lic_s lic;
  int sts;

  sts = fju_check_license( NULL, 0, &lic );
  if( sts ) {
    rpc_log( RPC_LOG_ERROR, "License check failed, exiting." );
    rpcd_stop();
  } else if( (lic.expire - time( NULL )) < FJUD_LICEXPIRE_WARN ) {
    rpc_log( RPC_LOG_WARN, "License will expire in %d days", (lic.expire - time( NULL )) / (60*60*24) );
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

