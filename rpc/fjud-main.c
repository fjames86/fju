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
  
  /* run daemon. */
  rpcd_main( argc, argv, main_cb, NULL );
 
  return 0;
}

