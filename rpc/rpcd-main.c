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
 
#include <fju/rpcd.h>
#include <fju/shauth.h>
#include <fju/hrauth.h>
#include <fju/raft.h>
#include <fju/log.h>
#include <fju/nls.h>
#include <fju/rex.h>
#include <fju/freg.h>

#include "rpc-private.h"

#define SHAUTH_SECRET "123abcd123"

static void init_cb( void ) {
  /* register rpc programs */
  rpcbind_register();
  
  /* register providers */
  {
    uint8_t key[32];
    int i;
    char *p, *terminator;
    char tmp[4];
    
    memset( key, 0, sizeof(key) );
    p = SHAUTH_SECRET;
    for( i = 0; i < 32; i++ ) {
      memset( tmp, 0, 4 );
      if( *p ) {
	tmp[0] = *p;
	p++;
      }
      if( *p ) {
	tmp[1] = *p;
	p++;
      }
      
      key[i] = (uint8_t)strtoul( tmp, &terminator, 16 );
      if( *terminator ) break;
      
      if( !*p ) break;
    }
    shauth_register( key );
  }

  hrauth_register();
  raft_register();
  nls_register();
  rex_register();
  freg_register();
}

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

int main( int argc, char **argv ) {
  int sts;

  /* open log */
  sts = log_open( NULL, NULL, &logger );
  if( sts ) printf( "Warning: rpcd failed to open default log file\n" );
  else { 
    logger_open = 1;
    logger.ltag = RPC_LOG_LTAG;
    rpcd_loggers[0].cb = rpcd_logger_cb;
    rpc_add_logger( &rpcd_loggers[0] );
  }
  
  /* run daemon */
  rpcd_main( argc, argv, init_cb );

  if( logger_open ) log_close( &logger );
  
  return 0;
}

