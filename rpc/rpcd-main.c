
#include "rpcd.h"
#include "shauth.h"
#include <hrauth.h>
#include <raft.h>
#include <log.h>

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

  //raft_register();
}

static int logger_open = 0;
static struct log_s logger;

static struct rpc_logger rpcd_loggers[1];

static void rpcd_logger_cb( int lvl, char *fmt, va_list args ) {
  int loglvl = 0;
  if( lvl == RPC_LOG_INFO ) loglvl = LOG_LVL_INFO;
  else if( lvl == RPC_LOG_DEBUG ) loglvl = LOG_LVL_DEBUG;
  else if( lvl == RPC_LOG_WARN ) loglvl = LOG_LVL_WARN;
  else if( lvl == RPC_LOG_ERROR ) loglvl = LOG_LVL_ERROR;
  
  log_writev( &logger, loglvl, fmt, args );
}

int main( int argc, char **argv ) {
  int sts;

  /* open log */
  sts = log_open( mmf_default_path( "rpcd.log", NULL ), NULL, &logger );
  if( sts ) printf( "Warning: Failed to open rpcd debug log file\n" );
  else { 
    logger_open = 1;

    rpcd_loggers[0].cb = rpcd_logger_cb;
    rpc_add_logger( &rpcd_loggers[0] );
  }
  
  /* run daemon */
  rpcd_main( argc, argv, init_cb );

  if( logger_open ) log_close( &logger );
  
  return 0;
}

