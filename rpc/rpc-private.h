
#ifndef RPC_PRIVATE_H
#define RPC_PRIVATE_H

#define RPC_LOG_TRACE 0
#define RPC_LOG_DEBUG 1
#define RPC_LOG_INFO  2
#define RPC_LOG_WARN  3
#define RPC_LOG_ERROR 4
#define RPC_LOG_FATAL 5
void rpc_log( int lvl, char *fmt, ... );

struct rpc_logger {
  struct rpc_logger *next;
  void (*cb)( int lvl, char *fmt, va_list args );
};
void rpc_add_logger( struct rpc_logger *logger );

#define RPC_LOG_LTAG 0x1000

struct rpc_conn *rpc_conn_list( void );

#endif

