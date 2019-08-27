
#ifndef RPCD_H
#define RPCD_H

#ifdef WIN32
#include <Winsock2.h>
#include <Windows.h>
#include <ws2tcpip.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#ifndef WIN32
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <poll.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <fcntl.h>
#include <inttypes.h>
#endif

#include <fju/rpc.h>

#define RPC_MAX_BUF (1024*1024)
#define RPC_MAX_CONN 32 
#define RPC_MAX_LISTEN 8
#define RPC_CONNECTION_TIMEOUT (60*1000)  /* how long to wait before a connection is deemed stale and gets closed */

typedef enum {
	RPC_LISTEN_UDP = 1,
	RPC_LISTEN_UDP6 = 2,
	RPC_LISTEN_TCP = 3,
	RPC_LISTEN_TCP6 = 4,
	RPC_LISTEN_UNIX = 5,
} rpc_listen_t;

struct rpc_listen {
#ifdef WIN32
	SOCKET fd;
#else
	int fd;
#endif
	rpc_listen_t type;
	union {
		struct sockaddr_in sin;
		struct sockaddr_in6 sin6;
#ifndef WIN32
		struct sockaddr_un sun;
#endif
	} addr;
};

/* networking state */
typedef enum {
	RPC_NSTATE_RECV = 1,
	RPC_NSTATE_SEND = 2,
	RPC_NSTATE_CONNECT = 3,
} rpc_nstate_t;

typedef enum {
	RPC_CSTATE_RECVLEN = 1,
	RPC_CSTATE_RECV = 2,
	RPC_CSTATE_SENDLEN = 3,
	RPC_CSTATE_SEND = 4,
	RPC_CSTATE_CONNECT = 5,
	RPC_CSTATE_CLOSE = 6,
} rpc_cstate_t;

struct rpc_conn;
typedef enum {
    RPC_CONN_CLOSE = 0,
    RPC_CONN_CONNECT = 1,
} rpc_conn_event_t;
typedef void (*rpc_conn_cb_t)( rpc_conn_event_t event, struct rpc_conn *conn );

struct rpc_conn {
	struct rpc_conn *next;

	uint64_t connid;

#ifdef WIN32
	SOCKET fd;
#else
	int fd;
#endif
	rpc_nstate_t nstate;

	rpc_cstate_t cstate;
	struct {
		uint32_t count;       /* amount to send/recv */
		uint32_t offset;      /* how much has been sent/recv */
		uint64_t timeout;     /* when to give up */

		/* callback on completion */
	        rpc_conn_cb_t cb;
		void *cxt;
	} cdata;

	struct rpc_inc inc;

	uint32_t count;    /* size of buf */
	uint8_t *buf;

	uint64_t timestamp;
};


int rpcd_main( int argc, char **argv, void (*init_cb)(void) );
struct rpc_listen *rpcd_listen_by_type( rpc_listen_t type );

int rpc_connect( struct sockaddr *addr, socklen_t alen, rpc_conn_cb_t cb, void *cxt, uint64_t *connid );
int rpc_send( struct rpc_conn *c, int count );
struct rpc_conn *rpc_conn_acquire( void );
void rpc_conn_release( struct rpc_conn *c );
struct rpc_conn *rpc_conn_by_connid( uint64_t connid );
void rpc_conn_close( struct rpc_conn *c );

typedef void (*rpcd_main_t)( uint32_t vers );

#endif

