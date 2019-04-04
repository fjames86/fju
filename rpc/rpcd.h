
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

#include "rpc.h"

#define RPC_MAX_BUF (1024*1024)
#define RPC_MAX_CONN 32 
#define RPC_MAX_LISTEN 8

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
struct rpc_conn {
	struct rpc_conn *next;

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
		void( *cb )(struct rpc_conn *c);
		void *cxt;
	} cdata;

	struct rpc_inc inc;

	uint32_t count;    /* size of buf */
	uint8_t *buf;
};


int rpcd_main( int argc, char **argv, void (*init_cb)(void) );
int rpc_connect( struct sockaddr *addr, socklen_t alen, void( *cb )(struct rpc_conn *c), void *cxt );


#endif

