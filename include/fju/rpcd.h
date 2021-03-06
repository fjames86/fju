
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
#include <fju/mmf.h>

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
    RPC_CONN_CLOSE = 0,  /* connection is closing */
    RPC_CONN_CONNECT = 1, /* connect completed successfully */
    RPC_CONN_DONE_SEND = 2, /* send operation completed, connetion reverted to recvlen (idle) */
} rpc_conn_event_t;
typedef void (*rpc_conn_cb_t)( rpc_conn_event_t event, struct rpc_conn *conn );

typedef enum {
    RPC_CONN_DIR_INCOMING = 0,
    RPC_CONN_DIR_OUTGOING = 1,
} rpc_conn_dir_t;

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

	  uint64_t rx;
	  uint64_t tx;
	} cdata;

	struct rpc_inc inc;

	uint32_t count;    /* size of buf */
	uint8_t *buf;

	uint64_t timestamp;

  rpc_listen_t listype;
  struct rpc_listen *listen;
  struct sockaddr_storage addr;
  int addrlen;

  rpc_conn_dir_t dirtype;
};

typedef enum {
	      RPCD_EVT_INIT = 0,      /* initialization callback */
	      RPCD_EVT_CLOSE = 1,     /* close callback */
	      RPCD_EVT_SIGNAL = 2,    /* daemon received a signal, arg=sig. unix only */
} rpcd_evt_t;

typedef void (*rpcd_main_t)( rpcd_evt_t evt, void *arg, void *cxt );
int rpcd_main( int argc, char **argv, rpcd_main_t cb, void *cxt );
struct rpc_listen *rpcd_listen_by_type( rpc_listen_t type );

/* register to listen on these protocols/ports. these functions may only be called from within a maincb init routine */
int rpcd_listen_tcp( int port );
int rpcd_listen_tcp6( int port );
int rpcd_listen_udp( int port );
int rpcd_listen_udp6( int port );
#ifndef WIN32
int rpcd_listen_unix( char *path );
#endif

int rpc_connect( struct sockaddr *addr, socklen_t alen, rpc_conn_cb_t cb, void *cxt, uint64_t *connid );
int rpc_send( struct rpc_conn *c, int count );
struct rpc_conn *rpc_conn_acquire( void );
void rpc_conn_release( struct rpc_conn *c );
struct rpc_conn *rpc_conn_by_connid( uint64_t connid );

/* find incoming connection by address */
struct rpc_conn *rpc_conn_by_addr( rpc_listen_t type, char *addr, int addrlen );
void rpc_conn_close( struct rpc_conn *c );

/* dynamically loaded service entry point */
typedef void (*rpcd_service_t)( void );

/* stop the daemon and exit */
void rpcd_stop( void );

/* true if running as rpcd */
int rpcdp( void );
int rpcd_get_default_port( void );


struct rpcd_active_conn {
    struct rpc_listen *listen;
    struct rpc_conn *conn;
};
int rpcd_active_conn( struct rpcd_active_conn *aconn );

void rpcd_init( void );
void rpc_service( int timeout );

#ifdef WIN32
HANDLE rpcd_win32event( void );
#endif

typedef void (*rpcd_aio_donecb)( struct mmf_s *mmf, char *buf, int len, void *prv );
int rpcd_aio_read( struct mmf_s *mmf, char *buf, int len, uint64_t offset, rpcd_aio_donecb donecb, void *prv );
int rpcd_aio_write( struct mmf_s *mmf, char *buf, int len, uint64_t offset, rpcd_aio_donecb donecb, void *prv );


#endif

