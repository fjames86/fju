/*
 * MIT License
 *
 * Copyright (c) 2018 Frank James
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
 * This file defines an example rpc daemon which can listen on a set of TCP/UDP ports or
 * UNIX file sockets. Essentially all this file does is implement a standard file descriptor
 * polling loop, waiting for things to happen. When it has buffered a complete message
 * it invokes rpc_process_incoming (from the library rpc.c) which will process
 * the message and invoke the relevant procedure, if available. The receive buffer
 * is filled out with the reply data and should then be used to send back to the client.
 */

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

#ifdef USE_SHAUTH
#include "shauth.h"
#endif

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

static struct {
	int foreground;
	volatile int exiting;

	struct rpc_listen listen[RPC_MAX_LISTEN];   /* listening fds */
	int nlisten;

	struct rpc_conn *clist;    /* active connection list */
	struct rpc_conn *flist;    /* free list */

	char *shauth_secret;

#ifdef WIN32
	HANDLE evt;
	SERVICE_STATUS_HANDLE hsvc;
	SERVICE_STATUS svcsts;
#endif

	struct rpc_conn conntab[RPC_MAX_CONN];
	uint8_t buftab[RPC_MAX_CONN][RPC_MAX_BUF];

} rpc;

#ifdef WIN32
typedef int socklen_t;
static void WINAPI rpcd_svc( DWORD argc, char **argv );
#endif

static void rpc_run( void );
static void rpc_accept( struct rpc_listen *lis );


#ifndef WIN32
static void rpc_sig( int sig, siginfo_t *info, void *context ) {

	(void)(info);
	(void)(context);

	switch( sig ) {
	case SIGINT:
	case SIGTERM:
		rpc.exiting = 1;
		break;
	}

}
#endif

static void usage( char *fmt, ... ) {
	va_list args;

	printf( "Usage: rpcd [-u port] [-u6 port]\n"
		"            [-t port] [-t6 port]\n"
#ifndef WIN32
		"            [-L path]\n"
#endif
		"            [-f]\n"
		"\n"
		"  Where:\n"
		"            -f               Run in foreground\n"
		"            -u -u6           Listen on IP/UDP or IPv6/UDP\n"
		"            -t -t6           Listen on IP/TCP or IPv6/TCP\n"
#ifndef WIN32
		"            -L path          Listen on AF_UNIX socket file\n"
#endif
#ifdef USE_SHAUTH
		"            -s secret        Shared secret\n"
#endif
		"\n" );


	if( fmt ) {
		va_start( args, fmt );
		printf( "Error: " );
		vprintf( fmt, args );
		va_end( args );
		printf( "\n" );
	}

	exit( 0 );
}

int main( int argc, char **argv ) {
#ifndef WIN32
	struct sigaction sa;
#endif
	int i;

	/* parse command line */
	i = 1;
	while( i < argc ) {
		if( strcmp( argv[i], "-t" ) == 0 ) {
			if( rpc.nlisten >= RPC_MAX_LISTEN ) usage( "Out of listen descriptors" );

			rpc.listen[rpc.nlisten].type = RPC_LISTEN_TCP;
			i++;
			if( i >= argc ) usage( NULL );
			rpc.listen[rpc.nlisten].addr.sin.sin_family = AF_INET;
			rpc.listen[rpc.nlisten].addr.sin.sin_port = htons( atoi( argv[i] ) );
			rpc.nlisten++;
		}
		else if( strcmp( argv[i], "-t6" ) == 0 ) {
			if( rpc.nlisten >= RPC_MAX_LISTEN ) usage( "Out of listen descriptors" );

			rpc.listen[rpc.nlisten].type = RPC_LISTEN_TCP6;
			i++;
			if( i >= argc ) usage( NULL );
			rpc.listen[rpc.nlisten].addr.sin6.sin6_family = AF_INET6;
			rpc.listen[rpc.nlisten].addr.sin6.sin6_port = htons( atoi( argv[i] ) );
			rpc.nlisten++;
		}
		else if( strcmp( argv[i], "-u" ) == 0 ) {
			if( rpc.nlisten >= RPC_MAX_LISTEN ) usage( "Out of listen descriptors" );

			rpc.listen[rpc.nlisten].type = RPC_LISTEN_UDP;
			i++;
			if( i >= argc ) usage( NULL );
			rpc.listen[rpc.nlisten].addr.sin.sin_family = AF_INET;
			rpc.listen[rpc.nlisten].addr.sin.sin_port = htons( atoi( argv[i] ) );
			rpc.nlisten++;
		}
		else if( strcmp( argv[i], "-u6" ) == 0 ) {
			if( rpc.nlisten >= RPC_MAX_LISTEN ) usage( "Out of listen descriptors" );

			rpc.listen[rpc.nlisten].type = RPC_LISTEN_UDP6;
			i++;
			if( i >= argc ) usage( NULL );
			rpc.listen[rpc.nlisten].addr.sin6.sin6_family = AF_INET6;
			rpc.listen[rpc.nlisten].addr.sin6.sin6_port = htons( atoi( argv[i] ) );
			rpc.nlisten++;
#ifndef WIN32
		} else if( strcmp( argv[i], "-L" ) == 0 ) {
			if( rpc.nlisten >= RPC_MAX_LISTEN ) usage( "Out of listen descriptors" );

			rpc.listen[rpc.nlisten].type = RPC_LISTEN_UNIX;
			i++;
			if( i >= argc ) usage( NULL );
			rpc.listen[rpc.nlisten].addr.sun.sun_family = AF_UNIX;
			strncpy( rpc.listen[rpc.nlisten].addr.sun.sun_path, argv[i], sizeof(rpc.listen[rpc.nlisten].addr.sun.sun_path) );
			rpc.nlisten++;
#endif
		}
		else if( strcmp( argv[i], "-f" ) == 0 ) {
			rpc.foreground = 1;
#ifdef USE_SHAUTH
		} else if( strcmp( argv[i], "-s" ) == 0 ) {
			i++;
			if( i >= argc ) usage( NULL );
			rpc.shauth_secret = argv[i];
#endif
		}
		else usage( NULL );

		i++;
	}




	if( !rpc.foreground ) {
#ifdef WIN32
		SERVICE_TABLE_ENTRYA svctab[2];
		memset( svctab, 0, sizeof(svctab) );
		svctab[0].lpServiceName = "rpcd";
		svctab[0].lpServiceProc = rpcd_svc;
		StartServiceCtrlDispatcherA( svctab );
		return 0;
#else
		pid_t pid = fork();
		if( pid < 0 ) exit( 1 );
		if( pid != 0 ) exit( 0 );

		setsid();
		signal( SIGHUP, SIG_IGN );
		pid = fork();
		if( pid < 0 ) exit( 1 );
		if( pid != 0 ) exit( 0 );

		chdir( "/" );
		umask( 0 );
		close( STDIN_FILENO ); open( "/dev/null", O_RDONLY );
		close( STDOUT_FILENO ); open( "/dev/null", O_WRONLY );
		close( STDERR_FILENO ); open( "/dev/null", O_WRONLY );
#endif
	}

#ifndef WIN32
	sa.sa_sigaction = rpc_sig;
	sa.sa_flags = SA_SIGINFO;
	sigemptyset( &sa.sa_mask );
	sigaction( SIGTERM, &sa, NULL );
	sigaction( SIGINT, &sa, NULL );
	sigaction( SIGALRM, &sa, NULL );
	sigaction( SIGIO, &sa, NULL );
	sigaction( SIGUSR1, &sa, NULL );
	sigaction( SIGHUP, &sa, NULL );
	sigaction( SIGBUS, &sa, NULL );
	sigaction( SIGPIPE, &sa, NULL );
#endif

	rpc_run();

	return 0;
}


#ifdef WIN32
static void WINAPI rpcd_svc_ctrl( DWORD req ) {
	switch( req ) {
	case SERVICE_CONTROL_STOP:
	case SERVICE_CONTROL_SHUTDOWN:
		rpc.exiting = 1;
		rpc.svcsts.dwCurrentState = SERVICE_STOP_PENDING;
		SetServiceStatus( rpc.hsvc, &rpc.svcsts );
		break;
	default:
		SetServiceStatus( rpc.hsvc, &rpc.svcsts );
		break;
	}
}

static void WINAPI rpcd_svc( DWORD argc, char **argv ) {

	rpc.hsvc = RegisterServiceCtrlHandlerA( "rpcd", rpcd_svc_ctrl );

	rpc.svcsts.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
	rpc.svcsts.dwCurrentState = SERVICE_RUNNING;
	rpc.svcsts.dwControlsAccepted = SERVICE_ACCEPT_SHUTDOWN | SERVICE_ACCEPT_STOP;
	SetServiceStatus( rpc.hsvc, &rpc.svcsts );

	rpc_run();

	rpc.svcsts.dwCurrentState = SERVICE_STOPPED;
	SetServiceStatus( rpc.hsvc, &rpc.svcsts );
}
#endif




static void rpc_init_listen( void ) {
	int i, sts, j;
	struct rpc_conn *c;
	int prot_ports[16];

	/* setup connection table */
	for( i = 0; i < RPC_MAX_CONN; i++ ) {
		c = &rpc.conntab[i];
		memset( c, 0, sizeof(*c) );
		c->next = rpc.flist;
		c->buf = rpc.buftab[i];
		c->count = sizeof(rpc.buftab[i]);
		rpc.flist = c;
	}

	if( !rpc.nlisten ) {
		rpc_log( LOG_LVL_WARN, "Not listening on any ports" );
	}

	/* listen on ports */
	for( i = 0; i < rpc.nlisten; i++ ) {
		switch( rpc.listen[i].type ) {
		case RPC_LISTEN_TCP:
			rpc_log( LOG_LVL_INFO, "Listening on TCP port %d", (int)ntohs( rpc.listen[i].addr.sin.sin_port ) );

			rpc.listen[i].fd = socket( AF_INET, SOCK_STREAM, 0 );
			if( rpc.listen[i].fd < 0 ) usage( "Failed to open TCP socket: %s", rpc_strerror( rpc_errno() ) );

			sts = 1;
			setsockopt( rpc.listen[i].fd, SOL_SOCKET, SO_REUSEADDR, (char *)&sts, sizeof(sts) );

			sts = bind( rpc.listen[i].fd, (struct sockaddr *)&rpc.listen[i].addr.sin, sizeof(rpc.listen[i].addr.sin) );
			if( sts < 0 ) usage( "Failed to bind to TCP port %d: %s", ntohs( rpc.listen[i].addr.sin.sin_port ), rpc_strerror( rpc_errno() ) );

			sts = listen( rpc.listen[i].fd, SOMAXCONN );
			if( sts < 0 ) usage( "Failed to listen on TCP port %d: %s", ntohs( rpc.listen[i].addr.sin.sin_port ), rpc_strerror( rpc_errno() ) );

			break;

		case RPC_LISTEN_TCP6:
			rpc_log( LOG_LVL_INFO, "Listening on TCP6 port %d", (int)ntohs( rpc.listen[i].addr.sin6.sin6_port ) );

			rpc.listen[i].fd = socket( AF_INET6, SOCK_STREAM, 0 );
			if( rpc.listen[i].fd < 0 ) usage( "Failed to open TCP6 socket: %s", rpc_strerror( rpc_errno() ) );

			sts = 1;
			setsockopt( rpc.listen[i].fd, SOL_SOCKET, SO_REUSEADDR, (char *)&sts, sizeof(sts) );

			sts = bind( rpc.listen[i].fd, (struct sockaddr *)&rpc.listen[i].addr.sin6, sizeof(rpc.listen[i].addr.sin6) );
			if( sts < 0 ) usage( "Failed to bind to TCP6 port %d: %s", ntohs( rpc.listen[i].addr.sin6.sin6_port ), rpc_strerror( rpc_errno() ) );

			sts = listen( rpc.listen[i].fd, SOMAXCONN );
			if( sts < 0 ) usage( "Failed to listen on TCP6 port %d: %s", ntohs( rpc.listen[i].addr.sin6.sin6_port ), rpc_strerror( rpc_errno() ) );

			break;

#ifndef WIN32
		case RPC_LISTEN_UNIX:
			rpc_log( LOG_LVL_INFO, "Listening on UNIX path %s", rpc.listen[i].addr.sun.sun_path );

			rpc.listen[i].fd = socket( AF_UNIX, SOCK_STREAM, 0 );
			if( rpc.listen[i].fd < 0 ) usage( "Failed to open UNIX socket: %s", rpc_strerror( rpc_errno() ) );

			sts = 1;
			setsockopt( rpc.listen[i].fd, SOL_SOCKET, SO_REUSEADDR, (char *)&sts, sizeof(sts) );

			sts = bind( rpc.listen[i].fd, (struct sockaddr *)&rpc.listen[i].addr.sun, sizeof(rpc.listen[i].addr.sun) );
			if( sts < 0 ) usage( "Failed to bind to UNIX %s: %s", rpc.listen[i].addr.sun.sun_path, rpc_strerror( rpc_errno() ) );

			sts = listen( rpc.listen[i].fd, SOMAXCONN );
			if( sts < 0 ) usage( "Failed to listen on UNIX %s: %s", rpc.listen[i].addr.sun.sun_path, rpc_strerror( rpc_errno() ) );
			break;
#endif

		case RPC_LISTEN_UDP:
			rpc_log( LOG_LVL_INFO, "Listening on UDP port %d", (int)ntohs( rpc.listen[i].addr.sin.sin_port ) );

			rpc.listen[i].fd = socket( AF_INET, SOCK_DGRAM, 0 );
			if( rpc.listen[i].fd < 0 ) usage( "Failed to open UDP socket: %s", rpc_strerror( rpc_errno() ) );

			sts = 1;
			setsockopt( rpc.listen[i].fd, SOL_SOCKET, SO_REUSEADDR, (char *)&sts, sizeof(sts) );

			sts = bind( rpc.listen[i].fd, (struct sockaddr *)&rpc.listen[i].addr.sin, sizeof(rpc.listen[i].addr.sin) );
			if( sts < 0 ) usage( "Failed to bind to UDP port %d: %s", ntohs( rpc.listen[i].addr.sin.sin_port ), rpc_strerror( rpc_errno() ) );

			break;

		case RPC_LISTEN_UDP6:
			rpc_log( LOG_LVL_INFO, "Listening on UDP6 port %d", (int)ntohs( rpc.listen[i].addr.sin6.sin6_port ) );

			rpc.listen[i].fd = socket( AF_INET6, SOCK_DGRAM, 0 );
			if( rpc.listen[i].fd < 0 ) usage( "Failed to open UDP6 socket: %s", rpc_strerror( rpc_errno() ) );

			sts = 1;
			setsockopt( rpc.listen[i].fd, SOL_SOCKET, SO_REUSEADDR, (char *)&sts, sizeof(sts) );

			sts = bind( rpc.listen[i].fd, (struct sockaddr *)&rpc.listen[i].addr.sin6, sizeof(rpc.listen[i].addr.sin6) );
			if( sts < 0 ) usage( "Failed to bind to UDP6 port %d: %s", ntohs( rpc.listen[i].addr.sin6.sin6_port ), rpc_strerror( rpc_errno() ) );

			break;
		default:
			break;
		}
	}


	/* instruct local rpcbind program about our listen addresses */
	j = 0;
	for( i = 0; i < rpc.nlisten; i++ ) {
		prot_ports[j] = 0;
		if( ((rpc.listen[i].type == RPC_LISTEN_TCP) || (rpc.listen[i].type == RPC_LISTEN_TCP6)) ) prot_ports[j] = (IPPROTO_TCP << 16);
		else if( ((rpc.listen[i].type == RPC_LISTEN_UDP) || (rpc.listen[i].type == RPC_LISTEN_UDP6)) ) prot_ports[j] = (IPPROTO_UDP << 16);

		if( (rpc.listen[i].type == RPC_LISTEN_TCP) || (rpc.listen[i].type == RPC_LISTEN_UDP) ) prot_ports[j] |= ntohs( rpc.listen[i].addr.sin.sin_port );
		else if( (rpc.listen[i].type == RPC_LISTEN_TCP6) || (rpc.listen[i].type == RPC_LISTEN_UDP6) ) prot_ports[j] |= ntohs( rpc.listen[i].addr.sin6.sin6_port );

		if( prot_ports[j] ) j++;
	}
	rpcbind_set_laddrs( prot_ports, j );


}

#ifdef WIN32
#define RPC_POLLIN        0x0001
#define RPC_POLLOUT       0x0004
#define RPC_POLLERR       0x0008
#define RPC_POLLHUP       0x0010
#else
#define RPC_POLLIN        POLLIN
#define RPC_POLLOUT       POLLOUT
#define RPC_POLLERR       POLLERR
#define RPC_POLLHUP       POLLHUP
#endif

static void rpc_poll( int timeout ) {
	int i, sts;
	int npfd;
#ifdef WIN32
	WSANETWORKEVENTS events;
#else
	struct pollfd pfd[RPC_MAX_LISTEN + RPC_MAX_CONN];
#endif
	int revents[RPC_MAX_LISTEN + RPC_MAX_CONN];
	struct xdr_s tmpx;
	uint8_t tmpbuf[4];
	struct rpc_conn *c, *prev, *next;

#ifndef WIN32
	memset( pfd, 0, sizeof(pfd) );
#endif
	for( i = 0; i < RPC_MAX_LISTEN; i++ ) {
		if( i < rpc.nlisten ) {
#ifdef WIN32
			WSAEventSelect( rpc.listen[i].fd, rpc.evt, FD_READ );
#else
			pfd[i].fd = rpc.listen[i].fd;
			pfd[i].events = POLLIN;
			pfd[i].revents = 0;
#endif
		}
		else {
#ifndef WIN32
			pfd[i].fd = -1;
#endif
		}
	}

	i = RPC_MAX_LISTEN;
	c = rpc.clist;
	npfd = RPC_MAX_LISTEN;
	while( c ) {
#ifdef WIN32
		switch( c->nstate ) {
		case RPC_NSTATE_RECV:
			WSAEventSelect( c->fd, rpc.evt, FD_READ );
			break;
		case RPC_NSTATE_SEND:
			WSAEventSelect( c->fd, rpc.evt, FD_WRITE );
			break;
		case RPC_NSTATE_CONNECT:
			WSAEventSelect( c->fd, rpc.evt, FD_CONNECT );
			break;
		default:
			break;
		}
#else
		pfd[i].fd = c->fd;

		switch( c->nstate ) {
		case RPC_NSTATE_RECV:
			pfd[i].events = POLLIN;
			break;
		case RPC_NSTATE_SEND:
			pfd[i].events = POLLOUT;
			break;
		case RPC_NSTATE_CONNECT:
			pfd[i].events = POLLOUT;
			break;
		default:
			break;
		}

		pfd[i].revents = 0;
#endif

		c = c->next;
		i++;
		npfd++;
	}

#ifdef WIN32
	sts = WSAWaitForMultipleEvents( 1, &rpc.evt, FALSE, timeout, FALSE );
	if( sts == WSA_WAIT_TIMEOUT ) return;
	if( sts != WSA_WAIT_EVENT_0 ) return;
#else
	sts = poll( pfd, npfd, timeout );
	if( sts <= 0 ) return;
#endif

	memset( revents, 0, sizeof(revents) );
#ifdef WIN32
	for( i = 0; i < rpc.nlisten; i++ ) {
		int j;

		WSAEnumNetworkEvents( rpc.listen[i].fd, rpc.evt, &events );
		if( events.lNetworkEvents & FD_READ ) revents[i] |= RPC_POLLIN;
		if( events.lNetworkEvents & FD_WRITE ) revents[i] |= RPC_POLLOUT;
		if( events.lNetworkEvents & FD_ACCEPT ) revents[i] |= RPC_POLLIN;
		if( events.lNetworkEvents & FD_CONNECT ) revents[i] |= RPC_POLLOUT;
		if( events.lNetworkEvents & FD_CLOSE ) revents[i] |= RPC_POLLHUP;
		for( j = 0; j < 6; j++ ) {
			if( events.iErrorCode[j] ) revents[i] |= RPC_POLLERR;
		}
	}
	i = RPC_MAX_LISTEN;
	c = rpc.clist;
	while( c ) {
		WSAEnumNetworkEvents( c->fd, rpc.evt, &events );
		revents[i] = events.lNetworkEvents;
		c = c->next;
		i++;
	}
#else
	for( i = 0; i < npfd; i++ ) {
		revents[i] = pfd[i].revents;
	}
#endif

	/* process connections first */
	i = RPC_MAX_LISTEN;
	c = rpc.clist;
	while( c ) {
		if( (revents[i] & RPC_POLLERR) || (revents[i] & RPC_POLLHUP) ) {
			c->cstate = RPC_CSTATE_CLOSE;
		}
		else if( revents[i] & RPC_POLLIN ) {

			switch( c->cstate ) {
			case RPC_CSTATE_RECVLEN:
				sts = recv( c->fd, c->buf, 4, 0 );
				if( sts < 0 ) {
#ifdef WIN32
					if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
					if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
					c->cstate = RPC_CSTATE_CLOSE;
					break;
				}
				else if( sts == 0 ) {
					c->cstate = RPC_CSTATE_CLOSE;
				}
				else {
					xdr_init( &tmpx, c->buf, 4 );
					xdr_decode_uint32( &tmpx, &c->cdata.count );
					if( !(c->cdata.count & 0x80000000) ) {
						c->cstate = RPC_CSTATE_CLOSE;
						break;
					}

					c->cdata.count &= ~0x80000000;
					if( c->cdata.count > RPC_MAX_BUF ) {
						c->cstate = RPC_CSTATE_CLOSE;
						break;
					}

					c->cdata.offset = 0;
					c->cstate = RPC_CSTATE_RECV;
				}
				break;
			case RPC_CSTATE_RECV:
				sts = recv( c->fd, c->buf + c->cdata.offset, RPC_MAX_BUF - c->cdata.offset, 0 );
				if( sts < 0 ) {
#ifdef WIN32
					if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
					if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
					c->cstate = RPC_CSTATE_CLOSE;
					break;
				}
				else if( sts == 0 ) {
					c->cstate = RPC_CSTATE_CLOSE;
					break;
				}

				c->cdata.offset += sts;
				if( c->cdata.offset == c->cdata.count ) {
					/* msg complete - process */
					xdr_init( &c->inc.xdr, c->buf, RPC_MAX_BUF );
					c->inc.xdr.count = c->cdata.count;
					sts = rpc_process_incoming( &c->inc );
					if( sts == 0 ) {

						c->cdata.count = c->inc.xdr.offset;
						c->cdata.offset = 0;
						c->cstate = RPC_CSTATE_SENDLEN;
						c->nstate = RPC_NSTATE_SEND;

						/* optimistically send count */
						xdr_init( &tmpx, tmpbuf, 4 );
						xdr_encode_uint32( &tmpx, c->cdata.count | 0x80000000 );

						sts = send( c->fd, tmpbuf, 4, 0 );
						if( sts < 0 ) {
#ifdef WIN32
							if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
							if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
							c->cstate = RPC_CSTATE_CLOSE;
							break;
						}

						c->cstate = RPC_CSTATE_SEND;
						sts = send( c->fd, c->buf + c->cdata.offset, c->cdata.count - c->cdata.offset, 0 );
						if( sts < 0 ) {
#ifdef WIN32
							if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
							if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
							c->cstate = RPC_CSTATE_CLOSE;
							break;
						}

						c->cdata.offset += sts;
						if( c->cdata.offset == c->cdata.count ) {
							c->nstate = RPC_NSTATE_RECV;
							c->cstate = RPC_CSTATE_RECVLEN;
						}
					}

				}

				break;
			default:
				break;
			}

		}
		else if( revents[i] & RPC_POLLOUT ) {

			switch( c->cstate ) {
			case RPC_CSTATE_SENDLEN:
				xdr_init( &tmpx, tmpbuf, 4 );
				xdr_encode_uint32( &tmpx, c->cdata.count | 0x80000000 );
				sts = send( c->fd, tmpbuf, 4, 0 );
				if( sts < 0 ) {
#ifdef WIN32
					if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
					if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
					c->cstate = RPC_CSTATE_CLOSE;
					break;
				}
				c->cstate = RPC_CSTATE_SEND;

				/* fall through */
			case RPC_CSTATE_SEND:
				sts = send( c->fd, c->buf + c->cdata.offset, c->cdata.count - c->cdata.offset, 0 );
				if( sts < 0 ) {
#ifdef WIN32
					if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
					if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
					c->cstate = RPC_CSTATE_CLOSE;
					break;
				}

				c->cdata.offset += sts;
				if( c->cdata.offset == c->cdata.count ) {
					c->nstate = RPC_NSTATE_RECV;
					c->cstate = RPC_CSTATE_RECVLEN;
				}
				break;
			case RPC_CSTATE_CONNECT:
				/* non-blocking connect completed */
			{
									   socklen_t slen;
									   sts = 0;
									   slen = sizeof(sts);
									   getsockopt( c->fd, SOL_SOCKET, SO_ERROR, (char *)&sts, &slen );
									   if( sts ) {
										   rpc_log( LOG_LVL_ERROR, "Connect failed: %s", rpc_strerror( rpc_errno() ) );
										   c->cstate = RPC_CSTATE_CLOSE;
									   }
									   else {
										   c->nstate = RPC_NSTATE_RECV;
										   c->cstate = RPC_CSTATE_RECVLEN;
									   }

									   if( c->cdata.cb ) c->cdata.cb( c );
			}
				break;
			default:
				break;
			}

		}

		c = c->next;
		i++;
	}

	for( i = 0; i < rpc.nlisten; i++ ) {
		if( revents[i] & RPC_POLLIN ) {
			/* ready to accept */
			rpc_accept( &rpc.listen[i] );
		}
	}

	/* Close pending connections */
	c = rpc.clist;
	prev = NULL;
	while( c ) {
		next = c->next;

		if( c->cstate == RPC_CSTATE_CLOSE ) {
#ifdef WIN32
			closesocket( c->fd );
#else
			close( c->fd );
#endif
			/* invoke callback if required */
			if( c->cdata.cb ) c->cdata.cb( c );

			if( prev ) prev->next = c->next;
			else rpc.clist = c->next;

			c->next = rpc.flist;
			rpc.flist = c;

			c = next;
		}
		else {
			prev = c;
			c = c->next;
		}
	}


}

static void rpc_accept( struct rpc_listen *lis ) {
	struct rpc_conn *c;
	int sts;

	switch( lis->type ) {
	case RPC_LISTEN_UDP:
	case RPC_LISTEN_UDP6:
	{
							socklen_t slen;

							rpc_log( LOG_LVL_INFO, "Accept UDP" );

							c = rpc.flist;
							if( !c ) return;

							/* setup inc */
							memset( &c->inc, 0, sizeof(c->inc) );

							c->inc.raddr_len = sizeof(c->inc.raddr);
							slen = c->inc.raddr_len;
							c->cdata.count = recvfrom( lis->fd, c->buf, RPC_MAX_BUF, 0, (struct sockaddr *)&c->inc.raddr, &slen );
							if( c->cdata.count < 0 ) return;
							c->inc.raddr_len = slen;

							memcpy( &c->inc.laddr,
								&lis->addr,
								lis->type == RPC_LISTEN_UDP ? sizeof(lis->addr.sin) : sizeof(lis->addr.sin6) );
							xdr_init( &c->inc.xdr, c->buf, RPC_MAX_BUF );
							c->inc.xdr.count = c->cdata.count;

							/* process message and send reply */
							rpc_log( LOG_LVL_INFO, "Process UDP call" );
							sts = rpc_process_incoming( &c->inc );
							if( sts == 0 ) {
								sts = sendto( lis->fd, c->buf, c->inc.xdr.offset, 0, (struct sockaddr *)&c->inc.raddr, c->inc.raddr_len );
								if( sts < 0 ) rpc_log( LOG_LVL_ERROR, "sendto: %s", rpc_strerror( rpc_errno() ) );
							}
							else {
								rpc_log( LOG_LVL_INFO, "rpc_process_incoming failed" );
							}

	}
		break;
	case RPC_LISTEN_TCP:
	case RPC_LISTEN_TCP6:
#ifndef WIN32
	case RPC_LISTEN_UNIX:
	{
							rpc_log( LOG_LVL_INFO, "Accept TCP/UNIX" );

							/* get connection descriptor */
							c = rpc.flist;
							if( !c ) return;
							rpc.flist = c->next;

							c->inc.raddr_len = sizeof(c->inc.raddr);
							c->fd = accept( lis->fd, (struct sockaddr *)&c->inc.raddr, &c->inc.raddr_len );
							c->cstate = RPC_CSTATE_RECVLEN;
							c->nstate = RPC_NSTATE_RECV;

							sts = fcntl( c->fd, F_GETFL, 0 );
							fcntl( c->fd, F_SETFL, sts|O_NONBLOCK );

							c->next = rpc.clist;
							rpc.clist = c;
	}
#endif
		break;
	}
}

static void rpc_run( void ) {
	struct rpc_conn *c;
	int i;
	int timeout;
	struct rpc_program *pglist;
	struct rpc_version *vlist;
	struct sockaddr_in sin;
	struct rpcbind_mapping map;

#ifdef WIN32
	{
		WSADATA wsadata;
		WSAStartup( MAKEWORD( 2, 2 ), &wsadata );
		rpc.evt = WSACreateEvent();
	}
#endif

	/* register programs */
	rpcbind_register();

	memset( &sin, 0, sizeof(sin) );
	sin.sin_family = AF_INET;
	sin.sin_port = htons( 111 );
	sin.sin_addr.s_addr = htonl( INADDR_LOOPBACK );

	pglist = rpc_program_list();
	while( pglist ) {
		map.prog = pglist->prog;
		vlist = pglist->vers;
		while( vlist ) {
			map.vers = vlist->vers;
			for( i = 0; i < rpc.nlisten; i++ ) {
				map.prot = IPPROTO_UDP;
				switch( rpc.listen[i].type ) {
				case RPC_LISTEN_UDP:
					map.port = ntohs( rpc.listen[i].addr.sin.sin_port );
				case RPC_LISTEN_UDP6:
					map.port = ntohs( rpc.listen[i].addr.sin6.sin6_port );
					map.prot = IPPROTO_UDP;
					break;
				case RPC_LISTEN_TCP:
					map.port = ntohs( rpc.listen[i].addr.sin.sin_port );
				case RPC_LISTEN_TCP6:
					map.port = ntohs( rpc.listen[i].addr.sin6.sin6_port );
					map.prot = IPPROTO_TCP;
					break;
				default:
					break;
				}

				rpcbind_call_set( &sin, &map );
			}

			vlist = vlist->next;
		}

		pglist = pglist->next;
	}

	/* register providers */
#ifdef USE_SHAUTH
	{
		uint8_t key[32];
		memset( key, 0, sizeof(key) );
		if( rpc.shauth_secret ) {
			/* must look like a hex string */
			char *p, *terminator;
			char tmp[4];
			p = rpc.shauth_secret;
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
				if( *terminator ) usage( "Failed to parse secret" );

				if( !*p ) break;
			}
		}
		shauth_register( key );
	}
#endif

	/* listen on ports */
	rpc_init_listen();

	/* poll loop */
	while( !rpc.exiting ) {
		/* compute timeout */
		timeout = rpc_iterator_timeout();
		if( timeout > 1000 ) timeout = 1000;
		if( timeout < 0 ) timeout = 1000;

		/* poll networking */
		rpc_poll( timeout );

		/* service interators */
		rpc_iterator_service();

		/* service waiters */
		rpc_waiter_service();
	}

	/* done */
	c = rpc.clist;
	while( c ) {
#ifdef WIN32
		closesocket( c->fd );
#else
		close( c->fd );
#endif
		c = c->next;
	}

	for( i = 0; i < rpc.nlisten; i++ ) {
#ifdef WIN32
		closesocket( rpc.listen[i].fd );
#else
		close( rpc.listen[i].fd );
#endif
	}

}

int rpc_connect( struct sockaddr *addr, socklen_t alen, void( *cb )(struct rpc_conn *c), void *cxt ) {
	struct rpc_conn *c;
	int sts;

	/* start by allocating a connection descriptor */
	c = rpc.flist;
	if( !c ) return -1;
	rpc.flist = c->next;


	c->fd = socket( addr->sa_family, SOCK_STREAM, 0 );
	if( c->fd < 0 ) goto failure;

	/* set non-blocking */
#ifdef WIN32
	{
		u_long nb = 1;
		ioctlsocket( c->fd, FIONBIO, &nb );
	}
#else
	sts = fcntl( c->fd, F_GETFL, 0 );
	fcntl( c->fd, F_SETFL, sts|O_NONBLOCK );
#endif
	c->cdata.cb = cb;
	c->cdata.cxt = cxt;

	sts = connect( c->fd, addr, alen );
	if( sts < 0 ) {
#ifdef WIN32
		if( WSAGetLastError() != WSAEWOULDBLOCK ) {
			closesocket( c->fd );
			goto failure;
		}
#else
		if( errno != EWOULDBLOCK ) {
			close( c->fd );
			goto failure;
		}
#endif
		c->cstate = RPC_CSTATE_CONNECT;
		c->nstate = RPC_NSTATE_CONNECT;
	}
	else {
		c->cstate = RPC_CSTATE_RECVLEN;
		c->nstate = RPC_NSTATE_RECV;

		if( c->cdata.cb ) c->cdata.cb( c );
	}

	c->next = rpc.clist;
	rpc.clist = c;

	return 0;

failure:
	c->next = rpc.flist;
	rpc.flist = c;
	return -1;
}
