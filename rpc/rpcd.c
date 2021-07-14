
/*
 * This file defines an example rpc daemon which can listen on a set of TCP/UDP ports or
 * UNIX file sockets. Essentially all this file does is implement a standard file descriptor
 * polling loop, waiting for things to happen. When it has buffered a complete message
 * it invokes rpc_process_incoming (from the library rpc.c) which will process
 * the message and invoke the relevant procedure, if available. The receive buffer
 * is filled out with the reply data and should then be used to send back to the client.
 */

#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#include <WinSock2.h>
#include <Windows.h>
#endif

#include <fju/rpcd.h>
#include "rpc-private.h"
#include <fju/freg.h>
#include <fju/mmf.h>
#include <inttypes.h>

#define RPCD_MAX_IOCB 256

#ifdef WIN32

struct rpcd_iocb {
  struct rpcd_iocb *next;

  OVERLAPPED overlap;
  struct mmf_s *mmf;
  void (*donecb)();
  void *prv;
  char *buf;
  int len;
  
};

#else
#include <dlfcn.h>
#endif

#ifdef __FreeBSD__
#include <sys/event.h>
#include <sys/aio.h>

struct rpcd_iocb {
  struct rpcd_iocb *next;
  
  struct mmf_s *mmf;
  struct aiocb aiocb;
  void (*donecb)();
  void *prv;
  char *buf;
  int len;
};
#endif

#ifdef __linux__
#include <libaio.h>
#include <sys/eventfd.h>

struct rpcd_iocb {
  struct rpcd_iocb *next;
  
  struct mmf_s *mmf;
  struct iocb aiocb;
  void (*donecb)();
  void *prv;
  char *buf;
  int len;
};
#endif

static struct {
	int foreground;
	int no_rpcregister;
	int quiet;
        int rpcdp;
	volatile int exiting;
	char *pidfile;
  
	struct rpc_listen listen[RPC_MAX_LISTEN];   /* listening fds */
	int nlisten;

	struct rpc_conn *clist;    /* active connection list */
	struct rpc_conn *flist;    /* free list */

        rpcd_main_t main_cb;
        void *main_cxt;
        struct rpc_logger loggers[1];

        
#ifdef WIN32
	HANDLE evt;
	SERVICE_STATUS_HANDLE hsvc;
	SERVICE_STATUS svcsts;
#endif

	struct rpc_conn conntab[RPC_MAX_CONN];
	uint8_t buftab[RPC_MAX_CONN][RPC_MAX_BUF];

	uint64_t connid;
  
        struct rpcd_active_conn aconn;
#ifdef __FreeBSD__
  int kq;
#endif
#ifdef __linux__
  io_context_t iocxt;
  int eventfd;
#endif
  
  struct rpcd_iocb iocb[RPCD_MAX_IOCB];
  struct rpcd_iocb *iocbfl;
} rpc;

static struct rpcd_iocb *rpcd_iocb_alloc( void ) {
  struct rpcd_iocb *iocb;
  
  if( !rpc.iocbfl ) return NULL;
  iocb = rpc.iocbfl;
  rpc.iocbfl = iocb->next;
  memset( iocb, 0, sizeof(*iocb) );
  return iocb;
}

static void rpcd_iocb_free( struct rpcd_iocb *iocb ) {
  iocb->next = rpc.iocbfl;
  rpc.iocbfl = iocb;
}

#ifdef WIN32
typedef int socklen_t;
static void WINAPI rpcd_svc( DWORD argc, char **argv );
#endif

static void rpcd_run( void );
static void rpc_accept( struct rpc_listen *lis );
static void rpc_close_connections( void );
static char *mynet_ntop( rpc_listen_t type, struct sockaddr *addr, int alen, char *ipstr );
static void rpcd_load_service( char *svcname, char *path, char *mainfn );
static void rpcd_load_services( void );

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

	if( rpc.main_cb ) {
	  int snum = sig;
	  rpc.main_cb( RPCD_EVT_SIGNAL, (void *)&snum, rpc.main_cxt );
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
		"            [-f] [-R] [-q]\n"
		"\n"
		"  Where:\n"
		"            -f               Run in foreground\n"
		"            -u -u6           Listen on IP/UDP or IPv6/UDP\n"
		"            -t -t6           Listen on IP/TCP or IPv6/TCP\n"
#ifndef WIN32
		"            -L path          Listen on AF_UNIX socket file\n"
#endif
		"            -R               Register with rpcbind service.\n"
		"            -q               Quiet. Don't log to stdout\n"
#ifndef WIN32
		"            -p pidfile       Write a pidfile here\n"
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

int rpcd_listen_udp( int port ) {
  if( rpc.nlisten >= RPC_MAX_LISTEN ) return -1;

  rpc.listen[rpc.nlisten].type = RPC_LISTEN_UDP;
  rpc.listen[rpc.nlisten].addr.sin.sin_family = AF_INET;
  rpc.listen[rpc.nlisten].addr.sin.sin_port = htons( port );
  rpc.nlisten++;
  
  return 0;
}

int rpcd_listen_tcp( int port ) {
  if( rpc.nlisten >= RPC_MAX_LISTEN ) return -1;

  rpc.listen[rpc.nlisten].type = RPC_LISTEN_TCP;
  rpc.listen[rpc.nlisten].addr.sin.sin_family = AF_INET;
  rpc.listen[rpc.nlisten].addr.sin.sin_port = htons( port );
  rpc.nlisten++;

  return 0;
}

int rpcd_listen_udp6( int port ) {
  if( rpc.nlisten >= RPC_MAX_LISTEN ) return -1;

  rpc.listen[rpc.nlisten].type = RPC_LISTEN_UDP6;
  rpc.listen[rpc.nlisten].addr.sin6.sin6_family = AF_INET6;
  rpc.listen[rpc.nlisten].addr.sin6.sin6_port = htons( port );
  rpc.nlisten++;
  
  return 0;
}

int rpcd_listen_tcp6( int port ) {
  if( rpc.nlisten >= RPC_MAX_LISTEN ) return -1;

  rpc.listen[rpc.nlisten].type = RPC_LISTEN_TCP6;
  rpc.listen[rpc.nlisten].addr.sin6.sin6_family = AF_INET6;
  rpc.listen[rpc.nlisten].addr.sin6.sin6_port = htons( port );
  rpc.nlisten++;
  
  return 0;
}

#ifndef WIN32
int rpcd_listen_unix( char *path ) {
  if( rpc.nlisten >= RPC_MAX_LISTEN ) return -1;
  
  rpc.listen[rpc.nlisten].type = RPC_LISTEN_UNIX;
  rpc.listen[rpc.nlisten].addr.sun.sun_family = AF_UNIX;
  strncpy( rpc.listen[rpc.nlisten].addr.sun.sun_path, path, sizeof(rpc.listen[rpc.nlisten].addr.sun.sun_path) );
  rpc.nlisten++;
  
  return 0;
}
#endif

static void parse_args( int argc, char **argv ) {
    int i;
    i = 1;
    while( i < argc ) {
	if( strcmp( argv[i], "-t" ) == 0 ) {
	  i++;
	  if( i >= argc ) usage( NULL );
	  if( rpcd_listen_tcp( atoi( argv[i] ) ) ) usage( "Out of listen descriptors" );
	}
	else if( strcmp( argv[i], "-t6" ) == 0 ) {
	  i++;
	  if( i >= argc ) usage( NULL );
	  if( rpcd_listen_tcp6( atoi( argv[i] ) ) ) usage( "Out of listen descriptors" );
	}
	else if( strcmp( argv[i], "-u" ) == 0 ) {
	  i++;
	  if( i >= argc ) usage( NULL );
	  if( rpcd_listen_udp( atoi( argv[i] ) ) ) usage( "Out of listen descriptors" );
	}
	else if( strcmp( argv[i], "-u6" ) == 0 ) {
	  i++;
	  if( i >= argc ) usage( NULL );
	  if( rpcd_listen_udp6( atoi( argv[i] ) ) ) usage( "Out of listen descriptors" );
#ifndef WIN32
	} else if( strcmp( argv[i], "-L" ) == 0 ) {
	  i++;
	  if( i >= argc ) usage( NULL );
	  if( rpcd_listen_unix( argv[i] ) ) usage( "Out of listen descriptors" );
#endif
	}
	else if( strcmp( argv[i], "-f" ) == 0 ) {
	    rpc.foreground = 1;
	} else if( strcmp( argv[i], "-R" ) == 0 ) {
	    rpc.no_rpcregister = 0;
	} else if( strcmp( argv[i], "-q" ) == 0 ) {
	    rpc.quiet = 1;			
	} else if( strcmp( argv[i], "-p" ) == 0 ) {
	    i++;
	    if( i >= argc ) usage( NULL );
	    rpc.pidfile = argv[i];
	}
	else usage( NULL );

	i++;
    }
    
}

int rpcd_main( int argc, char **argv, rpcd_main_t main_cb, void *main_cxt ) {
#ifndef WIN32
	struct sigaction sa;
#endif	
	freg_open( NULL, NULL );
	
	rpc.main_cb = main_cb;
	rpc.main_cxt = main_cxt;
	rpc.no_rpcregister = 1;
	
	/* parse command line */
	parse_args( argc, argv );

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

		/* write pid file */
		if( rpc.pidfile ) {
		  int pidfd;
		  char pidstr[64];
		  pidfd = open( rpc.pidfile, O_RDWR|O_CREAT, 0600 );
		  if( pidfd != -1 ) {
		    sprintf( pidstr, "%d", getpid() );
		    write( pidfd, pidstr, strlen( pidstr ) + 1 );
		    close( pidfd );
		  }
		}
#endif
	} else if( !rpc.quiet ) {
	  /* if running in foreground and not quiet, add default stdout logger */
	  rpc_add_logger( &rpc.loggers[0] );
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
	//sigaction( SIGBUS, &sa, NULL );
	sigaction( SIGPIPE, &sa, NULL );
#endif

	rpcd_run();


#ifndef WIN32
	if( rpc.pidfile ) {
	  unlink( rpc.pidfile );
	}
#endif

	return 0;
}

int rpcdp( void ) {
    return rpc.rpcdp;
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

	parse_args( argc, argv );
	
	rpcd_run();

	rpc.svcsts.dwCurrentState = SERVICE_STOPPED;
	SetServiceStatus( rpc.hsvc, &rpc.svcsts );
}
#endif



int rpcd_get_default_port( void ) {
    int sts;
    uint32_t port;
    sts = freg_get_by_name( NULL, 0,
			    "/fju/rpc/port", FREG_TYPE_UINT32,
			    (char *)&port, sizeof(port), NULL );
    return sts ? 0 : port;
}

static void rpc_init_listen( void ) {
	int i, sts, j;
	int prot_ports[16];

	if( !rpc.nlisten ) {
	    uint32_t port = rpcd_get_default_port();
	    if( port ) {
		rpc.listen[0].type = RPC_LISTEN_UDP;
		rpc.listen[0].addr.sin.sin_family = AF_INET;
		rpc.listen[0].addr.sin.sin_port = htons( port );
		rpc.listen[1].type = RPC_LISTEN_TCP;
		rpc.listen[1].addr.sin.sin_family = AF_INET;
		rpc.listen[1].addr.sin.sin_port = htons( port );
		rpc.nlisten = 2;
	    } else {	    
		rpc_log( RPC_LOG_WARN, "Not listening on any ports" );
	    }
	}

	/* listen on ports */
	for( i = 0; i < rpc.nlisten; i++ ) {
		switch( rpc.listen[i].type ) {
		case RPC_LISTEN_TCP:
			rpc_log( RPC_LOG_INFO, "Listening on TCP port %d", (int)ntohs( rpc.listen[i].addr.sin.sin_port ) );

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
			rpc_log( RPC_LOG_INFO, "Listening on TCP6 port %d", (int)ntohs( rpc.listen[i].addr.sin6.sin6_port ) );

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
			rpc_log( RPC_LOG_INFO, "Listening on UNIX path %s", rpc.listen[i].addr.sun.sun_path );

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
			rpc_log( RPC_LOG_INFO, "Listening on UDP port %d", (int)ntohs( rpc.listen[i].addr.sin.sin_port ) );

			rpc.listen[i].fd = socket( AF_INET, SOCK_DGRAM, 0 );
			if( rpc.listen[i].fd < 0 ) usage( "Failed to open UDP socket: %s", rpc_strerror( rpc_errno() ) );

			sts = 1;
			setsockopt( rpc.listen[i].fd, SOL_SOCKET, SO_REUSEADDR, (char *)&sts, sizeof(sts) );

			sts = bind( rpc.listen[i].fd, (struct sockaddr *)&rpc.listen[i].addr.sin, sizeof(rpc.listen[i].addr.sin) );
			if( sts < 0 ) usage( "Failed to bind to UDP port %d: %s", ntohs( rpc.listen[i].addr.sin.sin_port ), rpc_strerror( rpc_errno() ) );

			break;

		case RPC_LISTEN_UDP6:
			rpc_log( RPC_LOG_INFO, "Listening on UDP6 port %d", (int)ntohs( rpc.listen[i].addr.sin6.sin6_port ) );

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





#ifdef __FreeBSD__

/*
 * We need to use kqueue on bsd systems so that we can suppor aio.
 * Linux we can use eventfd and poll that using standard poll() mechanism
 * Win32 we can use overlapped IO (and WriteFileEx etc) so no need to change there
 */

static int bsd_poll( struct pollfd *pfd, int npfd, int timeout ) {

  int sts, i, ni, no;
  struct timespec ts;
  struct kevent ievent[RPC_MAX_LISTEN + RPC_MAX_CONN], oevent[RPC_MAX_LISTEN + RPC_MAX_CONN];

  if( rpc.kq == 0 ) {
    rpc.kq = kqueue();
  }
  
  /* set events for all connections */
  ni = 0;
  for( i = 0; i < npfd; i++ ) {
    if( pfd[i].fd == -1 ) continue;
    
    if( pfd[i].events & POLLIN ) {
      EV_SET( &ievent[ni], pfd[i].fd, EVFILT_READ, EV_ADD|EV_CLEAR|EV_ENABLE, 0, 0, (void *)(long)i );
      ni++;
    } else if( pfd[i].events & POLLOUT ) {
      EV_SET( &ievent[ni], pfd[i].fd, EVFILT_WRITE, EV_ADD|EV_CLEAR|EV_ENABLE, 0, 0, (void *)(long)i );
      ni++;
    }
  }

  memset( oevent, 0, sizeof(oevent) );
  ts.tv_sec = timeout / 1000;
  ts.tv_nsec = (timeout % 1000) * 1000000;
  sts = kevent( rpc.kq, ievent, ni, oevent, RPC_MAX_LISTEN + RPC_MAX_CONN, &ts );
  if( sts < 0 ) {
    rpc_log( RPC_LOG_ERROR, "kevent error %s", strerror( errno ) );
    return -1;
  }
  if( sts == 0 ) {
    //rpc_log( RPC_LOG_INFO, "kevent timeout" );
    return 0;
  }

  for( no = 0; no < sts; no++ ) {
    i = (int)oevent[no].udata;

    /* translate to POLLIN/POLLOUT flags */
    switch( oevent[no].filter ) {
    case EVFILT_READ:
      pfd[i].revents = POLLIN | (oevent[no].flags & EV_EOF ? POLLHUP : 0) | (oevent[no].flags & EV_ERROR ? POLLERR : 0);
      break;
    case EVFILT_WRITE:
      pfd[i].revents = POLLOUT | (oevent[no].flags & EV_EOF ? POLLHUP : 0) | (oevent[no].flags & EV_ERROR ? POLLERR : 0);      
      break;
    case EVFILT_AIO:
      rpc_log( RPC_LOG_INFO, "kevent aio" );
      {
	struct rpcd_iocb *iocb;
	rpcd_aio_donecb donecb;

	iocb = (struct rpcd_iocb *)oevent[no].udata;
	donecb = (rpcd_aio_donecb)iocb->donecb;
	if( donecb ) donecb( iocb->mmf, (char *)iocb->aiocb.aio_buf, aio_return( &iocb->aiocb ), iocb->prv );
	rpcd_iocb_free( iocb );
      }      
      break;
    default:
      rpc_log( RPC_LOG_INFO, "kevent filter %u", oevent[no].filter );
      break;
    }
  }

  return sts;
}

#endif /* freebsd */


void rpc_poll( int timeout ) {
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
	struct rpc_conn *c;
#ifdef __linux__
	int eventfdidx;
#endif
       
#ifndef WIN32
	memset( pfd, 0, sizeof(pfd) );
#endif
	for( i = 0; i < RPC_MAX_LISTEN; i++ ) {
		if( i < rpc.nlisten ) {
#ifdef WIN32
			WSAEventSelect( rpc.listen[i].fd,
					rpc.evt,
					((rpc.listen[i].type == RPC_LISTEN_TCP) || (rpc.listen[i].type == RPC_LISTEN_TCP6)) ?
					FD_ACCEPT :
					FD_READ );
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
			WSAEventSelect( c->fd, rpc.evt, FD_READ|FD_CLOSE );
			break;
		case RPC_NSTATE_SEND:
			WSAEventSelect( c->fd, rpc.evt, FD_WRITE|FD_CLOSE );
			break;
		case RPC_NSTATE_CONNECT:
			WSAEventSelect( c->fd, rpc.evt, FD_CONNECT|FD_CLOSE );
			break;
		default:
		        WSAEventSelect( c->fd, rpc.evt, FD_READ|FD_CLOSE );
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

#ifdef __linux__
	pfd[i].fd = rpc.eventfd;
	pfd[i].events = POLLIN;
	pfd[i].revents = 0;
	npfd++;
	eventfdidx = i;
#endif
	
#ifdef WIN32
	/* Wait for something to happen - note we set bAlertable to true so that IO completion routines may run */
	sts = WSAWaitForMultipleEvents( 1, &rpc.evt, FALSE, timeout, TRUE );
	if( sts == WSA_WAIT_TIMEOUT ) {
	    return;
	}
	if( sts != WSA_WAIT_EVENT_0 ) {
	    return;
	}
#else
	/* not windows */
#ifdef __FreeBSD__
	sts = bsd_poll( pfd, npfd, timeout );
#else
	sts = poll( pfd, npfd, timeout );
#endif
	if( sts <= 0 ) return;
#endif

	memset( revents, 0, sizeof(revents) );
#ifdef WIN32
	for( i = 0; i < rpc.nlisten; i++ ) {
		int j;

		memset( &events, 0, sizeof(events) );
		WSAEnumNetworkEvents( rpc.listen[i].fd, rpc.evt, &events );
		if( events.lNetworkEvents & FD_READ ) revents[i] |= RPC_POLLIN;
		if( events.lNetworkEvents & FD_WRITE ) revents[i] |= RPC_POLLOUT;
		if( events.lNetworkEvents & FD_ACCEPT ) revents[i] |= RPC_POLLIN;
		if( events.lNetworkEvents & FD_CONNECT ) revents[i] |= RPC_POLLOUT;
		if( events.lNetworkEvents & FD_CLOSE ) revents[i] |= RPC_POLLHUP;
		for( j = 0; j < 6; j++ ) {
		    if( events.iErrorCode[j] ) {
			revents[i] |= RPC_POLLERR;
		    }
		}

	}
	i = RPC_MAX_LISTEN;
	c = rpc.clist;
	while( c ) {
    	        int j;

		memset( &events, 0, sizeof(events) );
		WSAEnumNetworkEvents( c->fd, rpc.evt, &events );
		if( events.lNetworkEvents & FD_READ ) revents[i] |= RPC_POLLIN;
		if( events.lNetworkEvents & FD_WRITE ) revents[i] |= RPC_POLLOUT;
		if( events.lNetworkEvents & FD_ACCEPT ) revents[i] |= RPC_POLLIN;
		if( events.lNetworkEvents & FD_CONNECT ) revents[i] |= RPC_POLLOUT;
		if( events.lNetworkEvents & FD_CLOSE ) revents[i] |= RPC_POLLHUP;
		for( j = 0; j < 6; j++ ) {
			if( events.iErrorCode[j] ) revents[i] |= RPC_POLLERR;
		}

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
		  rpc_log( RPC_LOG_DEBUG, "Connection connid=%"PRIx64" %s", c->connid,
			   (revents[i] & RPC_POLLERR) ? "POLLERR" : "POLLHUP" );

		  rpc_conn_close( c );
		}
		
		else if( revents[i] & RPC_POLLIN ) {
			c->timestamp = rpc_now();

			switch( c->cstate ) {
			case RPC_CSTATE_RECVLEN:
				sts = recv( c->fd, c->buf, 4, 0 );
				if( sts < 0 ) {
#ifdef WIN32
					if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
					if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
					rpc_log( RPC_LOG_DEBUG, "connid=%"PRIx64" recv: %s", c->connid, rpc_strerror( rpc_errno() ) );
					rpc_conn_close( c );
					break;
				}
				else if( sts == 0 ) {
				  rpc_log( RPC_LOG_DEBUG, "Connection graceful close connid=%"PRIx64"", c->connid );
				  rpc_conn_close( c );
				} 
				else {
				  if( sts != 4 ) rpc_log( RPC_LOG_ERROR, "recvlen %d != 4", sts );
				  
				  c->cdata.rx += sts;
					xdr_init( &tmpx, c->buf, 4 );
					xdr_decode_uint32( &tmpx, &c->cdata.count );
					if( !(c->cdata.count & 0x80000000) ) {
					  rpc_conn_close( c );
						break;
					}

					c->cdata.count &= ~0x80000000;
					if( c->cdata.count > RPC_MAX_BUF ) {
					  rpc_conn_close( c );
						break;
					}

					c->cdata.offset = 0;
					c->cstate = RPC_CSTATE_RECV;
				}
				break;
			case RPC_CSTATE_RECV:
				sts = recv( c->fd, c->buf + c->cdata.offset, c->cdata.count - c->cdata.offset, 0 );
				if( sts < 0 ) {
#ifdef WIN32
					if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
					if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
					rpc_conn_close( c );
					break;
				}
				else if( sts == 0 ) {
				  rpc_conn_close( c );
					break;
				}
				c->cdata.rx += sts;
				c->timestamp = rpc_now();
				
				c->cdata.offset += sts;
				if( c->cdata.offset == c->cdata.count ) {
					/* msg complete - process */
					xdr_init( &c->inc.xdr, c->buf, RPC_MAX_BUF );
					c->inc.xdr.count = c->cdata.count;

					memcpy( &c->inc.raddr, &c->addr, c->addrlen );
					c->inc.raddr_len = c->addrlen;
					rpc.aconn.listen = NULL;
					rpc.aconn.conn = c;

					/* If the msg is a reply and the reply handler wants to send back on this connection then it 
					 * can't because the connection is still in recv state. we can safely set it back to recvlen here */
					c->cstate = RPC_CSTATE_RECVLEN;
					c->nstate = RPC_NSTATE_RECV;
					c->cdata.offset = 0;

					sts = rpc_process_incoming( &c->inc );
					memset( &rpc.aconn, 0, sizeof(rpc.aconn) );
					
					if( sts == 0 ) {
					        c->cdata.count = c->inc.xdr.offset;
						c->cdata.offset = 0;
						c->cstate = RPC_CSTATE_SENDLEN;
						c->nstate = RPC_NSTATE_SEND;

						/* optimistically send count */
						xdr_init( &tmpx, tmpbuf, 4 );
						xdr_encode_uint32( &tmpx, c->cdata.count | 0x80000000 );
						c->timestamp = rpc_now();

						sts = send( c->fd, tmpbuf, 4, 0 );
						if( sts < 0 ) {
#ifdef WIN32
						  if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
						  if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
						  rpc_conn_close( c );
						  break;
						} else if( sts != 4 ) {
						  rpc_log( RPC_LOG_ERROR, "Failed to send count %d != 4", sts );
						}
						c->cdata.tx += sts;

						c->cstate = RPC_CSTATE_SEND;
						sts = send( c->fd, c->buf + c->cdata.offset, c->cdata.count - c->cdata.offset, 0 );
						if( sts < 0 ) {
#ifdef WIN32
							if( WSAGetLastError() == WSAEWOULDBLOCK ) break;
#else
							if( (errno == EAGAIN) || (errno == EINTR) ) break;
#endif
							rpc_conn_close( c );
							break;
						}
						c->cdata.tx += sts;
						
						c->cdata.offset += sts;
						if( c->cdata.offset == c->cdata.count ) {
							c->nstate = RPC_NSTATE_RECV;
							c->cstate = RPC_CSTATE_RECVLEN;
							if( c->cdata.cb ) c->cdata.cb( RPC_CONN_DONE_SEND, c );
						}
					} else if( sts > 0 ) {
					  rpc_log( RPC_LOG_TRACE, "rpc_process_incoming noresponse" );
					  c->cstate = RPC_CSTATE_RECVLEN;
					  c->nstate = RPC_NSTATE_RECV;
					  if( c->cdata.cb ) c->cdata.cb( RPC_CONN_DONE_SEND, c );
					} else {
					  rpc_log( RPC_LOG_ERROR, "rpc_process_incoming failed" );
					  c->cstate = RPC_CSTATE_RECVLEN;
					  c->nstate = RPC_NSTATE_RECV;
					  if( c->cdata.cb ) c->cdata.cb( RPC_CONN_DONE_SEND, c );
					}

				}

				break;
			default:
				break;
			}

		}
		else if( revents[i] & RPC_POLLOUT ) {
			c->timestamp = rpc_now();

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
					rpc_conn_close( c );
					break;
				} else if( sts != 4 ) {
				  rpc_log( RPC_LOG_ERROR, "Failed to send count %d != 4", sts );
				}
				
				c->cdata.tx += sts;
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
					rpc_conn_close( c );
					break;
				}
				c->cdata.tx += sts;
				
				c->cdata.offset += sts;
				if( c->cdata.offset == c->cdata.count ) {
					c->nstate = RPC_NSTATE_RECV;
					c->cstate = RPC_CSTATE_RECVLEN;
					if( c->cdata.cb ) c->cdata.cb( RPC_CONN_DONE_SEND, c );
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
										   rpc_log( RPC_LOG_ERROR, "Connect failed: %s", rpc_strerror( rpc_errno() ) );
										   rpc_conn_close( c );
									   }
									   else {
										   c->nstate = RPC_NSTATE_RECV;
										   c->cstate = RPC_CSTATE_RECVLEN;
									   }

									   rpc_log( RPC_LOG_INFO, "Nonblocking connect completed" );
									   if( c->cdata.cb ) c->cdata.cb( RPC_CONN_CONNECT, c );
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

#ifdef __linux__
	if( revents[eventfdidx] & POLLIN ) {
	  uint64_t nevents;
	  struct timespec tm;
	  struct io_event event;
	  struct rpcd_iocb *iocb;

	  rpc_log( RPC_LOG_INFO, "eventfd ready" );
	  sts = read( rpc.eventfd, (char *)&nevents, sizeof(nevents) );
	  rpc_log( RPC_LOG_INFO, "eventfd read sts=%d nevents=%"PRIu64"", sts, nevents );
	  
	  while( nevents > 0 ) {
	    memset( &tm, 0, sizeof(tm) );
	    sts = io_getevents( rpc.iocxt, 0, 1, &event, &tm );
	    rpc_log( RPC_LOG_INFO, "eventfd io_getevents sts=%d", sts );
	    if( sts > 0 ) {
	      iocb = (struct rpcd_iocb *)event.data;
	      if( iocb->donecb ) iocb->donecb( iocb->mmf, iocb->buf, event.res, iocb->prv );
	      rpcd_iocb_free( iocb );
	    }
	    nevents--;
	  }
	      
	}
#endif
	
	/* Close pending connections */
	rpc_close_connections();

}

static void rpc_close_connections( void ) {
	struct rpc_conn *c, *prev, *next;

	c = rpc.clist;
	while( c ) {
	  if( c->cstate == RPC_CSTATE_CLOSE && c->cdata.cb ) c->cdata.cb( RPC_CONN_CLOSE, c );
	  c = c->next;
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
			rpc_log( RPC_LOG_DEBUG, "Closing connection %"PRIu64"", c->connid );

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

struct rpc_conn *rpc_conn_acquire( void ) {
  struct rpc_conn *c;
  c = rpc.flist;
  if( !c ) return NULL;
  rpc.flist = rpc.flist->next;
  return c;
}

void rpc_conn_release( struct rpc_conn *c ) {
  c->next = rpc.flist;
  rpc.flist = c;
}

static void rpc_accept( struct rpc_listen *lis ) {
  struct rpc_conn *c;
  int sts;
  char ipstr[64];
  
  switch( lis->type ) {
  case RPC_LISTEN_UDP:
  case RPC_LISTEN_UDP6:
    {
      socklen_t slen;

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
      mynet_ntop( lis->type, (struct sockaddr *)&c->inc.raddr, c->inc.raddr_len, ipstr );
      rpc_log( RPC_LOG_DEBUG, "UDP Incoming %s count=%d", ipstr, c->cdata.count );
      rpc.flist = rpc.flist->next;

      rpc.aconn.listen = lis;
      rpc.aconn.conn = NULL;
      sts = rpc_process_incoming( &c->inc );
      memset( &rpc.aconn, 0, sizeof(rpc.aconn) );
      
      rpc.flist = c;
      if( sts == 0 ) {
	sts = sendto( lis->fd, c->buf, c->inc.xdr.offset, 0, (struct sockaddr *)&c->inc.raddr, c->inc.raddr_len );
	if( sts < 0 ) rpc_log( RPC_LOG_ERROR, "sendto: %s", rpc_strerror( rpc_errno() ) );
      } else if( sts > 0 ) {
	rpc_log( RPC_LOG_TRACE, "rpc_process_incoming noresponse" );
      } else {
	rpc_log( RPC_LOG_ERROR, "rpc_process_incoming failed" );
      }

    }
    break;
  case RPC_LISTEN_TCP:
  case RPC_LISTEN_TCP6:
#ifndef WIN32
  case RPC_LISTEN_UNIX:
#endif
    {
      rpc_log( RPC_LOG_TRACE, "Accept TCP/UNIX" );

      /* get connection descriptor */
      c = rpc.flist;
      if( !c ) return;
      rpc.flist = c->next;

      c->inc.raddr_len = sizeof(c->inc.raddr);
      c->fd = accept( lis->fd, (struct sockaddr *)&c->inc.raddr, &c->inc.raddr_len );
      memcpy( &c->addr, &c->inc.raddr, c->inc.raddr_len );
      c->addrlen = c->inc.raddr_len;
      
      c->cstate = RPC_CSTATE_RECVLEN;
      c->nstate = RPC_NSTATE_RECV;
      c->connid = ++rpc.connid;
      c->timestamp = rpc_now();
      c->listen = lis;
      c->listype = lis->type;
      c->dirtype = RPC_CONN_DIR_INCOMING;
      
#ifdef WIN32
	  {
		u_long nb = 1;
		ioctlsocket( c->fd, FIONBIO, &sts );
	  }
#else
      sts = fcntl( c->fd, F_GETFL, 0 );
      fcntl( c->fd, F_SETFL, sts|O_NONBLOCK );
#endif

      c->next = rpc.clist;
      rpc.clist = c;

      mynet_ntop( lis->type, (struct sockaddr *)&c->inc.raddr, c->inc.raddr_len, ipstr );
      rpc_log( RPC_LOG_DEBUG, "Accept Incoming Connection %s fd=%d", ipstr, (int)c->connid );
    }
    break;
  }
}

void rpcd_init( void ) {
	int i;
	struct rpc_conn *c;
	struct rpcd_iocb *iocb;
	
	/* setup connection table */
	for( i = 0; i < RPC_MAX_CONN; i++ ) {
		c = &rpc.conntab[i];
		memset( c, 0, sizeof(*c) );
		c->next = rpc.flist;
		c->buf = rpc.buftab[i];
		c->count = sizeof(rpc.buftab[i]);
		rpc.flist = c;
	}

	for( i = 0; i < RPCD_MAX_IOCB; i++ ) {
	  iocb = &rpc.iocb[i];
	  iocb->next = rpc.iocbfl;
	  rpc.iocbfl = iocb;
	}

#ifdef WIN32
	rpc.evt = WSACreateEvent();
#endif

#ifdef __linux__
	io_setup( RPCD_MAX_IOCB, &rpc.iocxt );
	rpc.eventfd = eventfd( 0, EFD_NONBLOCK );
#endif
}

#ifdef WIN32
HANDLE rpcd_win32event( void ) {
	return rpc.evt;
}
#endif

void rpc_service( int timeout ) {
	struct rpc_conn *c;
	uint64_t now;

	/* detect stale connections */
	c = rpc.clist;
	now = rpc_now();
	while( c ) {
		if( now > (c->timestamp + RPC_CONNECTION_TIMEOUT) ) {
		        rpc_log( RPC_LOG_DEBUG, "Closing stale connection fd=%d", (int)c->connid );
			rpc_conn_close( c );
		}

		c = c->next;
	}
	rpc_close_connections();

	/* service networking/iterators/waiters etc */
	rpc_poll( timeout );
	rpc_iterator_service();
	rpc_waiter_service();
}

static void rpcd_run( void ) {
	struct rpc_conn *c;
	int i;
	int timeout;
	struct rpc_program *pglist;
	struct rpc_version *vlist;
	struct sockaddr_in sin;
	struct rpcbind_mapping map;
	uint64_t now;

	rpc.rpcdp = 1;
	
#ifdef WIN32
	{
		WSADATA wsadata;
		WSAStartup( MAKEWORD( 2, 2 ), &wsadata );
	}
#endif

	/* setup connection table etc */
	rpcd_init();
	
	/* register programs and initialize */
	if( rpc.main_cb ) rpc.main_cb( RPCD_EVT_INIT, NULL, rpc.main_cxt );

	/* dynamically load other services */
	rpcd_load_services();
	
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

				if( !rpc.no_rpcregister ) rpcbind_call_set( &sin, &map );
			}

			vlist = vlist->next;
		}

		pglist = pglist->next;
	}

	/* listen on ports */
	rpc_init_listen();

	/* poll loop */
	while( !rpc.exiting ) {
		/* detect stale connections */
		c = rpc.clist;
		now = rpc_now();
		while( c ) {
			if( now > (c->timestamp + RPC_CONNECTION_TIMEOUT) ) {
			        rpc_log( RPC_LOG_DEBUG, "Closing stale connection fd=%d", (int)c->connid );
				rpc_conn_close( c );
			}

			c = c->next;
		}
		rpc_close_connections();

		/* compute timeout */
		timeout = rpc_iterator_timeout( 1000 );
		if( timeout > 1000 ) timeout = 1000;
		if( timeout < 0 ) timeout = 0;

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

	/* run shutdown callback if required */
	if( rpc.main_cb ) rpc.main_cb( RPCD_EVT_CLOSE, NULL, rpc.main_cxt );

	/* close listening sockets */
	for( i = 0; i < rpc.nlisten; i++ ) {
#ifdef WIN32
		closesocket( rpc.listen[i].fd );
#else
		close( rpc.listen[i].fd );
#endif
	}

	rpc.rpcdp = 0;
}

int rpc_connect( struct sockaddr *addr, socklen_t alen, rpc_conn_cb_t cb, void *cxt, uint64_t *connid ) {
	struct rpc_conn *c;
	int sts;

	/* start by allocating a connection descriptor */
	c = rpc.flist;
	if( !c ) return -1;

	rpc.flist = c->next;

	c->fd = socket( addr->sa_family, SOCK_STREAM, 0 );
	if( c->fd < 0 ) {
	  rpc_log( RPC_LOG_ERROR, "socket: %s", rpc_strerror( rpc_errno() ) );
	  goto failure;
	}

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
	switch( addr->sa_family ) {
	case AF_INET:
	  c->listype = RPC_LISTEN_TCP;
	  break;
#ifndef WIN32
	case AF_UNIX:
	  c->listype = RPC_LISTEN_UNIX;
	  break;
#endif
	default:
	  c->listype = 0;
	  break;
	}
	memcpy( &c->addr, addr, alen );
	c->addrlen = alen;
	       
	sts = connect( c->fd, addr, alen );
	if( sts < 0 ) {
#ifdef WIN32
		if( WSAGetLastError() != WSAEWOULDBLOCK ) {
			closesocket( c->fd );
			goto failure;
		}
#else
		/* tcp returns EINPROGRESS, unix domain sockets return EAGAIN */
		if( errno != EINPROGRESS && errno != EAGAIN ) {
		  rpc_log( RPC_LOG_ERROR, "connect: %s", rpc_strerror( errno ) );
		  close( c->fd );
		  goto failure;
		}
#endif
		rpc_log( RPC_LOG_DEBUG, "connect nonblocking in progress" );
		c->cstate = RPC_CSTATE_CONNECT;
		c->nstate = RPC_NSTATE_CONNECT;
		c->connid = ++rpc.connid;
	}
	else {
		c->cstate = RPC_CSTATE_RECVLEN;
		c->nstate = RPC_NSTATE_RECV;
		c->connid = ++rpc.connid;

		rpc_log( RPC_LOG_DEBUG, "connect completed immediately" );
		if( c->cdata.cb ) c->cdata.cb( RPC_CONN_CONNECT, c );
	}

	if( connid ) *connid = c->connid;

	c->timestamp = rpc_now();
	c->dirtype = RPC_CONN_DIR_OUTGOING;
	c->next = rpc.clist;
	rpc.clist = c;

	return 0;

failure:
	rpc_log( RPC_LOG_INFO, "rpc_connect failure" );
	c->next = rpc.flist;
	rpc.flist = c;
	return -1;
}

int rpc_send( struct rpc_conn *c, int count ) {
  int sts;
  char tmpbuf[4];
  struct xdr_s tmpx;
  
  /* unless the connection is idle we can't start sending */
  if( c->cstate != RPC_CSTATE_RECVLEN ) return -1;
  
  c->cstate = RPC_CSTATE_SENDLEN;
  c->nstate = RPC_NSTATE_SEND;
  c->cdata.count = count;
  c->cdata.offset = 0;

  /* optimistically send count */
  xdr_init( &tmpx, (uint8_t *)tmpbuf, 4 );
  xdr_encode_uint32( &tmpx, c->cdata.count | 0x80000000 );
  c->timestamp = rpc_now();
  
  sts = send( c->fd, tmpbuf, 4, 0 );
  if( sts < 0 ) {
#ifdef WIN32
    if( WSAGetLastError() == WSAEWOULDBLOCK ) return 0;
#else
    if( (errno == EAGAIN) || (errno == EINTR) ) return 0;
#endif
    rpc_conn_close( c );
    return 0;
  } else if( sts != 4 ) {
    rpc_log( RPC_LOG_ERROR, "Failed to send count %d != 4", sts );
  }
  c->cdata.tx += sts;
  
  c->cstate = RPC_CSTATE_SEND;
  sts = send( c->fd, c->buf + c->cdata.offset, c->cdata.count - c->cdata.offset, 0 );
  if( sts < 0 ) {
#ifdef WIN32
    if( WSAGetLastError() == WSAEWOULDBLOCK ) return 0;
#else
    if( (errno == EAGAIN) || (errno == EINTR) ) return 0;
#endif
    rpc_conn_close( c );
    return 0;
  }
  c->cdata.tx += sts;
  
  c->cdata.offset += sts;
  if( c->cdata.offset == c->cdata.count ) {
    c->nstate = RPC_NSTATE_RECV;
    c->cstate = RPC_CSTATE_RECVLEN;
    if( c->cdata.cb ) c->cdata.cb( RPC_CONN_DONE_SEND, c );
  }
  
  return 0;
}

struct rpc_listen *rpcd_listen_by_type( rpc_listen_t type ) {
  int i;
  for( i = 0; i < rpc.nlisten; i++ ) {
    if( rpc.listen[i].type == type ) return &rpc.listen[i];
  }
  return NULL;
}

struct rpc_conn *rpc_conn_by_connid( uint64_t connid ) {
	struct rpc_conn *c;
	c = rpc.clist;
	while( c ) {
		if(c->connid == connid) return c;
		c = c->next;
	}
	return NULL;
}

struct rpc_conn *rpc_conn_by_addr( rpc_listen_t type, char *addr, int addrlen ) {
  struct rpc_conn *c;
  c = rpc.clist;
  while( c ) {
    if( c->dirtype == RPC_CONN_DIR_INCOMING && c->listen->type == type ) {
      switch( type ) {
      case RPC_LISTEN_TCP:
	{
	  struct sockaddr_in *sinp = (struct sockaddr_in *)&c->addr;
	  if( addrlen == sizeof(sinp->sin_addr) &&
	      (memcmp( &sinp->sin_addr, addr, addrlen ) == 0) ) {
	    return c;
	  }
	}
	break;
      case RPC_LISTEN_TCP6:
	{
	  struct sockaddr_in6 *sin6p = (struct sockaddr_in6 *)&c->addr;
	  if( addrlen == sizeof(sin6p->sin6_addr) &&
	      (memcmp( &sin6p->sin6_addr, addr, addrlen ) == 0) ) {
	    return c;
	  }
	}
	break;
#ifndef WIN32
      case RPC_LISTEN_UNIX:
	{
	  /* TODO */
	}
	break;
#endif
      default:
	break;
      }
    }
    c = c->next;
  }
  return NULL;
}


void rpc_conn_close( struct rpc_conn *c ) {
  rpc_log( RPC_LOG_DEBUG, "Close connection %"PRIu64"", c->connid );
  c->cstate = RPC_CSTATE_CLOSE;
}

struct rpc_conn *rpc_conn_list( void ) {
  return rpc.clist;
}

    
static char *mynet_ntop( rpc_listen_t type, struct sockaddr *addr, int alen, char *ipstr ) {
  struct sockaddr_in *sinp;
  struct sockaddr_in6 *sinp6;
  int i;

  switch( type ) {
  case RPC_LISTEN_UDP:
  case RPC_LISTEN_TCP:
    if( alen != sizeof(*sinp) ) return NULL;
    sinp = (struct sockaddr_in *)addr;
    sprintf( ipstr, "%d.%d.%d.%d:%d",
	     (sinp->sin_addr.s_addr) & 0xff,
	     (sinp->sin_addr.s_addr >> 8) & 0xff,
	     (sinp->sin_addr.s_addr >> 16) & 0xff,
	     (sinp->sin_addr.s_addr >> 24) & 0xff,
	     ntohs( sinp->sin_port ) );
    break;
  case RPC_LISTEN_UDP6:
  case RPC_LISTEN_TCP6:
    if( alen != sizeof(*sinp6) ) return NULL;
    sinp6 = (struct sockaddr_in6 *)addr;
    strcpy( ipstr, "" );
    for( i = 0; i < 8; i++ ) {
      if( i > 0 ) sprintf( ipstr + strlen( ipstr ), ":" );
      if( sinp6->sin6_addr.s6_addr[2*i] == 0 &&
	  sinp6->sin6_addr.s6_addr[2*i + 1] == 0 ) {
      } else {
	sprintf( ipstr + strlen( ipstr ), "%02x%02x",
		 (uint32_t)sinp6->sin6_addr.s6_addr[2*i],
		 (uint32_t)sinp6->sin6_addr.s6_addr[2*i + 1] );
      }
    }
    sprintf( ipstr + strlen( ipstr ), ":%d", ntohs( sinp6->sin6_port ) );
    break;
  case RPC_LISTEN_UNIX:
    strcpy( ipstr, "unix" );
    break;
  default:
    sprintf( ipstr, "unknown" );
    break;
  }
  
  return ipstr;	     
}

static void rpcd_load_service( char *svcname, char *path, char *mainfn ) {
  void *hdl;
  rpcd_service_t pmain;

#ifdef WIN32
  hdl = LoadLibraryA( path );
  if( !hdl ) {
    rpc_log( RPC_LOG_ERROR, "Failed to load service %s from \"%s\"", svcname, path );
    return;
  }
  pmain = (rpcd_service_t)GetProcAddress( hdl, mainfn ? mainfn : "rpc_main" );
#else
  
  hdl = dlopen( path, 0 );
  if( !hdl ) {
    rpc_log( RPC_LOG_ERROR, "Failed to load service %s from \"%s\": %s", svcname, path, rpc_strerror( errno ) );
    return;
  }

  /* casting pointer to function pointer is not allowed, but the dl api forces us to do so? */
  pmain = (rpcd_service_t)dlsym( hdl, mainfn ? mainfn : "rpc_main" );
#endif
  
  if( pmain ) pmain();
  else rpc_log( RPC_LOG_ERROR, "Failed to load service %s entry point %s", svcname, mainfn );
    
}

static void rpcd_load_services( void ) {
  int sts;
  uint64_t hkey, id;
  struct freg_entry entry;
  char path[256], mainfn[64];
  int enabled;
  
  rpc_log( RPC_LOG_DEBUG, "Loading dynamic services" );
  
  sts = freg_subkey( NULL, 0, "/fju/rpc/services", 0, &hkey );
  if( sts ) return;

  id = 0;
  do {
    sts = freg_next( NULL, hkey, id, &entry );
    if( sts ) break;
    
    if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_KEY ) {
      memset( path, 0, sizeof(path) );
      memset( mainfn, 0, sizeof(mainfn) );
      enabled = 1;
      
      sts = freg_get_by_name( NULL, entry.id, "path", FREG_TYPE_STRING, path, sizeof(path) - 1, NULL );
      if( !sts ) {
	sts = freg_get_by_name( NULL, entry.id, "mainfn", FREG_TYPE_STRING, mainfn, sizeof(mainfn) - 1, NULL );
	if( sts ) {
	  strcpy( mainfn, "" );
	  sts = 0;
	}
      }
      if( !sts ) {
	freg_get_by_name( NULL, entry.id, "enabled", FREG_TYPE_UINT32, (char *)&enabled, sizeof(enabled), NULL );
      }
      
      if( !sts && enabled ) {
	rpc_log( RPC_LOG_DEBUG, "Loading service %s from \"%s\"", entry.name, path );
	rpcd_load_service( entry.name, path, mainfn[0] ? mainfn : NULL );
      }
    }
    
    id = entry.id;
  } while( 1 );
  
}

void rpcd_stop( void ) {
    rpc.exiting = 1;
}


int rpcd_active_conn( struct rpcd_active_conn *aconn ) {
    *aconn = rpc.aconn;
    return 0;
}

#ifdef __FreeBSD__
int rpcd_aio_read( struct mmf_s *mmf, char *buf, int len, uint64_t offset, rpcd_aio_donecb donecb, void *prv ) {
  struct rpcd_iocb *iocb;
  int sts;
  
  iocb = rpcd_iocb_alloc();
  if( !iocb ) return -1;

  iocb->aiocb.aio_fildes = mmf->fd;
  iocb->aiocb.aio_offset = offset;
  iocb->aiocb.aio_buf = buf;
  iocb->aiocb.aio_nbytes = len;
  iocb->aiocb.aio_sigevent.sigev_notify = SIGEV_KEVENT;
  iocb->aiocb.aio_sigevent.sigev_notify_kqueue = rpc.kq;
  iocb->aiocb.aio_sigevent.sigev_value.sigval_ptr = iocb;
  iocb->donecb = donecb;
  iocb->prv = prv;
  iocb->mmf = mmf;
  iocb->buf = buf;
  iocb->len = len;
  
  sts = aio_read( &iocb->aiocb );
  if( sts < 0 ) return -1;

  return 0;
}

int rpcd_aio_write( struct mmf_s *mmf, char *buf, int len, uint64_t offset, rpcd_aio_donecb donecb, void *prv ) {
  struct rpcd_iocb *iocb;
  int sts;
  
  iocb = rpcd_iocb_alloc();
  if( !iocb ) return -1;

  iocb->aiocb.aio_fildes = mmf->fd;
  iocb->aiocb.aio_offset = offset;
  iocb->aiocb.aio_buf = buf;
  iocb->aiocb.aio_nbytes = len;
  iocb->aiocb.aio_sigevent.sigev_notify = SIGEV_KEVENT;
  iocb->aiocb.aio_sigevent.sigev_notify_kqueue = rpc.kq;
  iocb->aiocb.aio_sigevent.sigev_value.sigval_ptr = iocb;
  iocb->donecb = donecb;
  iocb->prv = prv;
  iocb->mmf = mmf;
  iocb->buf = buf;
  iocb->len = len;
		    
  sts = aio_write( &iocb->aiocb );
  if( sts < 0 ) return -1;

  return 0;  
}


#endif

#ifdef __linux__

int rpcd_aio_read( struct mmf_s *mmf, char *buf, int len, uint64_t offset, rpcd_aio_donecb donecb, void *prv ) {
  struct rpcd_iocb *iocb;
  int sts;
  struct iocb *iocbp[1];
  
  iocb = rpcd_iocb_alloc();
  if( !iocb ) return -1;

  memset( iocb, 0, sizeof(*iocb) );
  iocb->donecb = donecb;
  iocb->prv = prv;
  iocb->mmf = mmf;
  iocb->buf = buf;
  iocb->len = len;
    
  io_prep_pread( &iocb->aiocb, mmf->fd, buf, len, offset );
  io_set_eventfd( &iocb->aiocb, rpc.eventfd );
  iocb->aiocb.data = iocb;
  
  iocbp[0] = &iocb->aiocb;
  sts = io_submit( rpc.iocxt, 1, iocbp );
  if( sts < 0 ) {
    rpcd_iocb_free( iocb );
    return -1;
  }

  return 0;
}

int rpcd_aio_write( struct mmf_s *mmf, char *buf, int len, uint64_t offset, rpcd_aio_donecb donecb, void *prv ) {
  struct rpcd_iocb *iocb;
  int sts;
  struct iocb *iocbp[1];
  
  iocb = rpcd_iocb_alloc();
  if( !iocb ) return -1;

  memset( iocb, 0, sizeof(*iocb) );
  iocb->donecb = donecb;
  iocb->prv = prv;
  iocb->mmf = mmf;
  iocb->buf = buf;
  iocb->len = len;
  
  io_prep_pwrite( &iocb->aiocb, mmf->fd, buf, len, offset );
  io_set_eventfd( &iocb->aiocb, rpc.eventfd );
  iocb->aiocb.data = iocb;
  
  iocbp[0] = &iocb->aiocb;
  sts = io_submit( rpc.iocxt, 1, iocbp );
  if( sts < 0 ) {
    rpcd_iocb_free( iocb );
    return -1;
  }

  return 0;
}

#endif

#ifdef WIN32

static void win32_iocb_done( DWORD errorcode, DWORD nbytes, OVERLAPPED *overlap ) {
  struct rpcd_iocb *iocb;

  iocb = (struct rpcd_iocb *)overlap->hEvent;
  if( iocb->donecb ) iocb->donecb( iocb->mmf, iocb->buf, nbytes, iocb->prv );
  rpcd_iocb_free( iocb );
}

int rpcd_aio_read( struct mmf_s *mmf, char *buf, int len, uint64_t offset, rpcd_aio_donecb donecb, void *prv ) {
  OVERLAPPED *overlap;
  int sts;
  struct rpcd_iocb *iocb;

  iocb = rpcd_iocb_alloc();
  if( !iocb ) return -1;

  overlap = &iocb->overlap;
  
  memset( iocb, 0, sizeof(iocb) );
  iocb->mmf = mmf;
  iocb->buf = buf;
  iocb->len = len;
  iocb->donecb = donecb;
  iocb->prv = prv;
  
  memset( overlap, 0, sizeof(*overlap) );
  overlap->Offset = offset & 0xffffffff;
  overlap->OffsetHigh = offset >> 32;
  overlap->hEvent = (HANDLE)iocb;
  
  sts = ReadFileEx( mmf->fd, buf, len, overlap, win32_iocb_done );
  if( sts == 0 ) {
    rpcd_iocb_free( iocb );
    return -1;
  }
  
  return 0;
}

int rpcd_aio_write( struct mmf_s *mmf, char *buf, int len, uint64_t offset, rpcd_aio_donecb donecb, void *prv ) {
    OVERLAPPED *overlap;
  int sts;
  struct rpcd_iocb *iocb;

  iocb = rpcd_iocb_alloc();
  if( !iocb ) return -1;

  overlap = &iocb->overlap;
  
  memset( iocb, 0, sizeof(iocb) );
  iocb->mmf = mmf;
  iocb->buf = buf;
  iocb->len = len;
  iocb->donecb = donecb;
  iocb->prv = prv;
  
  memset( overlap, 0, sizeof(*overlap) );
  overlap->Offset = offset & 0xffffffff;
  overlap->OffsetHigh = offset >> 32;
  overlap->hEvent = (HANDLE)iocb;
  
  sts = WriteFileEx( mmf->fd, buf, len, overlap, win32_iocb_done );
  if( sts == 0 ) {
    rpcd_iocb_free( iocb );
    return -1;
  }
  
  return 0;
}


#endif

