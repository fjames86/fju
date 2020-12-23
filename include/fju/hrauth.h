
#ifndef HRAUTH_H
#define HRAUTH_H

#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/programs.h>

#define RPC_AUTH_HRAUTH 0x27E1DAF0

struct hrauth_context {
  uint64_t remoteid;                /* remote host id */  
  uint8_t key[32];                  /* common secret */
  uint8_t session_key[32];          /* ephemeral session key */
  int service;                      /* service level */
#define HRAUTH_SERVICE_NONE   0     /* just authentication */
#define HRAUTH_SERVICE_INTEG  1     /* arg/result checksumming */
#define HRAUTH_SERVICE_PRIV   2     /* checksumming and encryption */
  int window;                       /* acceptable clock skew */
  uint32_t cipher;                  /* cipher suite. we want to be able to extend in future */
#define HRAUTH_CIPHER_MASK    0x0000ffff    /* mask off the low 16 bits to get the cipher to use */
#define HRAUTH_CIPHER_AES128  0x00000001    /* AES-128 */
#define HRAUTH_DIGEST_MASK    0xffff0000    /* mask off the high 16 bits to get the digest to use for hmacs */
#define HRAUTH_DIGEST_SHA1    0x00010000    /* SHA1 */
  uint32_t nickname;                /* authentication nickname */
  uint32_t timestamp;               /* timestamp we sent */
};

  /* encrypted and sent in the auth structure */
struct hrauth_cred {
  uint32_t nonce;
  uint8_t session_key[32];
  uint32_t service;
  uint32_t window;
  uint32_t cipher;
};

struct hrauth_auth {
  int tag;
#define HRAUTH_NICKNAME 0
#define HRAUTH_FULL     1
  union {
    uint32_t nickname;
    struct {
      uint64_t id;
      uint32_t cipher;
      uint8_t ecred[64];   /* encrypted cred structure */
    } full;
  } u;
};

/* verifier structure - encrypted with session key */
struct hrauth_verf {
  uint32_t nonce;      /* random number */
  uint32_t timestamp;  /* current time */
  uint32_t tverf;      /* timestamp - 1 */
  uint32_t nickname;   /* context nickname */
};

int hrauth_init( struct hrauth_context *cxt, uint64_t remoteid );
struct rpc_provider *hrauth_provider( void );
int hrauth_priv_header( struct hrauth_context *sa, struct xdr_s *xdr, int start, int end );
void hrauth_register( void );

struct hrauth_call {
  uint64_t hostid;
  uint32_t prog;
  uint32_t vers;
  uint32_t proc;
  void (*donecb)( struct xdr_s *res, struct hrauth_call *hcallp );
  void *cxt;  /* private data */
  uint64_t cxt2; /* more private data */
  int timeout;
  int service;
};

struct hrauth_call_opts {
  uint32_t mask;
#define HRAUTH_CALL_OPT_FD           0x0001   /* use this file descriptor to send on */
#define HRAUTH_CALL_OPT_TMPBUF       0x0002   /* encode xdr using this buffer */
#define HRAUTH_CALL_OPT_PORT         0x0004   /* send to this port */
#define HRAUTH_CALL_OPT_ADDRMASK     0x0008   /* bitmask indicating which host addresses to send to */
#ifdef WIN32
  SOCKET fd;
#else
  int fd;
#endif
  struct xdr_s tmpbuf;
  int port;
  uint32_t addrmask;
};
int hrauth_call_udp_async( struct hrauth_call *hcall, struct xdr_s *args, int nargs, struct hrauth_call_opts *opts );

//int hrauth_call_tcp_async( struct hrauth_call *hcall, struct xdr_s *args );

/* forward this call to the specified host */
int hrauth_call_udp_proxy( struct rpc_inc *inc, uint64_t hostid, struct xdr_s *args, int nargs );

/* synchronous rpc call - don't use these in service routines */
int hrauth_call_udp( struct hrauth_call *hcall, struct xdr_s *args, struct xdr_s *res, struct hrauth_call_opts *opts );
int hrauth_call_tcp( struct hrauth_call *hcall, struct xdr_s *args, struct xdr_s *res, struct hrauth_call_opts *opts );



/* 
 * TCP connection handling. This allows connecting to remote servers and sending RPC calls to them.
 * Hosts are registered to have connections established. Connections are reestablished if dropped.
 * Only a single RPC can be sent at a time, no queuing is implemented. This means if an RPC is 
 * currently being sent then a new RPC cannot be sent until the previous send has completed. 
 * Furthermore, if a reply is expected no futher calls can be sent until that reply is received. 
 * This is because receive and transmit share the same buffer (!). Maybe that should be fixed...
 * 
 * Because of these limitations the following is best practice: 
 * - Both services should establish connections to each other.
 * - These connections are outgoing only i.e. only RPC calls can be made, there are never any 
 * replies sent back. 
 * - A message oriented architecture needs to be used where RPCs procs are always silent and may 
 * reply only be sending back a new call on the outgoing connection.
 * 
 */
#define HRAUTH_CONN_PINGTIMEOUT (30000)

struct hrauth_conn_opts {
  uint32_t mask;
#define HRAUTH_CONN_OPT_ADDR 0x0001
#define HRAUTH_CONN_OPT_PINGTIMEOUT 0x0002 
  struct sockaddr_storage addr;
  int addrlen;
  uint32_t pingtimeout;
};
int hrauth_conn_register( uint64_t hostid, struct hrauth_conn_opts *opts );
int hrauth_conn_unregister( uint64_t hostid );
int hrauth_conn_list( uint64_t *hostid, int n );
int hrauth_call_tcp_async( struct hrauth_call *hcall, struct xdr_s *args, int nargs );
int hrauth_call_async( struct hrauth_call *hcall, struct xdr_s *args, int nargs );
int hrauth_reply_tcp( struct hrauth_context *hcxt, uint32_t xid, int acceptstat, struct xdr_s *res, int nres );
int hrauth_reply( struct rpc_inc *inc, struct xdr_s *res, int nres );

#endif

