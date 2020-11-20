
#ifndef RAFT_H
#define RAFT_H

#include <stdint.h>
#include <fju/log.h>

#define RAFT_MAX_MEMBER  8
#define RAFT_MAX_CLUSTER 32

struct raft_member {
  uint64_t hostid;      /* host identifier */
  uint64_t lastseen;    /* when last received a message from this host */
  uint64_t storedseq;   /* (leader only) highest command seq acked by member */
  uint32_t flags;       /* member flags */
#define RAFT_MEMBER_VOTED  0x0001    /* (leader only) received vote this election */
  
  uint32_t spare[5];
};

struct raft_cluster {
  uint64_t clid;         /* cluster identifier */
  uint64_t leaderid;     /* current leader. 0 indicates no leader */
  uint64_t voteid;       /* who we voted for this term */
  uint64_t term;         /* current term seqno */
  uint64_t appliedseq;   /* highest state seqno applied to local state machine */
  uint64_t commitseq;    /* (leader only) highest seq command saved by quorum of cluster */
  uint64_t timeout;      /* when current term/election ends */
  uint32_t state;        /* current state */
#define RAFT_STATE_FOLLOWER     0
#define RAFT_STATE_CANDIDATE    1
#define RAFT_STATE_LEADER       2
  
  /* members */
  uint32_t nmember;
  struct raft_member member[RAFT_MAX_MEMBER];

  uint32_t appid;
  uint32_t flags;
#define RAFT_CLUSTER_WITNESS 0x0001
  
  uint32_t spare[2];
};

struct raft_prop {
  uint32_t magic;
#define RAFT_MAGIC 0x22defc03
  uint32_t version;
#define RAFT_VERSION 1
  uint64_t seq;
  uint32_t count;
  uint32_t flags;
  uint16_t elec_low;
  uint16_t elec_high;
  uint16_t term_low;
  uint16_t term_high;
  uint32_t rpc_timeout;
  
  uint32_t spare[7];
};

int raft_open( void );
int raft_close( void );
int raft_prop( struct raft_prop *prop );

#define RAFT_PROP_ELEC_LOW 0x0001
#define RAFT_PROP_ELEC_HIGH 0x0002
#define RAFT_PROP_TERM_LOW 0x0004
#define RAFT_PROP_TERM_HIGH 0x0008
#define RAFT_PROP_RPC_TIMEOUT 0x0010
#define RAFT_PROP_FLAGS 0x0020
int raft_prop_set( uint32_t mask, struct raft_prop *prop );

/* database functions */
int raft_cluster_list( struct raft_cluster *cl, int ncl );
int raft_clid_list( uint64_t *clid, int n );
int raft_cluster_by_clid( uint64_t clid, struct raft_cluster *cl );
int raft_cluster_set( struct raft_cluster *cl );
int raft_cluster_rem( uint64_t clid );

/* open command log */
int raft_log_open( uint64_t clid, struct log_s *log );

/* register rpc service */
void raft_register( void );

/* applications must register to process commands */
struct raft_app {
  struct raft_app *next;
  uint32_t appid;

  /* apply command to state machine */
  void (*command)( struct raft_app *app, struct raft_cluster *cl, uint64_t cmdseq, char *buf, int len );
};
int raft_app_register( struct raft_app *app );

#define RAFT_MAX_COMMAND (32*1024)
/* 
 * Start process of initiating command:
 * - save command buffer locally 
 * - distribute to remote nodes
 * - function returns here
 * - asynchronously, when quorum of nodes has received the command, the app->command 
 * callback is invoked 
 */
int raft_cluster_command( uint64_t clid, char *buf, int len, uint64_t *cseq );

int raft_command_seq( uint64_t clid, uint64_t *term, uint64_t *seq );

#endif

