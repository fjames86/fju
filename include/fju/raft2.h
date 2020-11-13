
#ifndef RAFT2_H
#define RAFT2_H

#include <stdint.h>

#define RAFT2_MAX_MEMBER  8
#define RAFT2_MAX_CLUSTER 32

struct raft2_member {
  uint64_t hostid;      /* host identifier */
  uint64_t lastseen;    /* when last received a message from this host */
  uint64_t seq;         /* (leader only) last seq acked by member */
  uint32_t flags;       /* member flags */
#define RAFT2_MEMBER_VOTED  0x0001    /* (leader only) received vote this election */
  
  uint32_t spare[5];
};

struct raft2_cluster {
  uint64_t clid;         /* cluster identifier */
  uint64_t leaderid;     /* current leader. 0 indicates no leader */
  uint64_t voteid;       /* who we voted for this election */
  uint64_t term;         /* current term seqno */
  uint64_t seq;          /* state seqno */
  uint64_t commitseq;    /* highest seq command saved locally */
  uint64_t timeout;      /* when current term/election ends */
  uint32_t state;        /* current state */
#define RAFT2_STATE_FOLLOWER     0
#define RAFT2_STATE_CANDIDATE    1
#define RAFT2_STATE_LEADER       2
  
  /* members */
  uint32_t nmember;
  struct raft2_member member[RAFT2_MAX_MEMBER];

  uint32_t appid;
  uint32_t flags;
#define RAFT2_CLUSTER_WITNESS 0x0001
  
  uint32_t spare[2];
};

struct raft2_prop {
  uint32_t magic;
#define RAFT2_MAGIC 0x22defc03
  uint32_t version;
#define RAFT2_VERSION 1
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

int raft2_open( void );
int raft2_close( void );
int raft2_prop( struct raft2_prop *prop );

#define RAFT2_PROP_ELEC_LOW 0x0001
#define RAFT2_PROP_ELEC_HIGH 0x0002
#define RAFT2_PROP_TERM_LOW 0x0004
#define RAFT2_PROP_TERM_HIGH 0x0008
#define RAFT2_PROP_RPC_TIMEOUT 0x0010
#define RAFT2_PROP_FLAGS 0x0020
int raft2_prop_set( uint32_t mask, struct raft2_prop *prop );

/* database functions */
int raft2_cluster_list( struct raft2_cluster *cl, int ncl );
int raft2_clid_list( uint64_t *clid, int n );
int raft2_cluster_by_clid( uint64_t clid, struct raft2_cluster *cl );
int raft2_cluster_set( struct raft2_cluster *cl );
int raft2_cluster_rem( uint64_t clid );

/* register rpc service */
void raft2_register( void );

/* applications must register to process commands */
struct raft2_app {
  struct raft2_app *next;
  uint32_t appid;

  /* apply command to state machine */
  void (*command)( struct raft2_cluster *cl, char *buf, int len );
};
int raft2_app_register( struct raft2_app *app );

/* 
 * Start process of initiating command:
 * - save command buffer locally 
 * - send command buffer to follower nodes
 * - wait for quorum of nodes has acknowledged receipt of command
 * - increment seqno and send ping rpc to followers
 * - apply command 
 */
int raft2_cluster_command( uint64_t clid, char *buf, int len );

#endif

