
#ifndef RAFT_H
#define RAFT_H

#include <stdint.h>

struct raft_member {
    uint64_t clid;                   /* cluster id */
    uint64_t hostid;                 /* host identifier */
    uint64_t lastseen;               /* last time received a msg */
    uint32_t flags;                  /* member state+flags */
#define RAFT_MEMBER_LOCAL    0x0001
#define RAFT_MEMBER_VOTED    0x0002    /* true if this member voted for us this election */
    uint32_t spare[7];                 /* unused */
};

struct raft_cluster {
    uint64_t id;                       /* cluster identifier */
    uint64_t leaderid;
    uint64_t seq;                      /* cluster seqno */
    uint64_t voteid;                   /* member we voted for this election */
    uint64_t timeout;                   /* election/term timeout */
    uint32_t state;
#define RAFT_STATE_FOLLOWER    0x0000   /* we are follower */
#define RAFT_STATE_CANDIDATE   0x0001   /* we are candidate */
#define RAFT_STATE_LEADER      0x0002   /* we are leader */
    uint32_t flags;                    /* cluster flags+state */
    uint32_t votes;
    uint32_t spare[5];
};

int raft_open( void );
int raft_close( void );
int raft_reset( void );

struct raft_prop {
    uint32_t version;
#define RAFT_VERSION 1
    uint64_t seq;
    uint32_t cluster_max;
    uint32_t cluster_count;
    uint32_t member_max;
    uint32_t member_count;    
    uint32_t elec_low;
    uint32_t elec_high;
    uint32_t term_low;
    uint32_t term_high;
    uint32_t rpc_timeout;
};
int raft_prop( struct raft_prop *prop );
int raft_set_timeouts( uint32_t *elec_low, uint32_t *elec_high, uint32_t *term_low, uint32_t *term_high );
int raft_set_rpc_timeout( uint32_t rpc_timeout );

int raft_cluster_list( struct raft_cluster *cl, int n );
int raft_cluster_by_id( uint64_t clid, struct raft_cluster *cl );
int raft_cluster_set( struct raft_cluster *cl );
int raft_cluster_rem( uint64_t clid );
int raft_cluster_quorum( uint64_t clid );

int raft_member_list( uint64_t clid, struct raft_member *member, int n );
int raft_member_by_hostid( uint64_t clid, uint64_t hostid, struct raft_member *member );
int raft_member_local( uint64_t clid, struct raft_member *member );
int raft_member_leader( uint64_t clid, struct raft_member *member );
int raft_member_set( struct raft_member *member );
int raft_member_rem( uint64_t clid, uint64_t hostid );
int raft_member_clear_voted( uint64_t clid );

#define RAFT_RPC_PROG 0x27E1FAEF
#define RAFT_RPC_VERS 1
void raft_register( void );

#endif

