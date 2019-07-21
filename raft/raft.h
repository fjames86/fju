
#ifndef RAFT_H
#define RAFT_H

#include <stdint.h>

struct raft_member {
    uint64_t id;                     /* member identifier */
    uint64_t clid;                   /* cluster id */
    uint64_t hostid;                 /* host identifier */
    uint64_t lastseen;               /* last time received a msg */
    uint32_t flags;                  /* member state+flags */
#define RAFT_STATE_MASK      0x0007
#define RAFT_STATE_OFFLINE   0x0000     /* node offline */
#define RAFT_STATE_FOLLOWER  0x0001     /* we are a follower */
#define RAFT_STATE_CANDIDATE 0x0002     /* we are a candidate */
#define RAFT_STATE_LEADER    0x0003     /* we are leader */    
#define RAFT_MEMBER_VOTED    0x0008    /* true if this member voted for us this election */
#define RAFT_MEMBER_LOCAL    0x0010    /* true if this is local node */
    uint32_t spare[7];                 /* unused */
};

struct raft_cluster {
    uint64_t id;                       /* cluster identifier */
    uint64_t seq;                      /* cluster seqno */
    uint64_t voteid;                   /* member we voted for this election */
    uint64_t timeout;                   /* election/term timeout */
    uint32_t flags;                    /* cluster flags+state */
#define RAFT_CLUSTER_OFFLINE 0x0001     /* cluster offline */
    
    uint32_t spare[7];
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
};
int raft_prop( struct raft_prop *prop );
int raft_set_timeouts( uint32_t elec_low, uint32_t elec_high, uint32_t term_low, uint32_t term_high );

int raft_cluster_list( struct raft_cluster *cl, int n );
int raft_cluster_by_id( uint64_t clid, struct raft_cluster *cl );
int raft_cluster_add( struct raft_cluster *cl );
int raft_cluster_set( struct raft_cluster *cl );
int raft_cluster_rem( uint64_t clid );
int raft_cluster_online( uint64_t clid, int online );

int raft_member_list( uint64_t clid, struct raft_member *member, int n );
int raft_member_by_id( uint64_t mid, struct raft_member *member );
int raft_member_local( uint64_t clid, struct raft_member *member );
int raft_member_add( struct raft_member *member );
int raft_member_set( struct raft_member *member );
int raft_member_rem( uint64_t mid );


#endif

