
/*
 * This file was generated by cfgen.lisp

(CFGEN:GEN ("raft") NIL
           (("cluster" (("currentterm" :UINT64 NIL) ("votedfor" :UINT64 NIL)))
            ("member"
             (("clid" :UINT64 NIL) ("lastseen" :UINT64 NIL)
              ("nextping" :UINT64 NIL) ("nextidx" :UINT64 NIL)
              ("matchidx" :UINT64 NIL) ("flags" :UINT32 NIL)))))

 *
 */

#ifndef RAFT_H
#define RAFT_H

#include <stdint.h>

#define RAFT_RPC_PROGRAM 1231231
#define RAFT_RPC_VERSION 1

#define RAFT_MAX_CLUSTER_MEMBER 8    /* max members per cluster */

struct raft_cluster {
    uint64_t clid;
    uint64_t currentterm;
    uint64_t votedfor;
    uint64_t leaderid;   /* member id of current leader */
    uint64_t logid;      /* id of last log item written */
};

struct raft_member {
    uint64_t memberid;     /* locally unique member id */
    uint64_t clid;         /* cluster id */
    uint64_t hostid;       /* hostreg host id */
    uint64_t lastseen;     /* when last message received */
    uint64_t nextping;     /* when to next talk */
  
    uint64_t nextidx;
    uint64_t matchidx;
    uint32_t flags;
#define RAFT_MEMBER_ONLINE    0x0001
#define RAFT_MEMBER_LEADER    0x0002
#define RAFT_MEMBER_FOLLOWER  0x0004
#define RAFT_MEMBER_CANDIDATE 0x0006
#define RAFT_MEMBER_STATEMASK 0x0006
};


struct raft_prop {
    uint32_t version;
#define RAFT_VERSION 1
    uint64_t seq;
    uint32_t cluster_max;
    uint32_t cluster_count;
    uint32_t member_max;
    uint32_t member_count;
};

int raft_open( void );
int raft_close( void );
int raft_prop( struct raft_prop *prop );
int raft_reset( void );

int raft_cluster_list( struct raft_cluster *list, int n );
int raft_cluster_by_id( uint64_t clid, struct raft_cluster *entry );
int raft_cluster_add( struct raft_cluster *entry );
int raft_cluster_rem( uint64_t clid );
int raft_cluster_set( struct raft_cluster *entry );

int raft_member_list( struct raft_member *list, int n );
int raft_member_list_by_clid( uint64_t clid, struct raft_member *memberlist, int n );
int raft_member_list_by_hostid( uint64_t hostid, struct raft_member *memberlist, int n );
int raft_member_by_id( uint64_t id, struct raft_member *entry );
int raft_member_add( struct raft_member *entry );
int raft_member_rem( uint64_t id );
int raft_member_set( struct raft_member *entry );
int raft_member_set_state( uint64_t memberid, uint32_t state );
uint64_t raft_member_by_hostid( uint64_t clid, uint64_t hostid );
int raft_member_set_nextping( uint64_t memberid, uint64_t nextping );

int raft_register( void );

#endif

