/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
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
 
#ifndef RAFT_H
#define RAFT_H

#include <stdint.h>

struct raft_member {
    uint64_t clid;                     /* cluster id */
    uint64_t hostid;                   /* host identifier */
    uint64_t lastseen;                 /* last time received a msg */
    uint32_t flags;                    /* member state+flags */
#define RAFT_MEMBER_VOTED    0x0001    /* true if this member voted for us this election */
    uint32_t unused;
    uint64_t nextseq;                  /* (when leader only) seqno of next state to send to member */
    uint64_t stateseq;                 /* (when leader only) last known state ack'ed by member */
  
    uint32_t spare[4];                 /* future expansion */
};

struct raft_cluster {
    uint64_t id;                       /* cluster identifier */
    uint64_t leaderid;                 /* current leader, if any */
    uint64_t termseq;                  /* cluster term seqno */
    uint64_t voteid;                   /* member we voted for this election */
    uint64_t timeout;                  /* election/term timeout */
    uint32_t state;                    /* local state */
#define RAFT_STATE_FOLLOWER    0x0000  /* we are follower */
#define RAFT_STATE_CANDIDATE   0x0001  /* we are candidate */
#define RAFT_STATE_LEADER      0x0002  /* we are leader */
    uint32_t flags;                    /* cluster flags */
#define RAFT_CLUSTER_WITNESS   0x0001  /* follower only, never become candidate/leader */
    uint32_t votes;                    /* number of votes received this election */
    uint32_t typeid;                   /* custom field to store a cluster type identifier */  
    uint64_t commitseq;                /* seqno of last state commited to storage */
    uint64_t stateseq;                 /* seqno of last state applied to state machine */
    uint64_t stateterm;                /* term seqno of last state applied to state machine */
  
    uint32_t spare[12];                /* future expansion */
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
int raft_member_set( struct raft_member *member );
int raft_member_rem( uint64_t clid, uint64_t hostid );
int raft_member_clear_voted( uint64_t clid );

#define RAFT_RPC_PROG 0x27E1FAEF
#define RAFT_RPC_VERS 1
void raft_register( void );

typedef void (*raft_notify_t)( struct raft_cluster *cl, void *cxt );
struct raft_notify_context {
  struct raft_notify_context *next;
  raft_notify_t cb;
  void *cxt;
};
void raft_notify_register( struct raft_notify_context *ncxt );

#endif

