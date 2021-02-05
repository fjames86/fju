
#ifndef FSM_H
#define FSM_H

#include <stdint.h>

int fsm_open( void );

#define FSM_MAX_NAME 64
struct fsm_info {
  char name[FSM_MAX_NAME];
  uint64_t fsmid;
};
int fsm_list( struct fsm_info *info, int n );
int fsm_create( char *name, uint64_t *fsmidp );
int fsm_delete( uint64_t fsmid );

int fsm_command_save( uint64_t fsmid, struct log_iov *iov, int niov, uint64_t *seq );
int fsm_command_load( uint64_t fsmid, uint64_t seq, struct log_iov *iov, int niov );

struct fsm_command_info {
  uint64_t seq;
  uint64_t when;
  uint32_t len;
};
int fsm_command_list( uint64_t fsmid, struct fsm_command_info *clist, int n );

int fsm_command_truncate( uint64_t fsmid, uint64_t seq );

int fsm_snapshot_save( uint64_t fsmid, uint64_t seq, char *buf, int len, int offset );

struct fsm_snapshot_info {
  uint64_t seq;
  uint32_t len;
};
int fsm_snapshot_load( uint64_t fsmid, struct log_iov *iov, int niov, struct fsm_snapshot_info *info );

#endif

