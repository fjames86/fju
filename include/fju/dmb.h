

#ifndef DMB_H
#define DMB_H

#include <stdint.h>

int dmb_open( void );
int dmb_close( void );

int dmb_publish( uint32_t msgid, uint32_t flags, char *buf, int size );

struct dmb_subscriber {
  struct dmb_subscriber *next;
  void (*cb)( uint64_t hostid, uint64_t seq, uint32_t msgid, char *buf, int size );
};
int dmb_subscribe( struct dmb_subscriber *sc );

int dmb_subscribe_fvm( char *modname, char *procname );


#endif

