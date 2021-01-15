

#ifndef DMB_H
#define DMB_H

#include <stdint.h>

/* Maximum message buffer size */
#define DMB_MAX_MSG (16*1024)

/* message ids are composed of a category (high 16 bits) and identifier (low 16 bits) */
#define DMB_MSGID(category,msgid) (uint32_t)((((category) & 0xffff) << 16) | ((msgid) & 0x0000ffff))

/* open/close */
int dmb_open( void );
int dmb_close( void );

/* publish a message */
#define DMB_LOCAL 0x0001 /* Do not publish remotely */
#define DMB_REMOTE 0x0002 /* Do not publish locally */
int dmb_publish( uint32_t msgid, uint32_t flags, char *buf, int size );

/* register a subscriber, optionally filtered on category */
struct dmb_subscriber {
  struct dmb_subscriber *next;
  uint32_t category; /* message category. if non-zero only invoked on matching category, otehrwise receives all messges */
  void (*cb)( uint64_t hostid, uint32_t msgid, char *buf, int size );
};
int dmb_subscribe( struct dmb_subscriber *sc );

/* register a subscriber that is implemented as an fvm procedure */
/* MUST have signature Proc(hostHigh : int, hostLow : int, msgid : int, flags : int, len : int, buf : opaque) */
int dmb_subscribe_fvm( char *modname, char *procname, uint32_t category );


#endif

