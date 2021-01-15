

#ifndef DMB_H
#define DMB_H


/*
 * DMB: "distributed message bus".
 * Provides a mechanism to send messages (uint32_t id and an opaque buffer) to one or more hosts.
 * Delivery is guaranteed, subject to sufficient log space - if log wraps before message delivered then
 * the message is lost.
 * 
 * The API for sending a message (dmb_publish) is low latency and asynchronous - all it does is write 
 * an entry into a local log file. This may be done either in or out of process.
 * The service periodically polls the log to detect new messages. When a new message is appended to the log 
 * it is sent to all registered remote hosts. 
 *
 * Receivers ("subscribers") may be registered as either C callbacks or fvm procedures. These receive the message
 * and the host it originated from. 
 */

#include <stdint.h>

/* Maximum message buffer size */
#define DMB_MAX_MSG (16*1024)

/* 
 * Message ids are composed of a category (high 16 bits) and identifier (low 16 bits) 
 * Subscribers may filter on a given category.
 */
#define DMB_MSGID(category,msgid) (uint32_t)((((category) & 0xffff) << 16) | ((msgid) & 0x0000ffff))

/* open/close. Call open before any other functions. */
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

/*
 * Register a subscriber that is implemented as an fvm procedure.
 * A given modname/procname may only be registered once. Subsequent fvm_subscribe_fvm calls 
 * with the same procname will only change the category filter.
 * MUST have signature Proc(hostHigh : int, hostLow : int, msgid : int, len : int, buf : opaque) 
*/
int dmb_subscribe_fvm( char *modname, char *procname, uint32_t category );

/* 
 * Unsubscribe an fvm subscriber
 */
int dmb_unsubscribe_fvm( char *modname, char *procname );

#endif

