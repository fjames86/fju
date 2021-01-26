

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
 * Message ids are composed of a category (high 24 bits) and identifier (low 8 bits) 
 */
#define DMB_MSGID(category,msgid) (uint32_t)((((category) & 0x00ffffff) << 8) | ((msgid) & 0x000000ff))

/* open/close. Call open before any other functions. */
int dmb_open( void );
int dmb_close( void );

/* publish a message */
#define DMB_LOCAL 0x0001 /* Do not publish remotely */
#define DMB_REMOTE 0x0002 /* Do not publish locally */
int dmb_publish( uint32_t msgid, uint32_t flags, char *buf, int size, uint64_t *seq );

/* register a subscriber, optionally filtered  */
struct dmb_subscriber {
  struct dmb_subscriber *next;
  uint32_t mask; /* if msgid & mask then invoke callback. typically mask is msg category. 0 is interpreted as matching all msgs */
  void (*cb)( uint64_t hostid, uint64_t seq, uint32_t msgid, char *buf, int size );
};
int dmb_subscribe( struct dmb_subscriber *sc );
#define DMB_SUBSCRIBER(name,cat,cb) struct dmb_subscriber name = { NULL, cat, cb }

/*
 * Register an fvm procedure to receive messages. If msgid is zero the the procedure
 * has signature Proc(msgid : int, len : int, buf : opaque) and receives all messages. 
 * If non-zero the procedure receives the buffer as its args directly, and only for that specific message.
*/
int dmb_subscribe_fvm( char *modname, char *procname, uint32_t msgid );

/* 
 * Unsubscribe an fvm subscriber
 */
int dmb_unsubscribe_fvm( char *modname, char *procname );

int dmb_host_info( uint64_t hostid, uint64_t *lastid, uint64_t *seq );
void dmb_msginfo( uint64_t *hostid, uint64_t *seq, uint32_t *msgid, char **buf, uint32_t *len );

#endif

