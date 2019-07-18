
#ifndef NLS_PRIVATE_H
#define NLS_PRIVATE_H

/* table sizes */
#define NLS_MAX_SHARE 32
#define NLS_MAX_REMOTE 32
#define NLS_MAX_NOTIFY 32

#define NLS_RPC_TIMEOUT  1000       /* milli seocnds to wait for an rpc reply */
#define NLS_NOTREG_PERIOD 300       /* seconds to wait since last notification. by default set to 5 minutes */
#define NLS_NOTIFY_PERIOD 1         /* seconds to wait between polling local seqno */

#endif

