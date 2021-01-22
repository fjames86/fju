
#ifndef DLM_H
#define DLM_H

/* open/close library */
int dlm_open( void );
int dlm_close( void );

struct dlm_lock {
  uint64_t lockid; /* lock identifier */
  uint32_t mode;   /* lock mode */
#define DLM_WAIT 0 /* waiting to acquire lock */
#define DLM_EX 1   /* lock acquired in exclusive mode */
#define DLM_SH 2   /* lock acquired in shared mode */
#define DLM_RELEASE 3
  uint32_t seq;    /* seqno - must be incremented every second to keep lock. otherwise it will be automatically released */
  uint64_t resid;  /* resource identifier - application specific id for locked resource */
  
#define DLM_MAX_COOKIE 8
  char cookie[DLM_MAX_COOKIE];  /* private data associated with lock by application */
};
int dlm_list( struct dlm_lock *dl, int ndl );

/* 
 * Attempt to acquire the lock for a given resource. 
 * If lock for resid not already held then lockid is set with mode=DLM_WAIT 
 * When the lock is acquired the state is set to mode (EX or SH) and, if set, the donecb is invoked 
 */

typedef void (*dlm_donecb_t)( struct dlm_lock *dl, void *cxt );
int dlm_acquire( uint64_t resid, uint32_t mode, char *cookie, uint64_t *lockid, dlm_donecb_t cb, void *cxt );

/* must call this every second to keep lock held */
int dlm_renew( uint64_t lockid );

/* release the lock */
int dlm_release( uint64_t lockid );


#endif

