
#ifndef DLM_H
#define DLM_H

/* open/close library */
int dlm_open( void );
int dlm_close( void );

struct dlm_lock {
  uint64_t lockid;      /* lock identifier */
  uint64_t hostid;      /* host that acquired the lock */
  uint64_t resid;       /* opaque resource identifier */
  uint32_t state;       /* lock state */
#define DLM_EX      1   /* lock acquired in exclusive mode */
#define DLM_SH      2   /* lock acquired in shared mode */
#define DLM_BLOCKEX 3   /* blocked waiting to acqurie exclsively */
#define DLM_BLOCKSH 4   /* blocked waiting to acquire shared */
  uint32_t spare;
};
int dlm_list( struct dlm_lock *dl, int ndl );
int dlm_lock_by_lockid( uint64_t lockid, struct dlm_lock *lock );

/* 
 * Attempt to acquire the lock for a given resource. 
 * If lock for resid not already held then lockid is set with mode=DLM_WAIT 
 * When the lock is acquired the state is set to mode (EX or SH) and, if set, the donecb is invoked 
 */

typedef enum {
    DLM_LOCKSTAT_FAIL = 0,
    DLM_LOCKSTAT_BLOCKED = 1,
    DLM_LOCKSTAT_ACQUIRED = 2,
} dlm_lockstat_t;

typedef void (*dlm_donecb_t)( uint64_t lockid, dlm_lockstat_t stat, void *cxt );

/*
 * Enqueue a request to acquire a lock. 
 * resid ::= resource id
 * shared ::= if true a shared lock is acquired, otherwise an exclusive lock 
 * lockid ::= receives the lockid allocated 
 * cb ::= callback invoked when lock acquired 
 * cxt ::= private data passed to cb 
 */
int dlm_acquire( uint64_t resid, int shared, uint64_t *lockid, dlm_donecb_t cb, void *cxt );

/* release the lock */
int dlm_release( uint64_t lockid );


#endif

