
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/log.h>

/*
 * Add the following freg entries

/fju/rpc/services key
/fju/rpc/services/fred key
/fju/rpc/services/fred/path str /root/fju/lib/libsvctest.so
/fju/rpc/services/fred/mainfn str fred_register
/fju/rpc/services/fred/vers u32 1

 * then start rpcd with the -M option to load dynamic service modules
 * 
 */

static struct {
  struct log_s log;
} glob;

static int fred_proc_null( struct rpc_inc *inc ) {
  int handle;
  rpc_init_accept_reply( inc, inc->msg.xid, RPC_ACCEPT_SUCCESS, NULL, &handle );
  rpc_complete_accept_reply( inc, handle );
  return 0;
}

static struct rpc_proc fred_procs[] = {
  { 0, fred_proc_null },
  { 0, NULL }
};

static struct rpc_version fred_vers = {
  NULL, 1, fred_procs
};

static struct rpc_program fred_prog = {
  NULL, 1231231, &fred_vers
};

void fred_register( uint32_t vers ) {
  log_open( NULL, NULL, &glob.log );
  
  if( vers != 1 ) {
    log_writef( &glob.log, LOG_LVL_DEBUG, "fred_register: invalid vers %d", vers );
  }
  rpc_program_register( &fred_prog );
}
