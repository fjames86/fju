
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/log.h>
#include <fju/programs.h>

/*
 * Add the following freg entries

/fju/rpc/services key
/fju/rpc/services/fred key
/fju/rpc/services/fred/path str /root/fju/lib/libsvctest.so
/fju/rpc/services/fred/mainfn str fred_register

 * then start rpcd with the -M option to load dynamic service modules
 * 
 */

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
  NULL, SVCTEST_RPC_PROG, &fred_vers
};

void fred_register( void ) {
  log_writef( NULL, LOG_LVL_DEBUG, "fred_register: loading svctest example program fred" );
  rpc_program_register( &fred_prog );
}
