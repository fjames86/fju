
#include <fju/rpc.h>
#include <fju/log.h>
#include <fju/fvm.h>
#include "../fvm-private.h"

static struct fvm_module mod;

static int native_procnull( struct xdr_s *args, struct xdr_s *res ) {
  log_writef( NULL, LOG_LVL_INFO, "native_procnull" );
  return 0;
}
static int native_proclist( struct xdr_s *args, struct xdr_s *res ) {
  log_writef( NULL, LOG_LVL_INFO, "native_proclist" );
  return 0;
}
static int native_init( struct xdr_s *args, struct xdr_s *res ) {
  log_writef( NULL, LOG_LVL_INFO, "native_init" );
  return 0;
}
static int native_exit( struct xdr_s *args, struct xdr_s *res ) {
  log_writef( NULL, LOG_LVL_INFO, "native_exit" );
  return 0;
}

static void read_pars( struct fvm_state *state, uint32_t *pars, int n ) {
  int i;
  for( i = 0; i < n; i++ ) {
    pars[n - i - 1] = fvm_stack_read( state, 4 + 4*i );
  }
}

static int native_sc_cb( struct fvm_syscall *sc, struct fvm_state *state ) {
  uint32_t pars[1];

  log_writef( NULL, LOG_LVL_INFO, "native_sc_cb" );
  
  read_pars( state, pars, 1 );
  fvm_write_u32( state, pars[0], 321 );
  
  return 0;
}

static struct fvm_syscall native_sc = { NULL, 1000, native_sc_cb };

void test_native_register( void ) {
  fvm_syscall_register( &native_sc );
  
  strcpy( mod.name, "Native" );
  mod.progid = 0;
  mod.versid = 0;
  mod.nprocs = 4;
  strcpy( mod.procs[0].name, "ProcNull" );
  mod.procs[0].siginfo = 0x0LL;
  mod.procs[0].nativeproc = native_procnull;
  strcpy( mod.procs[1].name, "ProcList" );
  mod.procs[1].nativeproc = native_proclist;
  mod.procs[1].siginfo = 0x400000000000034LL;
  strcpy( mod.procs[2].name, "Init" );
  mod.procs[2].siginfo = 0x0LL;
  mod.procs[2].nativeproc = native_init;    
  strcpy( mod.procs[3].name, "Exit" );
  mod.procs[3].siginfo = 0x0LL;
  mod.procs[3].nativeproc = native_exit;
  
  mod.timestamp = 1619425897LL;
  mod.flags = FVM_MODULE_STATIC|FVM_MODULE_NATIVE;
  fvm_module_register( &mod );
}
