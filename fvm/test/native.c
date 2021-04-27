
#include <fju/rpc.h>
#include <fju/log.h>
#include <fju/fvm.h>

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


void test_native_register( void ) {
  strcpy( mod.name, "NativeTest" );
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
