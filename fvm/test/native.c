
/*
 * This file tests two separate things:
 * 1. It implements an fvm module in C. This is a module which fits
 * into the FVM framework so that from the outside it appears to be written
 * in fvm pascal. But the actual procedures are really implemented in C. This 
 * makes it easy to e.g. do cross-calls into C code from FVM.
 * 2. It implements an fvm syscall provider. This allows defining custom system calls 
 * not impleemnted by the base fvm-syscall.c framework. The syscall id still needs to be 
 * hardcoded so we need to keep a registry somewhere of who has what syscall id allocated.
 */

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
static int native_testproc( struct xdr_s *args, struct xdr_s *res ) {
  uint32_t x;
  int sts;
  
  log_writef( NULL, LOG_LVL_INFO, "native_testproc" );
  sts = xdr_decode_uint32( args, &x );
  if( sts ) return -1;
  
  xdr_encode_uint32( res, x + 1 );
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
  strcpy( mod.procs[4].name, "TestProc" );
  mod.procs[4].siginfo = 0;
  FVM_SIGINFO_SETPARAM(mod.procs[4].siginfo,0,VAR_TYPE_U32,0);
  FVM_SIGINFO_SETPARAM(mod.procs[4].siginfo,1,VAR_TYPE_U32,1);
  FVM_SIGINFO_SETNPARS(mod.procs[4].siginfo,2);
  mod.procs[4].nativeproc = native_testproc;
  
  mod.timestamp = 1619425897LL;
  mod.flags = FVM_MODULE_STATIC|FVM_MODULE_NATIVE;
  fvm_module_register( &mod );
}
