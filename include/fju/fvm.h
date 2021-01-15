
#ifndef FVM_H
#define FVM_H

#include <stdint.h>
#include <fju/rpc.h>

#define FVM_MAX_NAME 64
#define FVM_MAX_PROC 32
#define FVM_MAX_PARAM 19
#define FVM_SIGINFO_NARGS(siginfo) ((uint32_t)(((siginfo) >> 57) & 0x1f))
#define FVM_SIGINFO_VARTYPE(siginfo,i) ((uint32_t)((siginfo >> (3*(i))) & 0x3))
#define FVM_SIGINFO_ISVAR(siginfo,i) ((uint32_t)((siginfo >> (3*(i))) & 0x4))

#define FVM_ADDR_DATA   0x1000
#define FVM_ADDR_TEXT   0x5000
#define FVM_ADDR_STACK  0xc000

#define FVM_MAX_DATA    0x4000
#define FVM_MAX_TEXT    0x8000
#define FVM_MAX_STACK   0x4000

struct fvm_module {
  struct fvm_module *next;

  char name[FVM_MAX_NAME];
  uint32_t progid;
  uint32_t versid;
  uint32_t nprocs;
  struct {
    char name[FVM_MAX_NAME];
    uint32_t address;
    uint64_t siginfo;
  } procs[FVM_MAX_PROC];

  char *data;
  int datasize;
  char *text;
  int textsize;
  uint64_t timestamp;
  int tag;
};


#define FVM_RELOAD 0x0001 
int fvm_module_load( char *buf, int size, uint32_t flags, struct fvm_module **modulep );
int fvm_module_load_file( char *filename, uint32_t flags, struct fvm_module **modulep );
int fvm_module_unload( char *modname );

struct fvm_module *fvm_module_by_name( char *name );
struct fvm_module *fvm_module_by_progid( uint32_t progid, uint32_t versid );
struct fvm_module *fvm_module_by_tag( int tag );
int fvm_procid_by_name( struct fvm_module *module, char *procname );

int fvm_handle_by_name( char *modname, char *procname, uint32_t *phandle );
int fvm_proc_by_handle( uint32_t phandle, struct fvm_module **m, int *procid );

int fvm_run( struct fvm_module *module, uint32_t procid, struct xdr_s *argbuf , struct xdr_s *resbuf );

/* register rpc interface */
void fvm_rpc_register( void );

int fvm_cluster_run( uint64_t clid, char *modname, char *procname, char *args, int len );
int fvm_cluster_run2( uint64_t clid, char *modname, char *procname, char *args, int len, uint64_t tgt_hostid, uint64_t excl_hostid );
int fvm_cluster_updatestate( uint64_t clid, char *modname );

#endif

