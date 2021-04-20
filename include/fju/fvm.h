
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

struct fvm_perfdata {
  uint64_t nsteps;
  uint64_t rcount;
};

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
    struct fvm_perfdata perfdata;
  } procs[FVM_MAX_PROC];

  char *data;
  int datasize;
  char *text;
  int textsize;
  uint64_t timestamp;
  int tag;
  uint32_t flags;
#define FVM_MODULE_STATIC 0x0001 /* module cannot be unloaded */
#define FVM_MODULE_DISABLED 0x0002 /* module cannot run any procedures */
};


#define FVM_RELOAD 0x0001 
int fvm_module_load( char *buf, int size, uint32_t flags, struct fvm_module **modulep );
int fvm_module_load_file( char *filename, uint32_t flags, struct fvm_module **modulep );
int fvm_module_unload( char *modname );

/* low level routines */
int fvm_module_register( struct fvm_module *mod );
int fvm_module_unregister( struct fvm_module *module );

struct fvm_module *fvm_module_by_name( char *name ); /* lookup by name */
struct fvm_module *fvm_module_by_progid( uint32_t progid, uint32_t versid );  /* lookup by rpc prog */
struct fvm_module *fvm_module_by_tag( int tag ); /* lookup by tag */
int fvm_procid_by_name( struct fvm_module *module, char *procname ); /* lookup proc by name */
int fvm_procid_by_addr( struct fvm_module *module, int address );

/*
 * get a handle to a proc so it can be called later without saving the full modname/procname strings 
 * note the following: 
 * - handles are invalid once the module is unloaded. 
 * - handles cannot be passed between machines or persisted. 
 */
int fvm_handle_by_name( char *modname, char *procname, uint32_t *phandle );
int fvm_handle_by_procid( char *modname, int procid, uint32_t *phandle );

/* resolve a handle to module/procid so it can be called */
int fvm_proc_by_handle( uint32_t phandle, struct fvm_module **m, int *procid );

/* run the given proc with args receiving results */
int fvm_run( struct fvm_module *module, uint32_t procid, struct xdr_s *argbuf , struct xdr_s *resbuf );

/* register rpc interface */
void fvm_rpc_register( void );

/* clustering routines */
int fvm_cluster_run( uint64_t clid, char *modname, char *procname, char *args, int len );
int fvm_cluster_run2( uint64_t clid, char *modname, char *procname, char *args, int len, uint64_t tgt_hostid, uint64_t excl_hostid );
int fvm_cluster_updatestate( uint64_t clid, char *modname );

int fvm_module_enable( char *modname, int enable, int *prev );

#endif

