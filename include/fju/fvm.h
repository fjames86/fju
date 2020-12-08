
#ifndef FVM_H
#define FVM_H

#include <stdint.h>
#include <fju/rpc.h>

#define FVM_MAX_NAME 64
#define FVM_MAX_PROC 32

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
    uint32_t siginfo;
  } procs[FVM_MAX_PROC];

  char *data;
  int datasize;
  char *text;
  int textsize;
};


int fvm_module_load( char *buf, int size, struct fvm_module **modulep );
int fvm_module_load_file( char *filename, struct fvm_module **modulep );
int fvm_module_unload( char *modname );

struct fvm_module *fvm_module_by_name( char *name );
struct fvm_module *fvm_module_by_progid( uint32_t progid, uint32_t versid );
int fvm_procid_by_name( struct fvm_module *module, char *procname );

int fvm_run( struct fvm_module *module, uint32_t procid, struct xdr_s *argbuf , struct xdr_s *resbuf );

/* register rpc interface */
void fvm_rpc_register( void );


/* 
 * Issue a clustered command which runs the specified procedure 
 * clid ::= cluster id or 0 for default first cluster with appid=FVM_RPC_PROG
 * progid,procid,args,len ::= procedure and args to run 
 */
//int fvm_cluster_run( uint64_t clid, uint32_t progid, uint32_t procid, char *args, int len );

#endif

