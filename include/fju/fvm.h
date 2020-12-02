
#ifndef FVM_H
#define FVM_H

#include <stdint.h>
#include <fju/rpc.h>

#define FVM_MAX_NAME 64

#define FVM_ADDR_DATA   0x1000
#define FVM_ADDR_TEXT   0x5000
#define FVM_ADDR_STACK  0xc000

#define FVM_MAX_DATA    0x4000
#define FVM_MAX_TEXT    0x8000
#define FVM_MAX_STACK   0x4000
#define FVM_MAX_REG     16

struct fvm_symbol {
  char name[FVM_MAX_NAME];
  uint32_t addr;
  uint32_t flags;
#define FVM_SYMBOL_SIZE_MASK 0x0000ffff
#define FVM_SYMBOL_TYPE_MASK 0x000f0000
#define FVM_SYMBOL_PROC      0x00000000
#define FVM_SYMBOL_STRING    0x00010000
#define FVM_SYMBOL_UINT32    0x00020000
#define FVM_SYMBOL_UINT64    0x00030000
#define FVM_SYMBOL_FLAG_MASK 0xfff00000
};

struct fvm_module_info {
  uint32_t progid;
  uint32_t versid;
  char name[FVM_MAX_NAME];
  uint32_t datasize;
  uint32_t textsize;
  uint32_t flags;
#define FVM_MODULE_PERSISTENT   0x0002 /* save data segment after each run */
  uint64_t utime;
};
  
struct fvm_module;
struct fvm_s {
  struct fvm_module *module;

  uint32_t flags;
#define FVM_STATE_DIRTY        0x0001      /* data section was written to */
#define FVM_STATE_YIELD        0x0002      /* state yielded */
  uint32_t datasize;
  uint8_t *data;
  uint32_t textsize;
  uint8_t *text;
  uint32_t reg[FVM_MAX_REG];
  int32_t frame;
  uint32_t nsteps;
  uint32_t timeout;
  uint8_t stack[FVM_MAX_STACK];
};


/* register a module from memory. if non-null name receives module name */
int fvm_module_load_buffer( char *buf, int size, uint32_t *progid );
/* load and register a module from a file */
int fvm_module_load_file( char *filename, uint32_t *progid );
/* unload a given module */
int fvm_module_unload( uint32_t progid );

/* list modules and symbols */
int fvm_module_info( uint32_t progid, struct fvm_module_info *minfo );
int fvm_module_list( struct fvm_module_info *minfo, int n );
int fvm_module_symbols( uint32_t progid, struct fvm_symbol *sym, int n );
uint32_t fvm_procid_by_name( uint32_t progid, char *procname );
int fvm_module_set_flags( uint32_t progid, uint32_t flags, uint32_t mask );

/* initialize runtime state */
int fvm_state_init( struct fvm_s *state, uint32_t progid, uint32_t procid );
int fvm_set_args( struct fvm_s *state, char *buf, int len );
int fvm_set_args2( struct fvm_s *state, struct xdr_s *bufs, int nbuf );
int fvm_get_res( struct fvm_s *state, char **buf );

/* exeucute a single step */
int fvm_step( struct fvm_s *state );

/* execute until termination up to a maximum number of steps */
int fvm_run( struct fvm_s *state, int nsteps );

/* register rpc interface */
void fvm_rpc_register( void );



/* ----------------- clustering ---------- */

/*
 * To use fvm clustering: 
 * - allocate a raft cluster and set its appid=FVM_RPC_PROG
 * - either: 
 * - call fvm_cluster_run to request a given prog/proc is run across all nodes in the cluster
 * - call fvm_cluster_updatestate to distribute the current local data segment to all nodes in the cluster
 *
 */

/* 
 * Issue a clustered command which distributes the local data segment to all nodes 
 * clid ::= cluster id or 0 for default first cluster with appid=FVM_RPC_PROG
 * progid ::= module progid to distribute
*/
int fvm_cluster_updatestate( uint64_t clid, uint32_t progid );

/* 
 * Issue a clustered command which runs the specified procedure 
 * clid ::= cluster id or 0 for default first cluster with appid=FVM_RPC_PROG
 * progid,procid,args,len ::= procedure and args to run 
 */
int fvm_cluster_run( uint64_t clid, uint32_t progid, uint32_t procid, char *args, int len );

#endif

