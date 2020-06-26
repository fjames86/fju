
#ifndef FVM_H
#define FVM_H

#include <stdint.h>

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
};

struct fvm_module_info {
  char name[FVM_MAX_NAME];
  uint8_t checksum[20];
  uint32_t datasize;
  uint32_t textsize;
  uint32_t progid;
  uint32_t versid;
};
  
struct fvm_module;
struct fvm_s {
  struct fvm_module *module;

  uint32_t datasize;
  uint8_t *data;
  uint32_t textsize;
  uint8_t *text;
  uint32_t reg[FVM_MAX_REG];
  int32_t frame;
  uint8_t stack[FVM_MAX_STACK];
};


/* register a module from memory. if non-null name receives module name */
int fvm_module_register( char *buf, int size, char *name );
/* load and register a module from a file */
int fvm_module_load( char *filename, char *name );
/* unload a given module */
int fvm_module_unload( char *name );

/* list modules and symbols */
int fvm_module_list( struct fvm_module_info *minfo, int n );
int fvm_module_symbols( char *name, struct fvm_symbol *sym, int n );

/* initialize runtime state */
int fvm_state_init( struct fvm_s *state, uint32_t progid, uint32_t procid );

/* exeucute a single step */
int fvm_step( struct fvm_s *state );
/* execute until termination up to a maximum number of steps */
int fvm_run( struct fvm_s *state, int nsteps );

/* register this module as an rpc program */
int fvm_register_program( char *mname );
int fvm_unregister_program( char *mname );

/* register rpc interface */
void fvm_rpc_register( void );

#endif

