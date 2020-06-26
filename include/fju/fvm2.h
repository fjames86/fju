
#ifndef FVM2_H
#define FVM2_H

#define FVM2_MAX_NAME 64

#define FVM2_ADDR_DATA   0x1000
#define FVM2_ADDR_TEXT   0x5000
#define FVM2_ADDR_STACK  0xc000

#define FVM2_MAX_DATA    0x4000
#define FVM2_MAX_TEXT    0x8000
#define FVM2_MAX_STACK   0x4000
#define FVM2_MAX_REG     16

struct fvm2_symbol {
  char name[FVM2_MAX_NAME];
  uint32_t addr;
};

struct fvm2_module_info {
  char name[FVM2_MAX_NAME];
  uint8_t checksum[20];
  uint32_t datasize;
  uint32_t textsize;
  uint32_t progid;
  uint32_t versid;
};
  
struct fvm2_module;
struct fvm2_s {
  struct fvm2_module *module;

  uint32_t datasize;
  uint8_t *data;
  uint32_t textsize;
  uint8_t *text;
  uint32_t reg[FVM2_MAX_REG];
  int32_t frame;
  uint8_t stack[FVM2_MAX_STACK];
};


/* register a module from memory. if non-null name receives module name */
int fvm2_module_register( char *buf, int size, char *name );
/* load and register a module from a file */
int fvm2_module_load( char *filename, char *name );
/* unload a given module */
int fvm2_module_unload( char *name );

/* list modules and symbols */
int fvm2_module_list( struct fvm2_module_info *minfo, int n );
int fvm2_module_symbols( char *name, struct fvm2_symbol *sym, int n );

/* initialize runtime state */
int fvm2_state_init( struct fvm2_s *state, uint32_t progid, uint32_t procid );

/* exeucute a single step */
int fvm2_step( struct fvm2_s *state );
/* execute until termination up to a maximum number of steps */
int fvm2_run( struct fvm2_s *state, int nsteps );

/* register this module as an rpc program */
int fvm2_register_program( char *mname );
int fvm2_unregister_program( char *mname );

/* register rpc interface */
void fvm2_rpc_register( void );

#endif

