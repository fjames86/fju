
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
};
  
struct fvm2_module;
struct fvm2_s {
  struct fvm2_module *module;

  uint32_t datasize;
  uint8_t *data;
  uint32_t textsize;
  uint8_t *text;
  uint32_t reg[FVM2_MAX_REG];
  uint8_t stack[FVM2_MAX_STACK];
  uint32_t frame;
};


int fvm2_module_load( char *filename );
int fvm2_module_unload( char *name );
int fvm2_module_list( struct fvm2_module_info *minfo, int n );
int fvm2_module_symbols( char *name, struct fvm2_symbol *sym, int n );


int fvm2_state_init( char *module, char *fname, char *args, int argsize, struct fvm2_s *state );
int fvm2_step( struct fvm2_s *state );
int fvm2_run( struct fvm2_s *state, int nsteps );
int fvm2_results( struct fvm2_s *state, char *results, int size, int *rsize );

int fvm2_read_var( char *module, char *vname, char *buf, int size );
int fvm2_write_var( char *module, char *vname, char *buf, int size );


#endif

