
#ifndef FVM_PRIVATE_H
#define FVM_PRIVATE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <fju/fvm.h>

#define FVM_MAX_OPCODE 256

#define FVM_FLAG_POS  0x1
#define FVM_FLAG_NEG  0x2
#define FVM_FLAG_ZERO 0x4

struct fvm_module;

struct fvm_header {
  uint32_t magic;
#define FVM_MAGIC    0xf412a91c
  uint32_t version;
#define FVM_VERSION  1
  uint32_t flags; /* header flags */
  uint32_t progid;
  uint32_t versid;  
  uint32_t symcount;
  uint32_t datasize;
  uint32_t textsize;
  char name[FVM_MAX_NAME];
  
  uint32_t spare[8];
};

struct fvm_module {
  struct fvm_module *next;
  
  struct fvm_header header; /* module header */
  struct fvm_symbol *symbols; /* list of module symbols */
  uint8_t *data; /* module data segment */
  uint8_t *text; /* module text segment */
  
  uint64_t clusterid; /* non-zero if clustered */
  uint32_t flags; /* module flags */
  char path[256]; /* path to module file, if any */
  uint64_t utime; /* total user runtime in ms */
};

typedef enum {
	      FVM_REG_R0 = 0,
	      FVM_REG_R1 = 1,
	      FVM_REG_R2 = 2,
	      FVM_REG_R3 = 3,
	      FVM_REG_R4 = 4,
	      FVM_REG_R5 = 5,
	      FVM_REG_R6 = 6,
	      FVM_REG_R7 = 7,
	      FVM_REG_SP = 8,
	      FVM_REG_PC = 9,
	      FVM_REG_FLAGS = 10,
} fvm_reg_t;

struct fvm_module *fvm_module_by_name( char *name );
struct fvm_module *fvm_module_by_progid( uint32_t progid );
struct fvm_module *fvm_module_by_clid( uint64_t clid );
uint32_t fvm_progid_by_name( char *name );
uint32_t fvm_symbol_addr( struct fvm_module *m, char *name );
uint32_t fvm_symbol_index( struct fvm_module *m, char *name );
uint32_t fvm_symbol_by_index( struct fvm_module *m, uint32_t index );
int fvm_native_call( struct fvm_s *state, uint32_t procid );

char *fvm_getaddr( struct fvm_s *state, uint32_t addr );
char *fvm_getaddr_writable( struct fvm_s *state, uint32_t addr );
uint32_t fvm_read( struct fvm_s *state, uint32_t addr );
void fvm_write( struct fvm_s *state, uint32_t addr, uint32_t val );

void fvm_debug( int enable );
void fvm_printf( char *fmt, ... );
uint32_t fvm_max_steps( uint32_t n );
uint32_t fvm_default_timeout( uint32_t timeout );

//int fvm_cluster_update( struct fvm_module *module );
struct fvm_module *fvm_get_modules( void );

/* register this module as an rpc program */
int fvm_register_program( uint32_t progid );
int fvm_unregister_program( uint32_t progid );
void fvm_log( int lvl, char *fmt, ... );

uint32_t fvm_read_uint32( struct fvm_module *m, uint32_t procid );
int fvm_write_uint32( struct fvm_module *m, uint32_t procid, uint32_t val );
uint64_t fvm_read_uint64( struct fvm_module *m, uint32_t procid );
int fvm_write_uint64( struct fvm_module *m, uint32_t procid, uint64_t val );
int fvm_read_string( struct fvm_module *m, uint32_t procid, char *str, int size );
int fvm_write_string( struct fvm_module *m, uint32_t procid, char *str );

int fvm_audit_write( uint32_t progid, uint32_t procid, char *args, int len );
uint64_t fvm_audit_read( uint64_t nextid, uint32_t *progid, uint32_t *procid, char *args, int len, int *lenp );
int fvm_audit_replay( void );
int fvm_audit_reset( void );
int fvm_module_save_data( struct fvm_module *m );

#endif

