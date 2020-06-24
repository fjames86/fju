
#ifndef FVM2_PRIVATE_H
#define FVM2_PRIVATE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <fju/fvm2.h>

#define FVM2_MAX_OPCODE 256

#define FVM2_FLAG_POS  0x1
#define FVM2_FLAG_NEG  0x2
#define FVM2_FLAG_ZERO 0x4

struct fvm2_header {
  uint32_t magic;
#define FVM2_MAGIC    0xf412a91c
  uint32_t version;
#define FVM2_VERSION  1
  uint32_t flags;
  uint32_t symcount;
  uint32_t datasize;
  uint32_t textsize;
  char name[FVM2_MAX_NAME];
  uint8_t checksum[20]; /* sha1 hash of text segment */

  uint32_t spare[5];
};

struct fvm2_module {
  struct fvm2_module *next;
  
  struct fvm2_header header;
  struct fvm2_symbol *symbols;
  uint8_t *data;
  uint8_t *text;
};

typedef enum {
	      FVM2_REG_R0 = 0,
	      FVM2_REG_R1 = 1,
	      FVM2_REG_R2 = 2,
	      FVM2_REG_R3 = 3,
	      FVM2_REG_R4 = 4,
	      FVM2_REG_R5 = 5,
	      FVM2_REG_R6 = 6,
	      FVM2_REG_R7 = 7,
	      FVM2_REG_SP = 8,
	      FVM2_REG_PC = 9,
	      FVM2_REG_FLAGS = 10,
} fvm2_reg_t;

struct fvm2_module *fvm2_module_by_name( char *name );
uint32_t fvm2_symbol_addr( struct fvm2_module *m, char *name );

#endif

