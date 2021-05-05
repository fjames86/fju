
#ifndef FVMC2_H
#define FVMC2_H

#include <fju/fvm.h>

typedef enum {
    OP_NOP = 0,    /* no op */
    OP_LDI32 = 1,  /* load 32bit immediate arg:u32*/
    OP_UNUSED1 = 2,
    OP_LEA = 3,    /* load address relative to pc arg:u16 */
    OP_ADDSP = 4,  /* add constant to sp (allocate stack space) arg:u16*/
    OP_SUBSP = 5,  /* subtract const from sp (free stack space) arg:u16*/
    OP_CALL = 6,   /* push pc and jump arg:u16*/
    OP_RET = 7,    /* pop pc and jump */
    OP_LEASP = 8,  /* load address relative to sp arg:u16*/
    OP_LDSP = 9,   /* load value relative to sp arg:u16*/
    OP_STSP = 10,  /* store value relative to sp arg:u16*/
    OP_BR = 11,    /* jump to const address if true arg:u16 */
    OP_EQ = 12,    /* pop two values, push 1 if equal 0 if false */
    OP_NEQ = 13,
    OP_GT = 14,
    OP_GTE = 15,
    OP_LT = 16,
    OP_LTE = 17,
    OP_JMP = 18,   /* unconditional jump arg:u16*/
    OP_ADD = 19,
    OP_SUB = 20,
    OP_MUL = 21,
    OP_DIV = 22,
    OP_MOD = 23,
    OP_AND = 24,
    OP_OR = 25,
    OP_XOR = 26,
    OP_NOT = 27,
    OP_SHL = 28,
    OP_SHR = 29,
    OP_LD = 30, /* load from address */
    OP_ST = 31, /* store to address */
    OP_SYSCALL = 32, /* syscall arg:u16 */
    OP_BRZ = 33, /* branch if zero (inverse of br) */
    OP_LD8 = 34, /* load 8bit */
    OP_ST8 = 35, /* Store 8bit */
    OP_LDIZ = 36, /* load zero */
    OP_LDI16 = 37, /* load 16bit immediate */
} op_t;

/* any procthat needs a u64 just has a something like Proc( high : u32, low : u32 ) i.e. high word followed by low word */
typedef enum {
    VAR_TYPE_U32 = 0,
    VAR_TYPE_STRING = 1,
    VAR_TYPE_OPAQUE = 2,
    VAR_TYPE_SPARE = 3,
} var_t;

#define FVM_SIGINFO_SETPARAM(siginfo,index,type,isvar) (siginfo |= ((((uint64_t)(type)) | (isvar ? 0x4ULL : 0)) << (3 * (index))))
#define FVM_SIGINFO_SETNPARS(siginfo,npars) (siginfo |= (((uint64_t)(npars)) << 57))

struct fvm_headerinfo {
  uint32_t magic;
#define FVM_MAGIC 0x13fd54ec
  uint32_t version;
#define FVM_VERSION 1
  char name[FVM_MAX_NAME];
  uint32_t progid;
  uint32_t versid;
  uint32_t datasize;
  uint32_t textsize;
  uint32_t nprocs;
  struct {
    char name[FVM_MAX_NAME];
    uint32_t address;
    uint64_t siginfo;    /* 3 bits per param, 3*19=57 bits of param info, 5 bits of length info and 2 bits spare */
  } procs[FVM_MAX_PROC];
  uint64_t timestamp;
};

struct fvm_state {
  struct fvm_module *module;
  uint32_t nsteps;
  uint32_t timeout;
  uint32_t pc;
  uint32_t sp;
  uint32_t frame;
  char stack[FVM_MAX_STACK];
};


int fvm_syscall( struct fvm_state *state, uint16_t syscallid );
uint32_t fvm_stack_read( struct fvm_state *state, uint32_t depth );
int fvm_write_u32( struct fvm_state *state, uint32_t addr, uint32_t u );
uint32_t fvm_read_u32( struct fvm_state *state, uint32_t addr );
char *fvm_getptr( struct fvm_state *state, uint32_t addr, int len, int writeable );
char *fvm_getstr( struct fvm_state *state, uint32_t addr );
void fvm_setdebug( int debugmode );
int fvm_log( int lvl, char *fmt, ... );


#endif

