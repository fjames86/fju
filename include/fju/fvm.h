
#ifndef FVM_H
#define FVM_H

#include <stdint.h>

#include <fju/log.h>

typedef enum {
	      FVM_INST_BR = 0,      /* conditional branch */
	      FVM_INST_ADD = 1,     /* add/move/test */
	      FVM_INST_LD = 2,      /* load from offset relative to pc */
	      FVM_INST_ST = 3,      /* store to offset relative to pc */
	      FVM_INST_CALL = 4,    /* jump subroutine */
	      FVM_INST_NAND = 5,    /* bitwise nand */ 
	      FVM_INST_LDR = 6,     /* load from register */
	      FVM_INST_STR = 7,     /* store from register */
	      FVM_INST_RTI = 8,     /* return from interrupt */
	      FVM_INST_PUSH = 9,    /* stack push/pop */
	      FVM_INST_LDI = 10,    /* load immediate */
	      FVM_INST_STI = 11,    /* store immediate */
	      FVM_INST_JMP = 12,    /* unconditional jump / return */
	      FVM_INST_MUL = 13,    /* mul/div/mod/cmp */
	      FVM_INST_LEA = 14,    /* load effective address */
	      FVM_INST_RES3 = 15,   /* reserved opcode 3 */
} fvm_inst_t;

typedef enum {
	      /* general purpose registers */
	      FVM_REG_R0 = 0,
	      FVM_REG_R1 = 1,
	      FVM_REG_R2 = 2,
	      FVM_REG_R3 = 3,
	      FVM_REG_R4 = 4,
	      FVM_REG_R5 = 5,
	      FVM_REG_R6 = 6,   /* return stack pointer */
	      FVM_REG_R7 = 7,   /* data stack pointer */
	      /* program counter */
	      FVM_REG_PC = 8,
	      /* processor status register (condition codes etc) */
	      FVM_REG_PSR = 9,
	      FVM_REG_MAX,
} fvm_reg_t;
#define FVM_REG_RP FVM_REG_R6    /* alias register R6 to return stack pointer */
#define FVM_REG_SP FVM_REG_R7    /* alias register R7 to stack pointer */

/* psr register flags */
#define FVM_PSR_POS           0x0001    /* positive flag */
#define FVM_PSR_ZERO          0x0002    /* zero flag */
#define FVM_PSR_NEG           0x0004    /* negative flag */
#define FVM_PSR_USERMODE      0x8000    /* if true, running in user mode, otherwise supervisor mode */
#define FVM_PSR_PL_MASK       0x7000    /* priority level mask */

#define FVM_MAX_MEM (64*1024)

struct fvm_state {
  uint16_t reg[FVM_REG_MAX];
  uint16_t mem[FVM_MAX_MEM];
  uint32_t flags;
#define FVM_FLAG_RUNNING 0x0001    
#define FVM_FLAG_VERBOSE 0x0002
#define FVM_FLAG_DONE    0x0004 
  uint64_t tickcount;
  struct log_s *inlog;
  uint64_t inlog_id;
  struct log_s *outlog;
  uint16_t bos;
  uint64_t sleep_timeout;
};

int fvm_load( struct fvm_state *state, uint16_t *program, int proglen );
int fvm_run( struct fvm_state *state );
int fvm_run_nsteps( struct fvm_state *state, int nsteps );
int fvm_run_timeout( struct fvm_state *state, int timeout );
int fvm_reset( struct fvm_state *fvm );
int fvm_call_word( struct fvm_state *fvm, int word, uint16_t *args, int nargs, uint16_t *res, int nres );

#define FVM_INT_EXCEPTION 0x8000 
#define FVM_INT_PME       0x00    /* privilege mode exception */
#define FVM_INT_PME_PL    FVM_INT_EXCEPTION       /* pme priority level */
#define FVM_INT_IOC       0x01    /* illegal opcode exception */
#define FVM_INT_IOC_PL    FVM_INT_EXCEPTION       /* ioc priority level */
#define FVM_INT_DBZ       0x02    /* divide by zero */
#define FVM_INT_DBZ_PL    FVM_INT_EXCEPTION       /* divide by zero level */
int fvm_interrupt( struct fvm_state *state, uint16_t ivec, uint16_t priority );

#define FVM_RPC_PROG 0x27E1FB11
#define FVM_RPC_VERS 1
void fvm_register( void );

#endif

