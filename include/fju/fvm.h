
#ifndef FVM_H
#define FVM_H

#include <stdint.h>

typedef enum {
	      FVM_INST_BR = 0,      /* conditional branch */
	      FVM_INST_ADD = 1,     /* add/move */
	      FVM_INST_LD = 2,      /* load from offset relative to pc */
	      FVM_INST_ST = 3,      /* store to offset relative to pc */
	      FVM_INST_CALL = 4,    /* jump subroutine */
	      FVM_INST_NAND = 5,    /* bitwise nand */ 
	      FVM_INST_LDR = 6,     /* load register */
	      FVM_INST_STR = 7,     /* store register */
	      FVM_INST_RES4 = 8,    /* reserved opcode 4 */
	      FVM_INST_PUSH = 9,    /* stack push/pop */
	      FVM_INST_LDI = 10,    /* load indirect */
	      FVM_INST_STI = 11,    /* store indirect */
	      FVM_INST_JMP = 12,    /* unconditional jump / return */
	      FVM_INST_MUL = 13,    /* mul/div/mod */
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

#define FVM_MAX_MEM (64*1024)

struct fvm_state {
  uint16_t reg[FVM_REG_MAX];
  uint16_t mem[FVM_MAX_MEM];
  uint32_t flags;
#define FVM_FLAG_RUNNING 0x0001
#define FVM_FLAG_VERBOSE 0x0002 
};

int fvm_load( struct fvm_state *state, uint16_t *program, int proglen );
int fvm_run( struct fvm_state *state );



#endif

