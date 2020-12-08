
#ifndef FVMC2_H
#define FVMC2_H

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
} op_t;

/* any procthat needs a u64 just has a something like Proc( high : u32, low : u32 ) i.e. high word followed by low word */
typedef enum {
    VAR_TYPE_U32 = 0,
    VAR_TYPE_STRING = 1,
    VAR_TYPE_OPAQUE = 2,
    VAR_TYPE_SPARE = 3,
} var_t;


#endif

