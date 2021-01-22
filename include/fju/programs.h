
#ifndef FJU_PROGRAMS_H
#define FJU_PROGRAMS_H

#define RPCBIND_RPC_PROG 100000
#define RPCBIND_RPC_VERS 1

/*
 * Base RPC program number, chosen from user defined range 
 * All other fju programs should come sequentially after this.
 * This file serves as program registry. Any new fju programs 
 * should be listed here.
 */
#define FJU_BASE_PROG 0x2FFF7770 

#define RAFT_RPC_PROG (FJU_BASE_PROG + 1)
#define RAFT_RPC_VERS 1

#define HRAUTH_RPC_PROG (FJU_BASE_PROG + 2)
#define HRAUTH_RPC_VERS 1

#define NLS_RPC_PROG (FJU_BASE_PROC + 3)
#define NLS_RPC_VERS 1

#define FREG_RPC_PROG (FJU_BASE_PROG + 4)
#define FREG_RPC_VERS 1

#define FVM_RPC_PROG (FJU_BASE_PROG + 5)
#define FVM_RPC_VERS 1

#define SVCTEST_RPC_PROG (FJU_BASE_PROG + 6)
#define SVCTEST_RPC_VERS 1

#define FJUD_RPC_PROG (FJU_BASE_PROG + 7)
#define FJUD_RPC_VERS 1

#define RAFT2_RPC_PROG (FJU_BASE_PROG + 9)
#define RAFT2_RPC_VERS 1

#define LOG_RPC_PROG (FJU_BASE_PROG + 10)
#define LOG_RPC_VERS 1

#define DMB_RPC_PROG (FJU_BASE_PROG + 11)
#define DMB_RPC_VERS 1

#define DLM_RPC_PROG (FJU_BASE_PROG + 12)
#define DLM_RPC_VERS 1

#endif

