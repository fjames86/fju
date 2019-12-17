/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
*/

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

#define NLS_RPC_PROG (FJU_BASE_PROG + 0)
#define NLS_RPC_VERS 1

#define RAFT_RPC_PROG (FJU_BASE_PROG + 1)
#define RAFT_RPC_VERS 1

#define HRAUTH_RPC_PROG (FJU_BASE_PROG + 2)
#define HRAUTH_RPC_VERS 1

#define REX_RPC_PROG (FJU_BASE_PROG + 3)
#define REX_RPC_VERS 1

#define FREG_RPC_PROG (FJU_BASE_PROG + 4)
#define FREG_RPC_VERS 1

#define FVM_RPC_PROG (FJU_BASE_PROG + 5)
#define FVM_RPC_VERS 1

#define SVCTEST_RPC_PROG (FJU_BASE_PROG + 6)
#define SVCTEST_RPC_VERS 1

#define FJUD_RPC_PROG (FJU_BASE_PROG + 7)
#define FJUD_RPC_VERS 1

#endif

