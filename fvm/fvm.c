#/*
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

#ifdef WIN32
#include <Winsock2.h>
#include <Windows.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <fju/fvm.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/sec.h>
#include <fju/freg.h>
#include <fju/hrauth.h>
#include <fju/hostreg.h>

#include "fvm-private.h"

/*
 * Loosely modelled on the LC3 processor. 
 * Layout:
 * 0x0000 - 0x07ff word jump table 
 * 0x0800 - 0x08ff isr table 
 * 0x0900 - 0x0aff shared memory area (512 words, 1024 bytes)
 * 0x0b00 - 0x0fff unused/reserved
 * 0x1000 - 0x2fff return stack 
 * 0x3000 - 0xfdff user program code + data stack 
 * 0xfe00 - 0xffff device registers 
 */

#define FVM_PUSH(fvm,val) do { write_mem( fvm, fvm->reg[FVM_REG_SP], val ); fvm->reg[FVM_REG_SP]--; } while( 0 )
#define FVM_POP(fvm) (fvm->reg[FVM_REG_SP]++, fvm->mem[fvm->reg[FVM_REG_SP]])

static void write_mem( struct fvm_state *state, uint16_t offset, uint16_t val );
static void fvm_set_dirty_page( struct fvm_state *fvm, uint16_t page );
static void fvm_set_dirty_region( struct fvm_state *fvm, uint16_t startpage, int npage );

void fvm_push_value( struct fvm_state *fvm, uint16_t val ) {
    FVM_PUSH( fvm, val );
}

static uint16_t sign_extend( uint16_t x, int bit_count ) {
  if( (x >> (bit_count - 1)) & 1 ) {
    x |= (0xFFFF << bit_count);
  }
  return x;
}

static void update_flags( struct fvm_state *state, uint16_t x ) {
  state->reg[FVM_REG_PSR] &= ~(FVM_PSR_POS|FVM_PSR_ZERO|FVM_PSR_NEG);
  if( x == 0 ) state->reg[FVM_REG_PSR] |= FVM_PSR_ZERO;
  else if( x & 0x8000 ) state->reg[FVM_REG_PSR] |= FVM_PSR_NEG;
  else state->reg[FVM_REG_PSR] |= FVM_PSR_POS;
}

/* device registers */
#define FVM_DEVICE_MCR 0xfe00   /* machine control register */
#define FVM_DEVICE_CDR 0xfe01   /* console data register */
#define FVM_DEVICE_RNG 0xfe02   /* random number generator */
#define FVM_DEVICE_TICKCOUNT 0xfe03 /* tick counter */
#define FVM_DEVICE_CLOCKLOW 0xfe04  /* unix time clock */
#define FVM_DEVICE_CLOCKHIGH 0xfe05  /* unix time clock */
#define FVM_DEVICE_INLOG 0xfe06     /* input log register */
#define FVM_DEVICE_OUTLOG 0xfe07    /* output log register */
#define FVM_DEVICE_ALARM 0xfe08    /* sleep */
#define FVM_DEVICE_RPC 0xfe09      /* rpc device */
#define FVM_DEVICE_IDLOW 0xfe0a
#define FVM_DEVICE_IDHIGH 0xfe0b

static uint16_t read_mem( struct fvm_state *state, uint16_t offset ) {
  if( offset >= 0xfe00 ) {
    /* memory mapped device registers */
    switch( offset ) {
    case FVM_DEVICE_MCR:
      /* machine control register */
      return 0x8000;
    case FVM_DEVICE_RNG:
      /* random number generator */
      return sec_rand_uint32() & 0xffff;
    case FVM_DEVICE_TICKCOUNT:
      return (uint16_t)state->tickcount;
    case FVM_DEVICE_CLOCKLOW:
      return (uint16_t)time( NULL ) & 0xffff;
    case FVM_DEVICE_CLOCKHIGH:
      return (uint16_t)((time( NULL ) >> 16) & 0xffff);
    case FVM_DEVICE_IDLOW:
      return (state->id & 0xffff);
    case FVM_DEVICE_IDHIGH:
      return (state->id >> 16) & 0xffff;
    default:
      return 0;
    }
  }
  return state->mem[offset];    
}

#define FVM_RPCCMD_RESET  0
#define FVM_RPCCMD_ENCU32 1
#define FVM_RPCCMD_ENCU64 2
#define FVM_RPCCMD_ENCSTR 3
#define FVM_RPCCMD_ENCOPQ 4
#define FVM_RPCCMD_ENCFIX 5
#define FVM_RPCCMD_DECU32 6
#define FVM_RPCCMD_DECU64 7
#define FVM_RPCCMD_DECSTR 8
#define FVM_RPCCMD_DECOPQ 9
#define FVM_RPCCMD_DECFIX 10
#define FVM_RPCCMD_CALL   11
#define FVM_RPCCMD_GETTIMEOUT   12
#define FVM_RPCCMD_SETTIMEOUT   13
#define FVM_RPCCMD_GETSERVICE   14
#define FVM_RPCCMD_SETSERVICE   15 
#define FVM_RPCCMD_GETHOSTID    16
#define FVM_RPCCMD_SETHOSTID    17 
#define FVM_RPCCMD_SETBUF       18
#define FVM_RPCCMD_GETOFFSET    19

void fvm_rpc_force_iter( void );

static void rpcdev_donecb( struct xdr_s *res, void *cxt ) {
  struct fvm_state *fvm = (struct fvm_state *)cxt;
  int maxlen, startpage, npage;

  fvm->flags |= FVM_FLAG_RUNNING;
  fvm->flags &= ~FVM_FLAG_RPC;
  fvm_rpc_force_iter();
  
  if( !res ) {
    fvm->reg[FVM_REG_R0] = 0;
    return;
  }

  /* copy into fvm memory at bos */
  maxlen = res->count - res->offset;
  if( maxlen > fvm->rpc.buf.buf_size ) {
    /* not enough space! return error status */
    fvm->reg[FVM_REG_R0] = 0;
    return;
  }

  fvm->reg[FVM_REG_R0] = -1;
  memcpy( fvm->rpc.buf.buf, res->buf + res->offset, maxlen );
  
  /* mark dirty pages */
  startpage = fvm->rpc.bufaddr / FVM_PAGE_SIZE;
  npage = (maxlen / FVM_PAGE_SIZE) + ((maxlen % FVM_PAGE_SIZE) ? 1 : 0);  
  fvm_set_dirty_region( fvm, startpage, npage );

  fvm->rpc.buf.count = maxlen;
  fvm->rpc.buf.offset = 0;
}

static int rpcdev_call( struct fvm_state *fvm, uint32_t prog, uint32_t vers, uint32_t proc ) {
  int sts, maxlen;
  struct hrauth_call hcall;
  struct hrauth_call_opts opts;
  struct xdr_s args, res;
    
  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = fvm->rpc.hostid ? fvm->rpc.hostid : hostreg_localid();
  hcall.prog = prog;
  hcall.vers = vers;
  hcall.proc = proc;
  hcall.donecb = rpcdev_donecb;
  hcall.cxt = fvm;
  hcall.timeout = fvm->rpc.timeout ? fvm->rpc.timeout : 1000;
  hcall.service = (fvm->rpc.service > HRAUTH_SERVICE_PRIV ? -1 : (uint32_t)fvm->rpc.service); // -1 == no auth

  if( fvm->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x RPC %u:%u:%u\n", fvm->reg[FVM_REG_PC] - 1, hcall.prog, hcall.vers, hcall.proc );
  
  args = fvm->rpc.buf;

  if( rpcdp() ) {
    /* if running as rpcd then send call and await reply */
//    hcall.hostid = hostreg_localid();
    sts = hrauth_call_udp_async( &hcall, &args, NULL );
    if( sts ) return sts;
    
    /* stop execution, continue when reply received */
    fvm->flags &= ~FVM_FLAG_RUNNING;
    fvm->flags |= FVM_FLAG_RPC;
  } else {
    /* if not running a rpcd then just make a standard call and block */

    memset( &opts, 0, sizeof(opts) );
    opts.mask |= HRAUTH_CALL_OPT_TMPBUF;
    xdr_init( &opts.tmpbuf, malloc( 32*1024 ), 32*1024 );  
    opts.mask |= HRAUTH_CALL_OPT_PORT;
    opts.port = rpcd_get_default_port();
    if( !opts.port ) opts.port = 8000; /*  Default to something sane */
    sts = hrauth_call_udp( &hcall, &args, &res, &opts );
    if( sts ) {
      free( opts.tmpbuf.buf );
      return sts;
    }

    /* copy into buffer */
    maxlen = res.count - res.offset;
    if( maxlen > fvm->rpc.buf.buf_size ) {
      /* not enough space! return error status */
      return -1;
    } else {
      memcpy( fvm->rpc.buf.buf, res.buf + res.offset, maxlen );
      fvm->rpc.buf.count = maxlen;
      fvm->rpc.buf.offset = 0;

      fvm_set_dirty_region( fvm,
			    fvm->rpc.bufaddr / FVM_PAGE_SIZE,
			    (maxlen / FVM_PAGE_SIZE) + ((maxlen % FVM_PAGE_SIZE) ? 1 : 0) );
    }
    free( opts.tmpbuf.buf );
  }

  return 0;
}

static void devrpc_writemem( struct fvm_state *fvm, uint16_t val ) {
  int sts = 0;

  switch( val ) {
  case FVM_RPCCMD_RESET:
    /* reset device */
    xdr_reset( &fvm->rpc.buf );
    break;
  case FVM_RPCCMD_ENCU32:
    /* encode uint32. (high low --)*/
    {
      uint16_t words[2];
      uint32_t u32;
      
      words[0] = FVM_POP(fvm);
      words[1] = FVM_POP(fvm);
      u32 = ((uint32_t)words[1] << 16) | words[0];
      sts = xdr_encode_uint32( &fvm->rpc.buf, u32 );
    }
    break;
  case FVM_RPCCMD_ENCU64:
    /* encode uint32. (high ... low) */
    {
      uint16_t words[4];
      uint64_t u64;
      
      words[0] = FVM_POP(fvm); /* lowest */
      words[1] = FVM_POP(fvm);
      words[2] = FVM_POP(fvm);
      words[3] = FVM_POP(fvm);  /* highest */
      u64 = ((uint64_t)words[3] << 48) |
	((uint64_t)words[2] << 32) |
	((uint64_t)words[1] << 16) |
	(uint64_t)words[0];
      sts = xdr_encode_uint64( &fvm->rpc.buf, u64 );
    }
    break;    
  case FVM_RPCCMD_ENCSTR:
    /* encode string. (addr) */
    {
      char *str = (char *)&fvm->mem[FVM_POP(fvm)];
      sts = xdr_encode_string( &fvm->rpc.buf, str );
    }
    break;
  case FVM_RPCCMD_ENCOPQ:
    /* encode opqque. (addr len) */
    {
      int len;
      uint8_t *opq;
      len = FVM_POP(fvm);
      opq = (uint8_t *)&fvm->mem[FVM_POP(fvm)]; 
      sts = xdr_encode_opaque( &fvm->rpc.buf, opq, len );
    }
    break;
  case FVM_RPCCMD_ENCFIX:
    /* encode fixed */
    {
      int len;
      uint8_t *opq;
      len = FVM_POP(fvm);
      opq = (uint8_t *)&fvm->mem[FVM_POP(fvm)]; 
      sts = xdr_encode_fixed( &fvm->rpc.buf, opq, len );
    }    
    break;
  case FVM_RPCCMD_DECU32:
    /* decode u32. (-- high low) */
    {
      uint32_t u32 = 0;
      sts = xdr_decode_uint32( &fvm->rpc.buf, &u32 );
      if( sts ) u32 = 0;
      FVM_PUSH(fvm, (u32 >> 16) & 0xffff );
      FVM_PUSH(fvm, u32 & 0xffff );
    }
    break;
  case FVM_RPCCMD_DECU64:
    /* decode u64 (-- high ... low */
    {
      uint64_t u64 = 0;
      sts = xdr_decode_uint64( &fvm->rpc.buf, &u64 );
      if( sts ) u64 = 0;
      FVM_PUSH(fvm, (u64 >> 48) & 0xffff );
      FVM_PUSH(fvm, (u64 >> 32) & 0xffff );
      FVM_PUSH(fvm, (u64 >> 16) & 0xffff );
      FVM_PUSH(fvm, u64 & 0xffff );
    }
    break;
  case FVM_RPCCMD_DECSTR:
    /* decode string. (addr len --) */
    {
      int len, straddr;
      char *str;
      len = FVM_POP(fvm);
      straddr = FVM_POP(fvm);
      str = (char *)&fvm->mem[straddr];
      sts = xdr_decode_string( &fvm->rpc.buf, str, len );
      if( sts ) *str = 0;

      /* mark dirty pages */
      len = strlen( str );
      fvm_set_dirty_region( fvm, straddr / FVM_PAGE_SIZE, (len / FVM_PAGE_SIZE) + ((len % FVM_PAGE_SIZE) ? 1 : 0) );
    }
    break;
  case FVM_RPCCMD_DECOPQ:
    /* decode opaque (addr len -- len) */
    {
      int len, straddr;
      uint8_t *ptr;
      len = FVM_POP(fvm);
      straddr = FVM_POP(fvm);
      ptr = (uint8_t *)&fvm->mem[straddr];
      sts = xdr_decode_opaque( &fvm->rpc.buf, ptr, &len );
      FVM_PUSH(fvm, sts ? 0 : len);

      /* set dirty pages */
      len = sts ? 0 : len;
      fvm_set_dirty_region( fvm, straddr / FVM_PAGE_SIZE, (len / FVM_PAGE_SIZE) + ((len % FVM_PAGE_SIZE) ? 1 : 0) );
    }
    break;
  case FVM_RPCCMD_DECFIX:
    /* decode fixed (addr len)*/
    {
      int len, straddr;
      uint8_t *ptr;
      len = FVM_POP(fvm);
      straddr = FVM_POP(fvm);
      ptr = (uint8_t *)&fvm->mem[straddr];
      sts = xdr_decode_fixed( &fvm->rpc.buf, ptr, len );
      if( sts ) memset( ptr, 0, len );

      /* set dirty pages */
      fvm_set_dirty_region( fvm, straddr / FVM_PAGE_SIZE, (len / FVM_PAGE_SIZE) + ((len % FVM_PAGE_SIZE) ? 1 : 0) );
    }
    break;
  case FVM_RPCCMD_CALL:
    /* send call, await reply (prog-high prog-low vers proc -- sts) */
    {
      uint32_t prog, vers, proc;
      proc = (uint32_t)FVM_POP(fvm);
      vers = (uint32_t)FVM_POP(fvm);
      prog = (uint32_t)FVM_POP(fvm);
      prog |= ((uint32_t)FVM_POP(fvm)) << 16;
      sts = rpcdev_call( fvm, prog, vers, proc );
      /* push error status */
      FVM_PUSH(fvm,sts ? 0 : -1);
    }
    break;
  case FVM_RPCCMD_GETTIMEOUT:
    {
	FVM_PUSH(fvm, fvm->rpc.timeout);
    }
    break;
  case FVM_RPCCMD_SETTIMEOUT:
    {
	uint16_t timeout = FVM_POP(fvm);
	fvm->rpc.timeout = timeout;
    }
    break;
  case FVM_RPCCMD_GETSERVICE:
    {
	FVM_PUSH(fvm, fvm->rpc.service);
    }
    break;
  case FVM_RPCCMD_SETSERVICE:
    {
	uint16_t service = FVM_POP(fvm);
	fvm->rpc.service = service > HRAUTH_SERVICE_PRIV ? -1 : service;
    }
    break;
  case FVM_RPCCMD_GETHOSTID:
    {
        FVM_PUSH(fvm, (fvm->rpc.hostid >> 48) & 0xffff );
	FVM_PUSH(fvm, (fvm->rpc.hostid >> 32) & 0xffff );
	FVM_PUSH(fvm, (fvm->rpc.hostid >> 16) & 0xffff );
	FVM_PUSH(fvm, (fvm->rpc.hostid) & 0xffff );
    }
    break;
  case FVM_RPCCMD_SETHOSTID:
    {
      fvm->rpc.hostid = (uint64_t)FVM_POP(fvm); 
      fvm->rpc.hostid |= ((uint64_t)FVM_POP(fvm)) << 16;
      fvm->rpc.hostid |= ((uint64_t)FVM_POP(fvm)) << 32;
      fvm->rpc.hostid |= ((uint64_t)FVM_POP(fvm)) << 48;
    }
    break;
  case FVM_RPCCMD_SETBUF:
    {
      uint16_t bufaddr, bufsize;
      bufaddr = FVM_POP(fvm);
      bufsize = FVM_POP(fvm);
      xdr_init( &fvm->rpc.buf, (uint8_t *)&fvm->mem[bufaddr], bufsize );
      fvm->rpc.bufaddr = bufaddr;
    }
    break;
  case FVM_RPCCMD_GETOFFSET:
    FVM_PUSH( fvm, fvm->rpc.buf.offset );
    break;
  default:
    log_writef( NULL, LOG_LVL_INFO, "fvm rpcdev unknown command %u", val );
    sts = -1;
    break;
  }

}


static void write_mem( struct fvm_state *state, uint16_t offset, uint16_t val ) {
  char *addr;
  uint32_t count;
  int sts, ne, memaddr;
  struct log_entry entry;
  struct log_iov iov[1];
	
  if( offset >= 0xfe00 ) {
    /* write to memory mapped device registers */
    switch( offset ) {
    case FVM_DEVICE_MCR:
      /* machine control register */
      if( !(val & 0x8000) ) {
	if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; Halt execution\n" );
	state->flags &= ~FVM_FLAG_RUNNING;
	return;
      }
      break;
    case FVM_DEVICE_CDR:
      /* display data register - write character */
      printf( "%c", val & 0x7f );
      return;
    case FVM_DEVICE_INLOG:
      /* Input register */
      switch( val ) {
      case 0:
	/* read nex message */
	if( !state->inlog ) {
	  state->reg[FVM_REG_R0] = 0;
	  return;
	}

	memaddr = state->reg[FVM_REG_R0];
	addr = (char *)&state->mem[memaddr];
	count = state->reg[FVM_REG_R1];
	memset( &entry, 0, sizeof(entry) );
	iov[0].buf = addr;
	iov[0].len = count;
	entry.niov = 1;
	entry.iov = iov;
	entry.id = state->inlogid;
	sts = log_read( state->inlog, state->inlogid, &entry, 1, &ne );
	if( sts || !ne ) {
	  state->reg[FVM_REG_R0] = 0; /* R0 receives msglen */
	} else {
	  state->inlogid = entry.id;
	  state->reg[FVM_REG_R0] = entry.msglen;
	  fvm_set_dirty_region( state,
			       memaddr / FVM_PAGE_SIZE,
			       (count / FVM_PAGE_SIZE) + ((count % FVM_PAGE_SIZE) ? 1 : 0) );
	}
	break;
      case 1:
	/* reset msg id */
	state->inlogid = 0;
	break;
      }
      
      break;
    case FVM_DEVICE_OUTLOG:
	/* output register */
        switch( val ) {
	case 0: /* write string */
	case 1: /* write binary */
	  if( !state->outlog ) {
	    return;
	  }

	  addr = (char *)&state->mem[state->reg[FVM_REG_R0]];
	  if( val == 0 ) count = strlen( addr );
	  else count = state->reg[FVM_REG_R1];
	  
	  memset( &entry, 0, sizeof(entry) );
	  entry.flags = LOG_LVL_INFO|(val == 1 ? LOG_BINARY : 0);
	  entry.iov = iov;
	  entry.niov = 1;
	  entry.iov[0].buf = addr;
	  entry.iov[0].len = count;
	  log_write( state->outlog, &entry );
	  break;
	default:
	  break;
	}
	break;
    case FVM_DEVICE_ALARM:
	state->flags &= ~FVM_FLAG_RUNNING;
	state->sleep_timeout = rpc_now() + val;
	break;
    case FVM_DEVICE_RPC:
      devrpc_writemem( state, val );
      break;
    }
  } else if( offset < 0x0900 ) {
      /* read only area - do nothing */
  } else {
      /* just write into memory */
      state->mem[offset] = val;

      /* set dirty flag */
      fvm_set_dirty_page( state, offset / FVM_PAGE_SIZE );
  }  
}

int fvm_write_mem( struct fvm_state *fvm, char *buf, int len, int offset ) {
    memcpy( &fvm->mem[offset], buf, len );
    fvm_set_dirty_region( fvm, offset / FVM_PAGE_SIZE, (len / FVM_PAGE_SIZE) + (len % FVM_PAGE_SIZE) ? 1 : 0);
    return 0;
}


static void set_pc( struct fvm_state *state, uint16_t val ) {
    if( val <= 0x2fff || val >= 0xfe00 ) {
	if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; Attempt to set PC to invalid address %x\n", val );
	//fvm_interrupt( state, FVM_INT_IOC, FVM_INT_IOC_PL );
	//state->flags &= ~FVM_FLAG_RUNNING;
	//return;
    }
    state->reg[FVM_REG_PC] = val;
}

/* conditional branch */
static void fvm_inst_br( struct fvm_state *state, uint16_t opcode ) {
  uint16_t flags = (opcode >> 9) & 0x7;
  uint16_t pcoffset = sign_extend( opcode & 0x1ff, 9 );

  if( state->flags & FVM_FLAG_VERBOSE )
    printf( ";; %04x BR-%s%s%s %x\n",
	    state->reg[FVM_REG_PC] - 1,
	    flags & FVM_PSR_POS ? "P" : "",
	    flags & FVM_PSR_ZERO ? "Z" : "",
	    flags & FVM_PSR_NEG ? "N" : "",
	    pcoffset );

  if( ((flags & (FVM_PSR_POS|FVM_PSR_ZERO|FVM_PSR_NEG)) == (FVM_PSR_POS|FVM_PSR_ZERO|FVM_PSR_NEG)) ||
      (state->reg[FVM_REG_PSR] & flags) ) {
    if( pcoffset == 0xffff ) {
      /* This will cause an infinite loop so it is an invalid instruction */
      fvm_interrupt( state, FVM_INT_IOC, FVM_INT_IOC_PL );
    } else {
      state->reg[FVM_REG_PC] += pcoffset;
    }
  }
  
}

/* arithmetic add. can also be used to move values between registers by adding immediate value 0 */
static void fvm_inst_add( struct fvm_state *state, uint16_t opcode ) {
  uint16_t dr, sr1, immp, imm, sr2;

  dr = (opcode >> 9) & 0x7;
  sr1 = (opcode >> 6) & 0x7;
  immp = opcode & 0x20;
  imm = sign_extend( opcode & 0x1f, 5 );
  sr2 = opcode & 0x7;
  
  if( state->flags & FVM_FLAG_VERBOSE ) {
    if( immp ) {
      if( imm == 0 )
	if( dr == sr1 ) printf( ";; %04x TEST R%x\n", state->reg[FVM_REG_PC] - 1, dr );
	else printf( ";; %04x MOV R%x R%x\n", state->reg[FVM_REG_PC] - 1, dr, sr1 );
      else printf( ";; %04x ADD R%x R%x 0x%x\n", state->reg[FVM_REG_PC] - 1, dr, sr1, imm );
    } else printf( ";; %04x ADD R%x R%x R%x\n", state->reg[FVM_REG_PC] - 1, dr, sr1, sr2 );
  }
  
  state->reg[dr] = state->reg[sr1] + (immp ? imm : state->reg[sr2]);
  update_flags( state, state->reg[dr] );
}

/* load value relative to pc */
static void fvm_inst_ld( struct fvm_state *state, uint16_t opcode ) {
  uint16_t dr, pcoffset;

  dr = (opcode >> 9) & 0x7;
  pcoffset = sign_extend( opcode & 0x1ff, 9 );

  if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x LD R%x [PC + 0x%x]\n", state->reg[FVM_REG_PC] - 1, dr, pcoffset );
  
  state->reg[dr] = read_mem( state, state->reg[FVM_REG_PC] + pcoffset );
  update_flags( state, state->reg[dr] );
}

/* store value relative to pc */
static void fvm_inst_st( struct fvm_state *state, uint16_t opcode ) {
  uint16_t sr, pcoffset;

  sr = (opcode >> 9) & 0x7;
  pcoffset = sign_extend( opcode & 0x1ff, 9 );
  
  if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x ST [PC + 0x%x] R%x\n", state->reg[FVM_REG_PC] - 1, pcoffset, sr );
  write_mem( state, state->reg[FVM_REG_PC] + pcoffset, state->reg[sr] );
}

/* jump to subroutine */
static void fvm_inst_call( struct fvm_state *state, uint16_t opcode ) {
  uint16_t br, pcoffset, currpc;

  /* push pc to return stack */
  currpc = state->reg[FVM_REG_PC];
  write_mem( state, state->reg[FVM_REG_RP], currpc );
  state->reg[FVM_REG_RP]--;

  if( opcode & 0x800 ) {
    /* jump to offset stored in word table */
    pcoffset = opcode & 0x7ff;
    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x CALL 0x%x CurrPC = %x RP = %x\n", state->reg[FVM_REG_PC] - 1, pcoffset, currpc, state->reg[FVM_REG_RP] );
    
    state->reg[FVM_REG_PC] = read_mem( state, pcoffset );
  } else {
    /* jump to offset stored in register */
    br = (opcode >> 6) & 0x7;
    if( state->flags & FVM_FLAG_VERBOSE ) {
      printf( ";; %04x CALL R%x CurrPC = %x RP = %x\n", state->reg[FVM_REG_PC] - 1, br, currpc, state->reg[FVM_REG_RP] );
    }
    
    set_pc( state, state->reg[br] );
  }
  
}

/* bitwise nand. can be used to derive all other bitwise operators */
static void fvm_inst_nand( struct fvm_state *state, uint16_t opcode ) {
  uint16_t dr, sr1, immp, imm, sr2;

  dr = (opcode >> 9) & 0x7;
  sr1 = (opcode >> 6) & 0x7;
  immp = opcode & 0x20;
  imm = sign_extend( opcode & 0x1f, 5 );
  sr2 = opcode & 0x7;
  
  if( state->flags & FVM_FLAG_VERBOSE ) {
    if( immp ) printf( ";; %04x NAND R%x R%x 0x%x\n", state->reg[FVM_REG_PC] - 1, dr, sr1, imm );
    else printf( ";; %04x NAND R%x R%x R%x\n", state->reg[FVM_REG_PC] - 1, dr, sr1, sr2 );
  }
  
  state->reg[dr] = ~(state->reg[sr1] & (immp ? imm : state->reg[sr2]));
  update_flags( state, state->reg[dr] );
}

/* load from register + offset */
static void fvm_inst_ldr( struct fvm_state *state, uint16_t opcode ) {
  uint16_t dr, baser, offset;
  
  dr = (opcode >> 9) & 0x7;
  baser = (opcode >> 6) & 0x7;
  offset = sign_extend( opcode & 0x1f, 5 );
  
  if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x LDR R%x [R%x + 0x%x] = %x\n", state->reg[FVM_REG_PC] - 1, dr, baser, offset, read_mem( state, state->reg[baser] + offset ) );
  
  state->reg[dr] = read_mem( state, state->reg[baser] + offset );
  update_flags( state, state->reg[dr] );
}

/* store to register + offset */
static void fvm_inst_str( struct fvm_state *state, uint16_t opcode ) {
  uint16_t sr, baser, offset;
  
  sr = (opcode >> 9) & 0x7;
  baser = (opcode >> 6) & 0x7;
  offset = sign_extend( opcode & 0x1f, 5 );
  
  if( state->flags & FVM_FLAG_VERBOSE )
    printf( ";; %04x STR [R%x + 0x%x] R%x\n", state->reg[FVM_REG_PC] - 1, baser, offset, sr );
  
  write_mem( state, state->reg[baser] + offset, state->reg[sr] );
}

int fvm_interrupt( struct fvm_state *state, uint16_t ivec, uint16_t priority ) {
    uint16_t isrpc;

    /* don't interrupt if current priority higher than this interrupts level */
    if( !(priority & 0x8000) &&
	((state->reg[FVM_REG_PSR] & FVM_PSR_PL_MASK) >> 12) >= (priority & 0x7) ) {
	if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; Decline interrupt PL %d >= %d\n", (state->reg[FVM_REG_PSR] & FVM_PSR_PL_MASK) >> 12, priority & 0x7 );
	return -1;
    }

    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; Interrupt %x PL %x\n", ivec, priority & 0x7 );

    /* always continue running after an interrupt */
    state->flags |= FVM_FLAG_RUNNING;	

    /* get jump address */
    isrpc = read_mem( state, 0x800 + ivec );
    if( isrpc == 0 ) {
	/* no isr set, do nothing ? */
	return -1;
    }
    
    /* save registers */
    write_mem( state, state->reg[FVM_REG_SP], state->reg[FVM_REG_R5] ); state->reg[FVM_REG_SP]--;
    write_mem( state, state->reg[FVM_REG_SP], state->reg[FVM_REG_R4] ); state->reg[FVM_REG_SP]--;
    write_mem( state, state->reg[FVM_REG_SP], state->reg[FVM_REG_R3] ); state->reg[FVM_REG_SP]--;
    write_mem( state, state->reg[FVM_REG_SP], state->reg[FVM_REG_R2] ); state->reg[FVM_REG_SP]--;
    write_mem( state, state->reg[FVM_REG_SP], state->reg[FVM_REG_R1] ); state->reg[FVM_REG_SP]--;
    write_mem( state, state->reg[FVM_REG_SP], state->reg[FVM_REG_R0] ); state->reg[FVM_REG_SP]--;
    write_mem( state, state->reg[FVM_REG_SP], state->reg[FVM_REG_PSR] ); state->reg[FVM_REG_SP]--;
    write_mem( state, state->reg[FVM_REG_SP], state->reg[FVM_REG_PC] ); state->reg[FVM_REG_SP]--;

    /* set supervisor mode and priority */
    state->reg[FVM_REG_PSR] &= ~FVM_PSR_USERMODE;
    if( !(priority & 0x8000) ) {
	state->reg[FVM_REG_PSR] &= ~FVM_PSR_PL_MASK;
	state->reg[FVM_REG_PSR] |= ((priority & 0x07) << 12);
    }

    /* jump */
    state->reg[FVM_REG_PC] = isrpc;

    return 0;
}

/* return from interrupt */
static void fvm_inst_rti( struct fvm_state *state, uint16_t opcode ) {
  if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x RTI\n", state->reg[FVM_REG_PC] - 1 );

  if( state->reg[FVM_REG_PSR] & FVM_PSR_USERMODE ) {
      printf( ";; Privilege mode exception: Attempt to RTI from user mode\n" ); 
      fvm_interrupt( state, FVM_INT_PME, FVM_INT_PME_PL );
  } else {
      /* restore pc and registers */
      state->reg[FVM_REG_SP]++; state->reg[FVM_REG_PC] = read_mem( state, state->reg[FVM_REG_SP] );
      state->reg[FVM_REG_SP]++; state->reg[FVM_REG_PSR] = read_mem( state, state->reg[FVM_REG_SP] );
      state->reg[FVM_REG_SP]++; state->reg[FVM_REG_R0] = read_mem( state, state->reg[FVM_REG_SP] );
      state->reg[FVM_REG_SP]++; state->reg[FVM_REG_R1] = read_mem( state, state->reg[FVM_REG_SP] );
      state->reg[FVM_REG_SP]++; state->reg[FVM_REG_R2] = read_mem( state, state->reg[FVM_REG_SP] );
      state->reg[FVM_REG_SP]++; state->reg[FVM_REG_R3] = read_mem( state, state->reg[FVM_REG_SP] );
      state->reg[FVM_REG_SP]++; state->reg[FVM_REG_R4] = read_mem( state, state->reg[FVM_REG_SP] );
      state->reg[FVM_REG_SP]++; state->reg[FVM_REG_R5] = read_mem( state, state->reg[FVM_REG_SP] );
  }
}

/* stack push/pop */
static void fvm_inst_push( struct fvm_state *state, uint16_t opcode ) {
  uint16_t reg, sp;

  reg = (opcode >> 9) & 0x7;
  sp = (opcode & 0x10) ? FVM_REG_RP : FVM_REG_SP;
  if( opcode & 0x20 ) {
    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x %sPOP R%x = %x %sP = %x\n", state->reg[FVM_REG_PC] - 1, sp == FVM_REG_RP ? "R" : "", reg, read_mem( state, state->reg[sp] + 1 ), sp == FVM_REG_RP ? "R" : "S", state->reg[sp] + 1 );
    state->reg[sp]++;
    if( (sp == FVM_REG_SP) && (state->reg[sp] > 0xfdff) ) {
	/* attempt to set stack pointer out of range so reset */
	state->reg[reg] = 0;
	state->reg[sp] = 0xfdff;
    } else if( (sp == FVM_REG_RP) && (state->reg[sp] > 0x2fff) ) {
	/* 
	 * Attempt to set return pointer out of range so reset. 
	 * XXX This is more serious than data stack overflow - what to do here?
	 */
	state->reg[reg] = 0;
	state->reg[sp] = 0x2fff;
    } else {
	state->reg[reg] = read_mem( state, state->reg[sp] );
	update_flags( state, state->reg[reg] );
    }
  } else {
    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x %sPUSH R%x = %x %sP = %x\n", state->reg[FVM_REG_PC] - 1, sp == FVM_REG_RP ? "R" : "", reg, state->reg[reg], sp == FVM_REG_RP ? "R" : "S", state->reg[sp] );
    write_mem( state, state->reg[sp], state->reg[reg] );
    state->reg[sp]--;
  }
  
}

/* load immediate */
static void fvm_inst_ldi( struct fvm_state *state, uint16_t opcode ) {
  uint16_t dr, val;

  dr = (opcode >> 9) & 0x7;
  val = sign_extend( opcode & 0x1ff, 9 );
  if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x LDI R%x %04x\n", state->reg[FVM_REG_PC] - 1, dr, val );
  
  state->reg[dr] = val;
  update_flags( state, state->reg[dr] );
}

/* store immediate */
static void fvm_inst_sti( struct fvm_state *state, uint16_t opcode ) {
  uint16_t sr, val;

  sr = (opcode >> 9) & 0x7;
  val = sign_extend( opcode & 0x1ff, 9 );
  if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x STI [R%x] R%x => [%04x] = %x\n", state->reg[FVM_REG_PC] - 1, sr, val, state->reg[sr], val );
  
  write_mem( state, state->reg[sr], val );
}

/* return/unconditional jump */
static void fvm_inst_jmp( struct fvm_state *state, uint16_t opcode ) {
  uint16_t baser, ret;

  ret = opcode & 0x0800;    /* return flag */
  /* spare bits: 0x061f i.e. bits 10, 9, 5-0 */
  
  if( ret ) {
    /* pop return address from return stack and jump */
    state->reg[FVM_REG_RP]++;
    ret = read_mem( state, state->reg[FVM_REG_RP] );

    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x RET PC = %04x RP = %x\n", state->reg[FVM_REG_PC] - 1, ret, state->reg[FVM_REG_RP] );
    set_pc( state, ret );
  } else {
    /* unconditional jump to address in register */
    baser = (opcode >> 6) & 0x7;
    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x JMP R%x = %x\n", state->reg[FVM_REG_PC] - 1, baser, state->reg[baser] );
    set_pc( state, state->reg[baser] );
  }
  

}

/* mul/div/mod/cmp */
static void fvm_inst_mul( struct fvm_state *state, uint16_t opcode ) {
  uint16_t dr, sr1, sr2, flags;
  dr = (opcode >> 9) & 0x7;
  sr1 = (opcode >> 6) & 0x7;
  sr2 = (opcode >> 3) & 0x7;
  flags = opcode & 0x7;
  if( flags == 0 ) {
    /* mul */
    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x MUL R%x R%x R%x\n", state->reg[FVM_REG_PC] - 1, dr, sr1, sr2 );
    state->reg[dr] = state->reg[sr1] * state->reg[sr2];
  } else if( flags == 1 ) {
    /* div */
    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x DIV R%x R%x R%x\n", state->reg[FVM_REG_PC] - 1, dr, sr1, sr2 );
    if( state->reg[sr2] == 0 ) {
      printf( ";; Divide by zero exception\n" );
      //fvm_interrupt( state, FVM_INT_DBZ, FVM_INT_DBZ_PL );
      //return;
      
      state->reg[dr] = 0;
    } else {
      state->reg[dr] = state->reg[sr1] / state->reg[sr2];
    }
  } else if( flags == 2 ) {
    /* mod */
    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x MOD R%x R%x R%x\n", state->reg[FVM_REG_PC] - 1, dr, sr1, sr2 );

    /*
    if( state->reg[sr2] == 0 ) {
	printf( ";; Divide by zero exception\n" );
	fvm_interrupt( state, FVM_INT_DBZ, FVM_INT_DBZ_PL );
	return;
    }
    */
    
    state->reg[dr] = (state->reg[sr2] ? state->reg[sr1] % state->reg[sr2] : 0);
  } else if( flags == 3 ) {
    /* cmp */
    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x CMP R%x R%x R%x = CMP %x %x\n", state->reg[FVM_REG_PC] - 1, dr, sr1, sr2, state->reg[sr1], state->reg[sr2] );
    state->reg[dr] = state->reg[sr1] - state->reg[sr2];
  }
  
  update_flags( state, state->reg[dr] );
}

/* load effective address */
static void fvm_inst_lea( struct fvm_state *state, uint16_t opcode ) {
  uint16_t dr, pcoffset;
  dr = (opcode >> 9) & 0x7;
  pcoffset = sign_extend( opcode & 0x1ff, 9 );
  if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x LEA R%x PC + 0x%x\n", state->reg[FVM_REG_PC] - 1, dr, pcoffset );
  
  state->reg[dr] = state->reg[FVM_REG_PC] + pcoffset;
  update_flags( state, state->reg[dr] );
}

/* reserved opcode */
static void fvm_inst_res( struct fvm_state *state, uint16_t opcode ) {
  if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x RES 0x%x\n", state->reg[FVM_REG_PC] - 1, opcode & 0x0fff );
  fvm_interrupt( state, FVM_INT_IOC, FVM_INT_IOC_PL );
}

typedef void (*fvm_inst_handler_t)( struct fvm_state *state, uint16_t opcode );

static fvm_inst_handler_t inst_handlers[] = {
    fvm_inst_br,
    fvm_inst_add,
    fvm_inst_ld,
    fvm_inst_st,
    fvm_inst_call,
    fvm_inst_nand,
    fvm_inst_ldr,
    fvm_inst_str,
    fvm_inst_rti,
    fvm_inst_push,
    fvm_inst_ldi,
    fvm_inst_sti,
    fvm_inst_jmp,
    fvm_inst_mul,
    fvm_inst_lea,
    fvm_inst_res
};

static int fvm_step( struct fvm_state *state ) {
  uint16_t opcode;
  uint16_t inst;

  /* get next opcode and increment pc */
  opcode = read_mem( state, state->reg[FVM_REG_PC] );
  state->reg[FVM_REG_PC]++;

  /* get instruction and invoke handler */
  inst = (opcode >> 12) & 0xf;
  inst_handlers[inst]( state, opcode );
  
  state->tickcount++;

  /* Terminate if return from initial word */
  if( state->reg[FVM_REG_RP] >= 0x3000 ) {
      if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; return stack overflow\n" );
      fvm_reset( state );
      state->flags &= ~FVM_FLAG_RUNNING;
      state->flags |= FVM_FLAG_DONE;
  }
  
  return 0;
}

int fvm_run( struct fvm_state *state ) {  

  while( state->flags & FVM_FLAG_RUNNING ) {
    fvm_step( state );
  }

  return 0;
}

int fvm_run_nsteps( struct fvm_state *state, int nsteps ) {
  int i = 0;
  while( (state->flags & FVM_FLAG_RUNNING) && (i < nsteps) ) {
    fvm_step( state );
    i++;
  }
  return 0;
}

int fvm_run_timeout( struct fvm_state *state, int timeout ) {
  uint64_t end;
  end = rpc_now() + timeout;  
  while( state->flags & FVM_FLAG_RUNNING ) {
    fvm_run_nsteps( state, 100 );
    if( rpc_now() >= end ) break;
  }
  return 0;
}

int fvm_load( struct fvm_state *state, char *progdata, int proglen ) {
  int i, j;
  uint16_t offset, count;
  uint16_t bos = 0;
  uint16_t *program;
  struct fvm_program_header *hdr;
  uint32_t crc;

  hdr = (struct fvm_program_header *)progdata;
  program = (uint16_t *)(progdata + sizeof(*hdr) );
  proglen -= sizeof(*hdr);
  proglen /= 2; /* convert to number of uint16_t */

  /* check header */
  if( hdr->magic != FVM_PROGRAM_MAGIC ) return -1;

  fvm_dirty_reset( state );
  
  memset( state->mem, 0, sizeof(state->mem) );
  i = 0;
  crc = 0xffffffff;
  while( i < proglen ) {
      if( (i + 1) >= proglen ) {
	  return -1;
      }

      offset = program[i];
      count = program[i+1];
      i += 2;
      if( ((uint32_t)offset + (uint32_t)count) >= 0xffff ) {
	  return -1;
      }

      if( (offset + count) > bos ) bos = offset + count;

      crc = sec_crc32( crc, (char *)&program[i], 2 * count );
      
      for( j = 0; j < count; j++ ) {
	  if( i >= proglen ) {
	      return -1;
	  }
	  
	  state->mem[offset + j] = program[i];
	  i++;
      }
      
      fvm_set_dirty_region( state, offset / FVM_PAGE_SIZE, (count / FVM_PAGE_SIZE) + (count % FVM_PAGE_SIZE ? 1 : 0) );
  }

  fvm_reset( state );
  state->bos = bos;

  /* 
   * default to setting rpc device buffer at bottom of stack. Typically programs 
   * should reset the buffer somewhere else.
   */
  xdr_init( &state->rpc.buf, (uint8_t *)&state->mem[bos], 1024 );
  state->rpc.bufaddr = bos;
  
  /* set other rpc parameters: default to no authentication and talking to local fjud */
  state->rpc.service = -1; 
  state->rpc.hostid = hostreg_localid();
 
  state->id = 0xffffffff;

  if( crc != hdr->crc32 ) {
      printf( ";; crc32 mismatch hdr=0x%08x calculated=0x%08x\n", hdr->crc32, crc );
      return -1;
  }
  
  return 0;
}

int fvm_load_freg( struct fvm_state *fvm, uint64_t hreg ) {
  int sts;
  uint32_t flags;
  char *buf;
  int len;
  
  sts = freg_get( NULL, hreg, &flags, NULL, 0, &len );
  if( sts ) return sts;
  if( (flags & FREG_TYPE_MASK) != FREG_TYPE_OPAQUE ) return -1;

  buf = malloc( len );
  sts = freg_get( NULL, hreg, NULL, buf, len, NULL );
  if( sts ) {
    free( buf );
    return -1;
  }

  sts = fvm_load( fvm, buf, len );
  free( buf );
  return sts;
}

int fvm_reset( struct fvm_state *fvm ) {
    int i;
    
    /* clear registers */
    for( i = 0; i < FVM_REG_MAX; i++ ) {
	fvm->reg[i] = 0;
    }
    /* set pc to start of program */
    fvm->reg[FVM_REG_PC] = 0x3000;
    fvm->reg[FVM_REG_SP] = 0xfdff;
    fvm->reg[FVM_REG_RP] = 0x2fff;
    fvm->reg[FVM_REG_PSR] = FVM_PSR_ZERO;
    fvm->flags = FVM_FLAG_RUNNING;
    fvm->flags &= ~FVM_FLAG_DONE;

  return 0;
}


/* remove this? */
int fvm_call_word( struct fvm_state *fvm, int word, uint16_t *args, int nargs, uint16_t *res, int nres ) {
    uint16_t regs[FVM_REG_MAX];
    int i;

    /* save registers */
    for( i = 0; i < FVM_REG_MAX; i++ ) {
	regs[i] = fvm->reg[i];
    }

    /* set pc to work */
    fvm->reg[FVM_REG_PC] = fvm->mem[word & 0x07ff];

    /* push args onto stack */
    for( i = 0; i < nargs; i++ ) {
	fvm->mem[fvm->reg[FVM_REG_SP]] = args[i];
	fvm->reg[FVM_REG_SP]--;
    }
    /* push current address */
    fvm->mem[fvm->reg[FVM_REG_RP]] = fvm->reg[FVM_REG_PC];
    fvm->reg[FVM_REG_RP]--;
    
    /* 
     * run until it returns. 
     * detect by watching rp get back to where it started 
     */
    while( fvm->flags & FVM_FLAG_RUNNING ) {
	fvm_step( fvm );

	/* detect word returned by checking return stack */
	if( fvm->reg[FVM_REG_RP] == regs[FVM_REG_RP] ) {
	    fvm->flags &= ~FVM_FLAG_RUNNING;
	    fvm->flags |= FVM_FLAG_DONE;	    
	    break; 
	}
    }

    /* extract results */
    for( i = 0; i < nres; i++ ) {
	fvm->reg[FVM_REG_SP]++;
	res[(nres - 1) - i] = fvm->mem[fvm->reg[FVM_REG_SP]];
    }
    
    /* restore registers */
    for( i = 0; i < FVM_REG_MAX; i++ ) {
	fvm->reg[i] = regs[i];
    }

    return 0;
}

static void fvm_set_dirty_page( struct fvm_state *fvm, uint16_t page ) {
  int idx, off;
  idx = page / 32;
  off = page % 32;
  fvm->dirty[idx] |= (1 << off);
}

static void fvm_set_dirty_region( struct fvm_state *fvm, uint16_t startpage, int npage ) {
  int i;
  for( i = 0; i < npage; i++ ) {
    fvm_set_dirty_page( fvm, startpage + i );
  }
}

static int fvm_is_dirty_page( struct fvm_state *fvm, uint16_t page ) {
  int idx, off;
  idx = page / 32;
  off = page % 32;
  return fvm->dirty[idx] & (1 << off);
}


void fvm_dirty_reset( struct fvm_state *fvm ) {
    memset( fvm->dirty, 0, sizeof(fvm->dirty) );
}

int fvm_dirty_regions( struct fvm_state *fvm, struct fvm_dirty *dirty, int nd ) {
    int page, n, started;

    n = 0;
    started = 0;
    for( page = 0; page < (FVM_MAX_MEM / FVM_PAGE_SIZE); page++ ) {      

	
	if( started ) {
  	    if( fvm_is_dirty_page( fvm, page ) ) {
		/* currently reading a region, so append */
		if( n < nd ) dirty[n].count += FVM_PAGE_SIZE;
	    } else {
		/* currently reading a region but this is a clean page */
		n++;
		started = 0;
	    }
	} else {
 	    if( fvm_is_dirty_page( fvm, page ) ) {
		/* start a new region */
		started = 1;
		if( n < nd ) {
 		    dirty[n].offset = page * FVM_PAGE_SIZE;
		    dirty[n].count = FVM_PAGE_SIZE;
		}
	    } else {
		/* not started a region and not dirty either -  do nothing */
	    }
	}
    }
    
    return n;
}

int fvm_shmem_read( struct fvm_state *fvm, char *buf, int n, int offset ) {
    int len;
    
    n &= 0x3ff;
    offset &= 0x3ff;
    len = n;
    if( offset > FVM_MAX_SHMEM ) return -1;
    if( len >= (FVM_MAX_SHMEM - offset) ) len = FVM_MAX_SHMEM - offset;
    memcpy( buf, &fvm->mem[0x0900], len );
    return len;
}
int fvm_shmem_write( struct fvm_state *fvm, char *buf, int n, int offset ) {
    int len, startpage, npage, i;
    
    n &= 0x3ff;
    offset &= 0x3ff;
    len = n;
    if( offset > FVM_MAX_SHMEM ) return -1;
    if( len >= (FVM_MAX_SHMEM - offset) ) len = FVM_MAX_SHMEM - offset;    
    memcpy( &fvm->mem[0x900], buf, len );

    startpage = 0x0900 / FVM_PAGE_SIZE;
    npage = (len / FVM_PAGE_SIZE) + ((len % FVM_PAGE_SIZE) ? 1 : 0);
    for( i = 0; i < npage; i++ ) {
      fvm_set_dirty_page( fvm, startpage + i );
    }
    
    return len;
}

#if 0

#define FVM_PERSIST_MAGIC 0xf491a91c
#define FVM_PERSIST_VERSION 1

static int fvm_encode_state( struct xdr_s *xdr, struct fvm_state *fvm ) {
    int sts;
    sts = xdr_encode_uint32( xdr, FVM_PERSIST_MAGIC );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, FVM_PERSIST_VERSION );
    if( sts ) return sts;
    sts = xdr_encode_fixed( xdr, (uint8_t *)fvm->reg, sizeof(fvm->reg) );
    if( sts ) return sts;
    sts = xdr_encode_fixed( xdr, (uint8_t *)fvm->mem, sizeof(fvm->mem) );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, fvm->flags );
    if( sts ) return sts;
    sts = xdr_encode_uint64( xdr, fvm->tickcount );
    if( sts ) return sts;
    sts = xdr_encode_uint64( xdr, fvm->inlogid );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, fvm->bos );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, fvm->rpc.timeout );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, fvm->rpc.service );
    if( sts ) return sts;
    sts = xdr_encode_uint64( xdr, fvm->rpc.hostid );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, fvm->rpc.buf.offset );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, fvm->rpc.buf.count );
    if( sts ) return sts;
    sts = xdr_encode_uint32( xdr, fvm->id );
    return 0;
}
static int fvm_decode_state( struct xdr_s *xdr, struct fvm_state *fvm ) {
    int sts;
    uint32_t u32;

    memset( fvm, 0, sizeof(*fvm) );
    
    sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) return sts;
    if( u32 != FVM_PERSIST_MAGIC ) return -1;
    if( !sts ) sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) return sts;
    if( u32 != FVM_PERSIST_VERSION ) return -1;
    if( !sts ) sts = xdr_decode_fixed( xdr, (uint8_t *)fvm->reg, sizeof(fvm->reg) );
    if( !sts ) sts = xdr_decode_fixed( xdr, (uint8_t *)fvm->mem, sizeof(fvm->mem) );
    if( !sts ) sts = xdr_decode_uint32( xdr, &fvm->flags );
    if( !sts ) sts = xdr_decode_uint64( xdr, &fvm->tickcount );
    if( !sts ) sts = xdr_decode_uint64( xdr, &fvm->inlogid );
    if( !sts ) sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) return sts;
    fvm->bos = (uint16_t)u32;
    if( !sts ) sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) return sts;
    fvm->rpc.timeout = (uint16_t)u32;
    if( !sts ) sts = xdr_decode_uint32( xdr, &u32 );
    if( sts ) return sts;
    fvm->rpc.service = (uint16_t)u32;
    if( !sts ) sts = xdr_decode_uint64( xdr, &fvm->rpc.hostid );
    if( !sts ) sts = xdr_decode_uint32( xdr, (uint32_t *)&fvm->rpc.buf.offset );
    if( !sts ) sts = xdr_decode_uint32( xdr, (uint32_t *)&fvm->rpc.buf.count );
    if( !sts ) sts = xdr_decode_uint32( xdr, &fvm->id );
    return 0;    
}

int fvm_persist( char *path, struct fvm_state *fvm, void *reserved ) {
    int sts;
    struct mmf_s mmf;
    struct xdr_s xdr;
    
    sts = mmf_open( path, &mmf );
    if( sts ) return sts;

    sts = mmf_remap( &mmf, 256*1024 );
    if( sts ) {
	mmf_close( &mmf );
	return -1;
    }

    xdr_init( &xdr, mmf.file, 256*1024 );
    fvm_encode_state( &xdr, fvm );
    mmf_truncate( &mmf, xdr.offset );
    
    mmf_close( &mmf );

    return 0;
}

int fvm_restore( char *path, struct fvm_state *fvm, void *reserved ) {
    int sts;
    struct mmf_s mmf;
    struct xdr_s xdr;
    
    sts = mmf_open2( path, &mmf, MMF_OPEN_EXISTING );
    if( sts ) return sts;

    mmf_remap( &mmf, mmf.fsize );
    xdr_init( &xdr, mmf.file, mmf.fsize );

    sts = fvm_decode_state( &xdr, fvm );

    mmf_close( &mmf );

    return sts;
}

#else

int fvm_persist( char *path, struct fvm_state *fvm, void *reserved ) {
  return -1;
}

int fvm_restore( char *path, struct fvm_state *fvm, void *reserved ) {
  return -1;
}

#endif

