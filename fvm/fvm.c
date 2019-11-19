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

#ifdef WIN32
#include <Winsock2.h>
#include <Windows.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <fju/fvm.h>
#include <fju/rpc.h>
#include <fju/sec.h>

#define FVM_INT_EXCEPTION 0x8000 
#define FVM_INT_PME       0x00    /* privilege mode exception */
#define FVM_INT_PME_PL    FVM_INT_EXCEPTION       /* pme priority level */
#define FVM_INT_IOC       0x01    /* illegal opcode exception */
#define FVM_INT_IOC_PL    FVM_INT_EXCEPTION       /* ioc priority level */
#define FVM_INT_DBZ       0x02    /* divide by zero */
#define FVM_INT_DBZ_PL    FVM_INT_EXCEPTION       /* divide by zero level */

/*
 * Loosely modelled on the LC3 processor. 
 * Layout:
 * 0x0000 - 0x07ff word jump table 
 * 0x0800 - 0x0fff unused 
 * 0x1000 - 0x2fff return stack 
 * 0x3000 - 0xfdff user program code + data stack 
 * 0xfe00 - 0xffff device registers 
 */

static int fvm_interrupt( struct fvm_state *state, uint16_t ivec, uint16_t priority );


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
    default:
      return 0;
    }
  }
  return state->mem[offset];    
}

static void write_mem( struct fvm_state *state, uint16_t offset, uint16_t val ) {
  char *addr;
  uint32_t count;
  int sts, ne;
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
      state->mem[offset] = val & 0xff;
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
      
	addr = (char *)&state->mem[state->reg[FVM_REG_R0]];
	count = state->reg[FVM_REG_R1];
	memset( &entry, 0, sizeof(entry) );
	iov[0].buf = addr;
	iov[0].len = count;
	entry.niov = 1;
	entry.iov = iov;
	entry.id = state->inlog_id;
	sts = log_read( state->inlog, state->inlog_id, &entry, 1, &ne );
	if( sts || !ne ) {
	  state->reg[FVM_REG_R0] = 0; /* R0 receives msglen */
	} else {
	  state->inlog_id = entry.id;
	  state->reg[FVM_REG_R0] = entry.msglen;
	}
	break;
      case 1:
	/* reset msg id */
	state->inlog_id = 0;
	break;
      }
      
      break;
    case FVM_DEVICE_OUTLOG:
	/* output register */
        switch( val ) {
	case 0:
	  if( !state->outlog ) {
	    return;
	  }

	  addr = (char *)&state->mem[state->reg[FVM_REG_R0]];
	  count = state->reg[FVM_REG_R1];
	  //printf( "writing %d bytes addr %d\n", (int)count, (int)state->reg[FVM_REG_R0]);
	  
	  memset( &entry, 0, sizeof(entry) );
	  entry.flags = LOG_LVL_INFO|LOG_BINARY;
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
    }
  } else {
    state->mem[offset] = val;
  }  
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

static int fvm_interrupt( struct fvm_state *state, uint16_t ivec, uint16_t priority ) {
    /* don't interrupt if current priority higher than this interrupts level */
    if( !(priority & 0x8000) &&
	((state->reg[FVM_REG_PSR] & FVM_PSR_PL_MASK) >> 12) >= (priority & 0x7) ) {
	if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; Decline interrupt PL %d >= %d\n", (state->reg[FVM_REG_PSR] & FVM_PSR_PL_MASK) >> 12, priority & 0x7 );
	return -1;
    }

    if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; Interrupt %x PL %x\n", ivec, priority & 0x7 );
    
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
    state->reg[FVM_REG_PC] = read_mem( state, 0x800 + ivec );
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
    state->reg[reg] = read_mem( state, state->reg[sp] );
    update_flags( state, state->reg[reg] );
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

/* reserved opcode 3 */
static void fvm_inst_res3( struct fvm_state *state, uint16_t opcode ) {
  if( state->flags & FVM_FLAG_VERBOSE ) printf( ";; %04x RES3\n", state->reg[FVM_REG_PC] - 1 );
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
    fvm_inst_res3
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

int fvm_load( struct fvm_state *state, uint16_t *program, int proglen ) {
  int i, j;
  uint16_t offset, count;

  memset( state->mem, 0, sizeof(state->mem) );
  i = 0;
  while( i < proglen ) {
      if( (i + 1) >= proglen ) return -1;

      offset = program[i];
      count = program[i+1];
      i += 2;
      if( ((uint32_t)offset + (uint32_t)count) >= 0xffff ) return -1;
      
      for( j = 0; j < count; j++ ) {
	  if( i >= proglen ) return -1;
	  
	  state->mem[offset + j] = program[i];
	  i++;
      }
  }

  fvm_reset( state );
  
  return 0;
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
    fvm->flags |= FVM_FLAG_RUNNING;

  return 0;
}


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

	/* detect word returend by checking return stack */
	if( fvm->reg[FVM_REG_RP] == regs[FVM_REG_RP] ) {
	    break; 
	}
    }

    /* extract results */
    for( i = 0; i < nres; i++ ) {
	fvm->reg[FVM_REG_SP]++;
	res[i] = fvm->mem[fvm->reg[FVM_REG_SP]];
    }
    
    /* restore registers */
    for( i = 0; i < FVM_REG_MAX; i++ ) {
	fvm->reg[i] = regs[i];
    }

    return 0;
}

    
