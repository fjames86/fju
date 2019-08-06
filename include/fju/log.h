/*
 * MIT License
 *
 * Copyright (c) 2018 Frank James
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

#ifndef LOG_H
#define LOG_H

#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <fju/mmf.h>

#define LOG_LBASIZE 64               /* lba size fixed to 64 bytes */

/* caller data */
struct log_s {
  struct mmf_s mmf;
  uint32_t pid;
  uint32_t ltag;
};

struct log_opts {
  uint32_t mask;                   /* bitmask of opening options */
#define LOG_OPT_LBACOUNT 0x0001
#define LOG_OPT_FLAGS    0x0002 
  uint32_t lbacount;               /* if creating log (opening for first time), use this many blocks */
  uint32_t flags;
#define LOG_FLAG_CIRCULAR 0x0000   /* default - circular log */
#define LOG_FLAG_FIXED    0x0001   /* opposite of circular - only append entries at end */
#define LOG_FLAG_GROW     0x0002   /* dynamically grow, requires LOG_FLAG_FIXED */
#define LOG_FLAG_LVLMASK  0x00f0
};

struct log_prop {
  uint32_t version;
#define LOG_VERSION 1
  uint64_t seq;
  uint32_t lbacount;
  uint32_t start;   /* starting index */
  uint32_t count;   /* block count */
  uint64_t last_id;
  uint32_t flags;
  uint64_t tag;
};

struct log_iov {
  char *buf;
  int len;
};

struct log_entry {
  uint64_t id;                          /* msg identifier */
  uint64_t seq;
  uint64_t timestamp;                   /* unix timestamp when msg written */
  uint32_t pid;                         /* pid of writer */
  uint32_t flags;                       /* msg flags */
#define LOG_LVL_MASK      0x0000000f
#define LOG_LVL_TRACE     0x00000000
#define LOG_LVL_DEBUG     0x00000001
#define LOG_LVL_INFO      0x00000002
#define LOG_LVL_WARN      0x00000003
#define LOG_LVL_ERROR     0x00000004
#define LOG_LVL_FATAL     0x00000005
#define LOG_BINARY        0x00000010    /* if set, msg contains opaque binary data, otherwise nul-terminated string */

  /* io buffers */
  int niov;
  struct log_iov *iov;

  int msglen;                           /* total length of message */
  uint64_t prev_id;
  uint32_t ltag;
};

/* open/close file */
int log_open( char *path, struct log_opts *opts, struct log_s *log );
int log_close( struct log_s *log );

/* reset log - clear all msgs */
int log_reset( struct log_s *log );

/* get log properties */
int log_prop( struct log_s *log, struct log_prop *prop );

/* 
 * read msgs starting from beginning (id==0) or from a given msg (id!=0). 
 * if entry->msglen == 0 then msglen is set and no data is copied. otherwise data is 
 * copied into entry->msg up to a maximum of msglen 
 * nelist optionally receives number of msgs retreived 
 */
int log_read( struct log_s *log, uint64_t id, struct log_entry *elist, int n, int *nelist );

/* read end message i.e. most recently written */
int log_read_end( struct log_s *log, uint64_t id, struct log_entry *elist, int n, int *nelist );

/* read a specific entry */
int log_read_entry( struct log_s *log, uint64_t id, struct log_entry *entry );
/* read a specific entry into single buffer */
int log_read_buf( struct log_s *log, uint64_t id, char *buf, int len, int *msglen );

/*
 * write msg. msg, msglen and flags must be set on input. pid, timestamp, id are set on output.
 */
int log_write( struct log_s *log, struct log_entry *entry );

/* utility funtions for writing formatted strings */
int log_writev( struct log_s *log, int lvl, char *fmt, va_list args );
int log_writef( struct log_s *log, int lvl, char *fmt, ... );

/* write single buffer */
int log_write_buf( struct log_s *log, int lvl, char *buf, int len, uint64_t *id );

/* set minimum lvl */
int log_set_lvl( struct log_s *log, int lvl );

#endif

