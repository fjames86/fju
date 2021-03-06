
/* Copyright Frank James 2018 */

#ifndef LOG_H
#define LOG_H

#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <fju/mmf.h>

#define LOG_LBASIZE 64               /* lba size fixed to 64 bytes */
#define LOG_MAX_COOKIE 16

/* caller data */
struct log_s {
  struct mmf_s mmf;                /* file handle */
  uint32_t pid;                    /* current process id */
  uint32_t ltag;                   /* log tag associated with all messages written */
  uint32_t flags;                  /* handle options, must be set after log_open */
#define LOG_VOLATILE     0x0000    /* writes are not guaranteed to flush until log_close or log_sync (unsafe but fastest) */
#define LOG_SYNC         0x0001    /* all writes are synchronously written (safest but slow) */
#define LOG_ASYNC        0x0002    /* all writes are asynchronously written (safeish but fast). LOG_SYNC takes priority over LOG_ASYNC */
#define LOG_NOLOCK       0x0004    /* no locks taken when writing to log */
  /*
   * typical time per write : volatile==1u, async==10us, sync==500us.
   */
    
};

struct log_opts {
  uint32_t mask;                   /* bitmask of opening options */
#define LOG_OPT_LBACOUNT 0x0001
#define LOG_OPT_FLAGS    0x0002
#define LOG_OPT_COOKIE   0x0004 
  uint32_t lbacount;               /* if creating log (opening for first time), use this many blocks */
  uint32_t flags;                  /* log flags - persistent properties of the log itself */
#define LOG_FLAG_CIRCULAR 0x0000   /* default - circular log */
#define LOG_FLAG_FIXED    0x0001   /* opposite of circular - only append entries at end */
#define LOG_FLAG_GROW     0x0002   /* dynamically grow, requires LOG_FLAG_FIXED */
#define LOG_FLAG_LVLMASK  0x00f0   /* only log messages with at least this level */
  char cookie[LOG_MAX_COOKIE];
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
  char cookie[LOG_MAX_COOKIE];
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

int log_sync( struct log_s *log, int sync );

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

/* set log cookie */
int log_set_cookie( struct log_s *log, char *cookie, int size );

#define _ltag_by_name(place,name) 
#define log_deflogger(_name,_tag) \
int _name( int lvl, char *fmt, ... ) {\
  va_list args;\
  char *tstr;\
  struct log_entry entry;\
  struct log_iov iov[1];\
  char buf[1024];\
\
  va_start(args,fmt);\
  vsnprintf( buf, sizeof(buf) - 1, fmt, args );  \
  va_end(args);\
  memset( &entry, 0, sizeof(entry) );\
  iov[0].buf = buf;\
  iov[0].len = (int)strlen( buf );\
  entry.iov = iov;\
  entry.niov = 1;\
  entry.flags = lvl & (~LOG_BINARY);\
  tstr = _tag;\
  strncpy( (char *)&entry.ltag, tstr, 4 );\
  return log_write( NULL, &entry );  \
}

#define LOG_TRUNC_START 0x0000
#define LOG_TRUNC_END   0x0001
int log_truncate( struct log_s *log, uint64_t id, uint32_t flags );
uint32_t log_default_set_flags( uint32_t mask, uint32_t flags );



#endif

