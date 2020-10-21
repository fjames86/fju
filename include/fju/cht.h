
#ifndef CHT_H
#define CHT_H

#include <fju/mmf.h>
#include <fju/log.h>
#include <fju/sec.h>

#define CHT_KEY_SIZE 16 /* key into database i.e. content hash */
#define CHT_BLOCK_SIZE (16*1024)
#define CHT_DEFAULT_COUNT (1024)
#define CHT_MAX_COOKIE 8

struct cht_entry {
  uint8_t key[CHT_KEY_SIZE];  /* key into database, e.g. content hash */
  uint32_t seq;     /* entry seqno. starts at 1, increments on write */
  uint32_t flags;   /* block size and other entry flags */
#define CHT_SIZE_MASK 0x0000ffff /* mask for entry size 16k */
#define CHT_STICKY    0x00010000 /* sticky: never evict entry */
#define CHT_READONLY  0x00020000 /* readonly: entry can never be updated */
  char cookie[CHT_MAX_COOKIE]; /* private data */
};

struct cht_file;

struct cht_s {
  struct mmf_s mmf;
  struct cht_file *file;
  uint32_t count;  /* database size, max entry count */
  uint32_t rdepth; /* recursion depth, scales as sqrt(count) */
  uint32_t flags;
#define CHT_AUDIT  0x0001 
  struct log_s alog;
};

struct cht_opts {
  uint32_t mask;
#define CHT_OPT_COUNT   0x0001    /* set the initial count if creating the table */
#define CHT_OPT_ALOG    0x0002   /* write all write/delete/purge/setflags operations to the audit log */
  uint32_t count;
  uint64_t alog_hshare;
};

int cht_open( char *path, struct cht_s *cht, struct cht_opts *opts );
int cht_close( struct cht_s *cht );
 
struct cht_prop {
  uint32_t magic;
#define CHT_MAGIC 0xc871a4ec
  uint32_t version;
#define CHT_VERSION 1
  uint64_t tag;     /* database identifier */
  uint64_t seq;     /* database seqno */
  uint32_t count;   /* database size */
  uint32_t fill;    /* database fill count */
  uint64_t alog_hshare;
  
  uint32_t spare[22]; /* future expansion */
};

int cht_prop( struct cht_s *cht, struct cht_prop *prop );

/* 
 * read a block's metadata and data
 * entry : optional, receives block metadata
 * buf : optional, receives block data
 */
int cht_read( struct cht_s *cht, char *key, char *buf, int size, struct cht_entry *entry );
int cht_read2( struct cht_s *cht, char *key, struct sec_buf *iov, int niov, struct cht_entry *entry );

/* 
 * write a block. 
 * buf : block data
 * size : block data size 
 * entry : key and flags must be set, seq receives entry seqno
 */
int cht_write( struct cht_s *cht, struct cht_entry *entry, char *buf, int size );
int cht_write2( struct cht_s *cht, struct cht_entry *entry, struct sec_buf *iov, int niov );

/* 
 * Delete an entry 
 */ 
int cht_delete( struct cht_s *cht, char *key );

/*
 * Delete all entries with matching flags 
 * i.e. those entries which have entry.flags & mask == flags 
 */
int cht_purge( struct cht_s *cht, uint32_t mask, uint32_t flags );

/*
 * Set flags on a given entry
 */
int cht_set_flags( struct cht_s *cht, char *key, uint32_t mask, uint32_t flags );

int cht_set_alog( struct cht_s *cht, uint64_t alog_hshare );

#endif
