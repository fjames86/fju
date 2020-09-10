
#ifndef CHT_H
#define CHT_H

#include <fju/mmf.h>

#define CHT_KEY_SIZE 16
#define CHT_BLOCK_SIZE (16*1024)
#define CHT_DEFAULT_COUNT (1024)

struct cht_entry {
  uint8_t key[CHT_KEY_SIZE];  /* content hash */
  uint32_t seq;     /* block seqno (increments on write) */
  uint32_t flags;   /* block size and other flags */
#define CHT_SIZE_MASK 0x0000ffff /* mask block size 16k */  
};


struct cht_file;

struct cht_s {
  struct mmf_s mmf;
  struct cht_file *file;
  uint32_t count;
  uint32_t rdepth;
};

struct cht_opts {
  uint32_t mask;
#define CHT_OPT_COUNT 0x0001
  uint32_t count;
};

int cht_open( char *path, struct cht_s *cht, struct cht_opts *opts );
int cht_close( struct cht_s *cht );
 
struct cht_prop {
  uint32_t magic;
#define CHT_MAGIC 0xc871a4ec
  uint32_t version;
#define CHT_VERSION 1
  uint32_t seq;
  uint32_t count;
  uint32_t fill;
  
  uint32_t spare[27];
};

int cht_prop( struct cht_s *cht, struct cht_prop *prop );

/* 
 * read a block's metadata and data
 * entry : optional, receives block metadata
 * buf : optional, receives block data
 */
int cht_read( struct cht_s *cht, char *key, struct cht_entry *entry, char *buf, int size );

/* 
 * write a block. 
 * buf : block data
 * size : block data size 
 * flags : optional, write flags
 * entry : optional, receives the newly allocated block metadata
 */
int cht_write( struct cht_s *cht, char *buf, int size, uint32_t flags, struct cht_entry *entry );

int cht_delete( struct cht_s *cht, char *key );

#endif
