
#ifndef CHT_PRIVATE_H
#define CHT_PRIVATE_H

int cht_entry_by_index( struct cht_s *cht, int idx, uint32_t seq, struct cht_entry *entry );

struct cht_alog_entry {
  uint32_t op;
#define CHT_ALOG_OP_WRITE    0x0000
#define CHT_ALOG_OP_DELETE   0x0001
#define CHT_ALOG_OP_SETFLAGS 0x0002
#define CHT_ALOG_OP_PURGE    0x0003
  struct cht_entry entry;
};

#endif

