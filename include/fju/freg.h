
#ifndef FREG_H
#define FREG_H

#include <stdint.h>

#define FREG_MAX_NAME 64

struct freg_entry {
  char name[FREG_MAX_NAME];
  int len;
  uint32_t flags;
#define FREG_TYPE_MASK   0x000f 
#define FREG_TYPE_OPAQUE 0x0000    /* data contains opaque octets */
#define FREG_TYPE_UINT32 0x0001    /* data contains uint32 */
#define FREG_TYPE_UINT64 0x0002    /* data contains uint64 */
#define FREG_TYPE_STRING 0x0003    /* data contains string */
#define FREG_TYPE_KEY    0x0004    /* data contains id of child node */
};

int freg_open( void );
int freg_close( void );

int freg_list( uint64_t parentid, struct freg_entry *entry, int n );
int freg_get( uint64_t parentid, char *name, uint32_t *flags, char *buf, int len, int *lenp );
int freg_put( uint64_t parentid, char *name, uint32_t flags, char *buf, int len );
int freg_rem( uint64_t parentid, char *name );

#define FREG_CREATE 0x0001 
int freg_subkey( uint64_t parentid, char *name, uint32_t flags, uint64_t *id );

#endif

