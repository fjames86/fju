
#ifndef LIC_H
#define LIC_H

#include <fju/sec.h>

struct lic_s {
  uint64_t hostid; /* hostid */
  uint64_t expire; /* expiry date */
  uint32_t version;
  uint32_t flags;
  uint32_t spare[2];
  uint32_t nverf;
  char verf[SEC_MAX_SIG];
};

int fju_check_license( char *licbuf, int size, struct lic_s *licp );
		       
#endif

