
#include "fvm-private.h"

#include <fju/log.h>

static struct log_s *auditlog( void ) {
  static struct log_s log;
  static int initialized = 0;

  if( !initialized ) {
    log_open( mmf_default_path( "fvmaudit.log", NULL ), NULL, &log );
    initialized = 1;
  }

  return &log;
}

int fvm_audit_reset( void ) {
  log_reset( auditlog() );
  return 0;
}

struct fvm_audit_header {
  uint32_t progid;
  uint32_t procid;

  uint32_t spare[6];
};

int fvm_audit_write( uint32_t progid, uint32_t procid, char *args, int len ) {
  struct log_entry entry;
  struct log_iov iov[2];
  struct fvm_audit_header header;

  memset( &header, 0, sizeof(header) );
  header.progid = progid;
  header.procid = procid;
  
  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = (char *)&header;
  iov[0].len = sizeof(header);
  iov[1].buf = args;
  iov[1].len = len;
  entry.iov = iov;
  entry.niov = 2;
  entry.flags = LOG_LVL_INFO|LOG_BINARY;
  log_write( auditlog(), &entry );

  return 0;
}

uint64_t fvm_audit_read( uint64_t nextid, uint32_t *progid, uint32_t *procid, char *args, int len, int *lenp ) {
  int sts, ne;
  struct log_entry entry;
  struct log_iov iov[2];
  struct fvm_audit_header header;
  
  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = (char *)&header;
  iov[0].len = sizeof(header);
  iov[1].buf = args;
  iov[1].len = len;  
  entry.iov = iov;
  entry.niov = 2;

  sts = log_read( auditlog(), nextid, &entry, 1, &ne );
  if( sts || !ne ) return 0;

  if( entry.msglen < sizeof(header) ) return 0;
  
  *progid = header.progid;
  *procid = header.procid;
  *lenp = entry.msglen - sizeof(header);
  
  return entry.id;
}

int fvm_audit_replay( void ) {
  struct fvm_s state;
  uint32_t progid, procid;
  uint64_t id;
  char args[FVM_MAX_STACK];
  int len, sts;
  
  id = 0;
  while( (id = fvm_audit_read( id, &progid, &procid, args, sizeof(args), &len )) ) {
    sts = fvm_state_init( &state, progid, procid );
    if( sts ) continue;

    fvm_set_args( &state, args, len );
    sts = fvm_run( &state, 0 );
  }

  return 0;
}
