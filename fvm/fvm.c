
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

#include <fju/fvm.h>
#include <fju/mmf.h>

static struct {
  struct fvm_module *modules;
} glob;

int fvm_module_load( char *buf, int size, struct fvm_module **modulep ) {
  return -1;
}

int fvm_module_load_file( char *filename, struct fvm_module **modulep ) {
  struct mmf_s mmf;
  int sts;
  sts = mmf_open2( filename, &mmf, MMF_OPEN_EXISTING );
  if( sts ) return sts;
  mmf_remap( &mmf, mmf.fsize );
  sts = fvm_module_load( mmf.file, mmf.fsize, modulep );
  mmf_close( &mmf );
  return sts;
}

int fvm_module_unload( char *modname ) {
  struct fvm_module *m, *prev;
  m = glob.modules;
  prev = NULL;
  while( m ) {
    if( strcasecmp( m->name, modname ) == 0 ) {
      if( prev ) prev->next = m->next;
      else glob.modules = m->next;
      free( m );
      return 0;
    }
    m = m->next;
  }
  return -1;
}

struct fvm_module *fvm_module_by_name( char *name ) {
  struct fvm_module *m;
  m = glob.modules;
  while( m ) {
    if( strcasecmp( m->name, name ) == 0 ) return m;
    m = m->next;
  }
  return NULL;
}

struct fvm_module *fvm_module_by_progid( uint32_t progid, uint32_t versid ) {
  struct fvm_module *m;
  m = glob.modules;
  while( m ) {
    if( m->progid == progid && m->versid == versid ) return m;
    m = m->next;
  }
  return NULL;
}
    
int fvm_procid_by_name( struct fvm_module *module, char *procname ) {
  int i;
  for( i = 0; i < module->nprocs; i++ ) {
    if( strcasecmp( module->procs[i].name, procname ) == 0 ) return i;
  }
  return -1;
}

int fvm_init( struct fvm_state *state, struct fvm_module *m, uint32_t procid ) {
  return -1;
}

int fvm_run( struct fvm_state *state, char *argbuf, int arglen ) {
  return -1;
}

int fvm_results( struct fvm_state *state, char *argbuf, int *arglen ) {
  return -1;
}

void fvm_rpc_register( void ) {
}
