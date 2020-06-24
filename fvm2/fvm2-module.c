
#include <fju/mmf.h>
#include <fju/sec.h>

#include "fvm2-private.h"

static struct fvm2_module *modules;

int fvm2_module_load( char *filename ) {
  int sts, i;
  struct mmf_s mmf;
  struct fvm2_header header;
  struct fvm2_module *m;
  
  sts = mmf_open2( filename, &mmf, MMF_OPEN_EXISTING );
  if( sts ) return sts;

  sts = mmf_read( &mmf, (char *)&header, sizeof(header), 0 );
  if( sts < 0 ) goto done;

  m = malloc( sizeof(*m) + header.symcount*sizeof(struct fvm2_symbol) + header.datasize + header.textsize );
  m->header = header;
  m->symbols = (struct fvm2_symbol *)((char *)m + sizeof(*m));
  m->data = (uint8_t *)((char *)m->symbols + header.symcount*sizeof(struct fvm2_symbol));
  m->text = (uint8_t *)((char *)m->data + header.datasize);

  for( i = 0; i < header.symcount; i++ ) {
    mmf_read( &mmf, (char *)&m->symbols[i], sizeof(struct fvm2_symbol), sizeof(*m) + i*sizeof(struct fvm2_symbol) );
  }

  mmf_read( &mmf, (char *)m->data, header.datasize, sizeof(*m) + header.symcount * sizeof(struct fvm2_symbol) );
  mmf_read( &mmf, (char *)m->text, header.textsize, sizeof(*m) + header.symcount * sizeof(struct fvm2_symbol) + header.datasize );

  m->next = modules;
  modules = m;
  sts = 0;
  
 done:
  mmf_close( &mmf );
  return sts;
}

int fvm2_module_unload( char *name ) {
  struct fvm2_module *m, *prev;

  m = modules;
  prev = NULL;
  while( m ) {
    if( strcmp( m->header.name, name ) == 0 ) {
      if( prev ) prev->next = m->next;
      else modules = m->next;

      free( m );
      return 0;
    }

    prev = m;
    m = m->next;
  }

  return -1;
}

int fvm2_module_list( struct fvm2_module_info *minfo, int n ) {
  int i;
  struct fvm2_module *m;

  m = modules;
  i = 0;
  while( m ) {
    if( i < n ) {
      strcpy( minfo[i].name, m->header.name );
      memcpy( (char *)minfo[i].checksum, (char *)m->header.checksum, sizeof(m->header.checksum) );
      minfo[i].datasize = m->header.datasize;
      minfo[i].textsize = m->header.textsize;
    }
    i++;
    m = m->next;
  }
  
  return i;
}

struct fvm2_module *fvm2_module_by_name( char *name ) {
  struct fvm2_module *m;
  m = modules;
  while( m ) {
    if( strcmp( m->header.name, name ) == 0 ) return m;
    m = m->next;
  }
  return NULL;
}

int fvm2_module_symbols( char *name, struct fvm2_symbol *sym, int n ) {
  struct fvm2_module *m;

  m = fvm2_module_by_name( name );
  if( !name ) return -1;

  if( n > m->header.symcount ) n = m->header.symcount;
  if( n < m->header.symcount ) n = m->header.symcount;
  if( n > 0) memcpy( (char *)sym, (char *)m->symbols, n * sizeof(*sym) );

  return m->header.symcount;
}

uint32_t fvm2_symbol_addr( struct fvm2_module *m, char *name ) {
  int i;
  for( i = 0; i < m->header.symcount; i++ ) {
    if( strcmp( m->symbols[i].name, name ) == 0 ) return m->symbols[i].addr;
  }
  return 0;
}

