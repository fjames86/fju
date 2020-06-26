
#include <fju/mmf.h>
#include <fju/sec.h>

#include "fvm2-private.h"

static struct fvm2_module *modules;

int fvm2_module_load( char *filename, char *name ) {
  int sts;
  struct mmf_s mmf;
  struct fvm2_header header;
  struct fvm2_module *m;
  
  sts = mmf_open2( filename, &mmf, MMF_OPEN_EXISTING );
  if( sts ) return sts;

  memset( &header, 0, sizeof(header) );
  sts = mmf_read( &mmf, (char *)&header, sizeof(header), 0 );
  if( sts != sizeof(header) ) {
    sts = -1;
    goto done;
  }

  if( header.magic != FVM2_MAGIC ) {
    sts = -1;
    goto done;
  }
  if( header.version != FVM2_VERSION ) {
    sts = -1;
    goto done;
  }      

  if( name ) strncpy( name, header.name, FVM2_MAX_NAME - 1 );
  
  if( fvm2_module_by_name( header.name ) ) {
    sts = -1;
    goto done;
  }

  if( fvm2_module_by_progid( header.progid ) ) {
    sts = -1;
    goto done;
  }
  
  
  m = malloc( sizeof(*m) + header.symcount*sizeof(struct fvm2_symbol) + header.datasize + header.textsize );
  m->header = header;
  m->symbols = (struct fvm2_symbol *)(((char *)m) + sizeof(*m));
  m->data = (uint8_t *)(((char *)m->symbols) + header.symcount*sizeof(struct fvm2_symbol));
  m->text = (uint8_t *)(((char *)m->data) + header.datasize);

  mmf_read( &mmf, (char *)m->symbols, sizeof(struct fvm2_symbol) * header.symcount, sizeof(struct fvm2_header) );
  mmf_read( &mmf, (char *)m->data, header.datasize, sizeof(header) + header.symcount * sizeof(struct fvm2_symbol) );
  mmf_read( &mmf, (char *)m->text, header.textsize, sizeof(header) + header.symcount * sizeof(struct fvm2_symbol) + header.datasize );

  m->next = modules;
  modules = m;
  sts = 0;
	       
 done:
  mmf_close( &mmf );
  return sts;
}

int fvm2_module_register( char *buf, int size, char *name ) {
  struct fvm2_header header;
  struct fvm2_module *m;
  
  memcpy( &header, buf, sizeof(header) );
  if( header.magic != FVM2_MAGIC ) return -1;
  if( header.version != FVM2_VERSION ) return -1;
  if( size != (sizeof(header) + header.symcount*sizeof(struct fvm2_symbol) + header.datasize + header.textsize) ) {
    return -1;
  }
  if( name ) strncpy( name, header.name, FVM2_MAX_NAME - 1 );
  if( fvm2_module_by_name( header.name ) ) return -1;
  if( fvm2_module_by_progid( header.progid ) ) return -1;

  
  m = malloc( sizeof(*m) + header.symcount*sizeof(struct fvm2_symbol) + header.datasize + header.textsize );
  m->header = header;
  m->symbols = (struct fvm2_symbol *)(((char *)m) + sizeof(*m));
  m->data = (uint8_t *)(((char *)m->symbols) + header.symcount*sizeof(struct fvm2_symbol));
  m->text = (uint8_t *)(((char *)m->data) + header.datasize);

  memcpy( (char *)m->symbols, buf + sizeof(struct fvm2_header), sizeof(struct fvm2_symbol) * header.symcount );
  memcpy( (char *)m->data, buf + sizeof(header) + header.symcount * sizeof(struct fvm2_symbol), header.datasize );
  memcpy( (char *)m->text, buf + sizeof(header) + header.symcount * sizeof(struct fvm2_symbol) + header.datasize, header.textsize );

  m->next = modules;
  modules = m;
  
  return 0;
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
      minfo[i].progid = m->header.progid;
      minfo[i].versid = m->header.versid;
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
    if( strcasecmp( m->header.name, name ) == 0 ) return m;
    m = m->next;
  }
  return NULL;
}

struct fvm2_module *fvm2_module_by_progid( uint32_t progid ) {
  struct fvm2_module *m;
  m = modules;
  while( m ) {
    if( m->header.progid == progid ) return m;
    m = m->next;
  }
  return NULL;
}

int fvm2_module_symbols( char *name, struct fvm2_symbol *sym, int n ) {
  struct fvm2_module *m;
    
  m = fvm2_module_by_name( name );
  if( !m ) return -1;

  if( n > m->header.symcount ) n = m->header.symcount;
  if( n > 0) memcpy( (char *)sym, (char *)m->symbols, n * sizeof(*sym) );

  return m->header.symcount;
}

uint32_t fvm2_symbol_addr( struct fvm2_module *m, char *name ) {
  int i;
  for( i = 0; i < m->header.symcount; i++ ) {
    if( strcasecmp( m->symbols[i].name, name ) == 0 ) return m->symbols[i].addr;
  }
  return 0;
}

uint32_t fvm2_symbol_by_index( struct fvm2_module *m, uint32_t index ) {
  return (index < m->header.symcount) ? m->symbols[index].addr : 0;
}


