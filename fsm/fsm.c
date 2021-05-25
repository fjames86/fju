
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include <fju/log.h>
#include <fju/fsm.h>
#include <fju/rpc.h>
#include <fju/freg.h>
#include <fju/sec.h>

struct fsm_s {
  uint64_t fsmid;
  uint64_t fregid;
  struct log_s log;
  char name[FSM_MAX_NAME];
};

#define MAX_FSM 32 
static struct {
  int nfsm;
  struct fsm_s fsm[MAX_FSM];
  uint32_t ocount;
  uint64_t rootid;
} glob;

static struct fsm_s *fsm_by_id( uint64_t id ) {
  int i;
  struct fsm_s *fsm;
  for( i = 0, fsm = glob.fsm; i < glob.nfsm; i++, fsm++ ) {
    if( fsm->fsmid == id ) return fsm;
  }
  return NULL;
}

static char *fsm_logpath( uint64_t fsmid ) {
  char idstr[64];
  sprintf( idstr, "%"PRIx64".log", fsmid );
  return mmf_default_path( "fsm", idstr, NULL );
}

int fsm_open( void ) {
  int sts, i;
  uint64_t id, rootid, fsmid;
  struct freg_entry entry;
  struct fsm_s *fsm;
  
  if( glob.ocount > 0 ) {
    glob.ocount++;
    return -1;
  }

  freg_open( NULL, NULL );
  
  sts = freg_subkey( NULL, 0, "/fju/fsm", FREG_CREATE, &rootid );
  if( sts ) return sts;

  mmf_ensure_dir( mmf_default_path( "fsm", NULL ) );

  glob.rootid = rootid;
  
  id = 0;
  i = 0;
  glob.nfsm = 0;
  while( !freg_next( NULL, rootid, id, &entry ) && (i < MAX_FSM) ) {
    if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_UINT64 ) {
      sts = freg_get( NULL, entry.id, NULL, (char *)&fsmid, sizeof(fsmid), NULL );
      if( !sts ) {
	fsm = &glob.fsm[i];
	fsm->fsmid = fsmid;
	fsm->fregid = entry.id;
	strncpy( fsm->name, entry.name, FSM_MAX_NAME - 1 );
	sts = log_open( fsm_logpath( fsmid ), NULL, &fsm->log );
	if( !sts ) {
	  i++;
	  glob.nfsm++;
	  fsm->log.flags |= LOG_NOLOCK;
	}
      }
    }
    
    id = entry.id;
  }

  glob.ocount = 1;
  
  return 0;
}


int fsm_list( struct fsm_info *info, int n ) {
  int i;

  for( i = 0; i < glob.nfsm; i++ ) {
    if( i < n ) {
      info[i].fsmid = glob.fsm[i].fsmid;
      strncpy( info[i].name, glob.fsm[i].name, FSM_MAX_NAME - 1 );
      log_prop( &glob.fsm[i].log, &info[i].logprop );
    }
  }
  return glob.nfsm;
}

int fsm_create( char *name, uint64_t *fsmidp ) {
  int i, sts;
  struct fsm_s *fsm;
  uint64_t fsmid;

  fsmid = *fsmidp;
  if( fsmid && fsm_by_id( fsmid ) ) return -1;

  if( !fsmid ) {
    sec_rand( &fsmid, 8 );
    while( fsm_by_id( fsmid ) ) {
      sec_rand( &fsmid, 8 );
    }
    *fsmidp = fsmid;
  }
  
  i = glob.nfsm;
  if( i >= MAX_FSM ) return -1;
  fsm = &glob.fsm[i];

  fsm->fsmid = fsmid;
  if( name ) strncpy( fsm->name, name, FSM_MAX_NAME - 1 );
  else sprintf( fsm->name, "%"PRIx64"", fsmid );

  freg_put( NULL, glob.rootid, fsm->name, FREG_TYPE_UINT64, (char *)&fsmid, sizeof(fsmid), &fsm->fregid );
  sts = log_open( fsm_logpath( fsmid ), NULL, &fsm->log );
  if( sts ) return -1;
  glob.nfsm++;

  return 0;
}

int fsm_delete( uint64_t fsmid ) {
  int i;
  struct fsm_s *fsm;
  char idstr[64];
  
  for( i = 0, fsm = glob.fsm; i < glob.nfsm; i++, fsm++ ) {
    if( fsm->fsmid == fsmid ) {
      log_close( &fsm->log );      
      mmf_delete_file( fsm_logpath( fsmid ) );
      freg_rem( NULL, fsm->fregid );

      /* delete snapshot files */
      sprintf( idstr, "%"PRIx64"-snapshot.dat.new", fsmid );
      mmf_delete_file( mmf_default_path( "fsm", idstr, NULL ) );
      
      sprintf( idstr, "%"PRIx64"-snapshot.dat", fsmid );
      mmf_delete_file( mmf_default_path( "fsm", idstr, NULL ) );
      
      if( i != (glob.nfsm - 1) ) glob.fsm[i] = glob.fsm[glob.nfsm - 1];
      glob.nfsm--;
      
      return 0;
    }
  }

  return -1;
}

int fsm_info( uint64_t fsmid, struct fsm_info *info ) {
  struct fsm_s *fsm;
  fsm = fsm_by_id( fsmid );
  if( !fsm ) return -1;
  
  info->fsmid = fsmid;
  strncpy( info->name, fsm->name, FSM_MAX_NAME - 1 );
  log_prop( &fsm->log, &info->logprop );
  return 0;
}

int fsm_command_save( uint64_t fsmid, struct log_iov *iov, int niov, uint64_t *seq ) {
  int sts;
  struct log_prop prop;
  struct log_entry entry;
  struct log_iov iov2[16];
  uint64_t lastseq;
  struct fsm_snapshot_info snapinfo;
  struct fsm_s *fsm;

  fsm = fsm_by_id( fsmid );
  if( !fsm ) return -1;
  
  lastseq = 0;
  sts = log_prop( &fsm->log, &prop );
  if( prop.last_id ) {
    memset( &entry, 0, sizeof(entry) );
    iov2[0].buf = (char *)&lastseq;
    iov2[0].len = sizeof(lastseq);
    entry.iov = iov2;
    entry.niov = 1;
    sts = log_read_entry( &fsm->log, prop.last_id, &entry );
  }

  sts = fsm_snapshot_load( fsmid, NULL, 0, &snapinfo );
  if( !sts && (snapinfo.seq > lastseq) ) lastseq = snapinfo.seq;

  lastseq++;
  
  memset( &entry, 0, sizeof(entry) );
  memcpy( &iov2[1], iov, niov * sizeof(*iov) );
  iov2[0].buf = (char *)&lastseq;
  iov2[0].len = sizeof(lastseq);
  entry.iov = iov2;
  entry.niov = niov + 1;
  entry.flags = LOG_BINARY;
  sts = log_write( &fsm->log, &entry );
  
  if( !sts && seq ) *seq = lastseq;
  
  return sts;
}

int fsm_command_load( uint64_t fsmid, uint64_t seq, struct log_iov *iov, int niov ) {
  int sts;
  struct log_entry entry;
  struct log_iov iov2[16];
  uint64_t ss;
  int ne;
  struct fsm_s *fsm;

  fsm = fsm_by_id( fsmid );
  if( !fsm ) return -1;

  memset( &entry, 0, sizeof(entry) );
  iov2[0].buf = (char *)&ss;
  iov2[0].len = sizeof(ss);
  entry.iov = iov2;
  entry.niov = 1;
  sts = log_read( &fsm->log, 0, &entry, 1, &ne );
  while( !sts && ne ) {
    if( ss == seq ) {
      memcpy( (char *)&iov2[1], iov, sizeof(*iov) * niov );
      iov2[0].buf = (char *)&ss;
      iov2[0].len = sizeof(ss);
      entry.niov = niov + 1;
      log_read_entry( &fsm->log, entry.id, &entry );
      return entry.msglen - sizeof(ss);
    }
    
    sts = log_read( &fsm->log, entry.id, &entry, 1, &ne );
  }

  return -1;
}

int fsm_command_info( uint64_t fsmid, struct fsm_command_info *info ) {
  int sts;
  struct fsm_s *fsm;
  struct log_prop prop;
  uint64_t ss;
  struct log_iov iov[1];
  struct log_entry entry;
  
  fsm = fsm_by_id( fsmid );
  if( !fsm ) return -1;
		      
  log_prop( &fsm->log, &prop );
  if( prop.last_id ) {
    memset( &entry, 0, sizeof(entry) );
    entry.iov = iov;
    entry.niov = 1;
    iov[0].buf = (char *)&ss;
    iov[0].len = sizeof(ss);
    sts = log_read_entry( &fsm->log, prop.last_id, &entry );
    if( sts ) return -1;
    info->seq = ss;
    info->when = entry.timestamp;
    info->len = entry.msglen;
  } else {
    info->seq = 0;
    info->when = 0;
    info->len = 0;    
  }

  return 0;
}

int fsm_command_list( uint64_t fsmid, struct fsm_command_info *clist, int n ) {
  struct log_entry entry;
  struct log_iov iov[1];
  uint64_t ss;
  int ne, i, sts;
  struct fsm_s *fsm;

  fsm = fsm_by_id( fsmid );
  if( !fsm ) return -1;
  
  i = 0;
  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = (char *)&ss;
  iov[0].len = sizeof(ss);
  entry.iov = iov;
  entry.niov = 1;
  sts = log_read( &fsm->log, 0, &entry, 1, &ne );
  while( !sts && ne ) {
    if( i < n ) {
      clist[i].seq = ss;
      clist[i].when = entry.timestamp;
      clist[i].len = entry.msglen - 8;
    }
    i++;
    
    sts = log_read( &fsm->log, entry.id, &entry, 1, &ne );
  }

  return i;
}

int fsm_command_truncate( uint64_t fsmid, uint64_t seq ) {
  struct log_entry entry;
  struct log_iov iov[1];
  uint64_t ss;
  int ne, sts;
  struct fsm_s *fsm;
  uint64_t previd;
  
  fsm = fsm_by_id( fsmid );
  if( !fsm ) return -1;
  
  memset( &entry, 0, sizeof(entry) );
  iov[0].buf = (char *)&ss;
  iov[0].len = sizeof(ss);
  entry.iov = iov;
  entry.niov = 1;
  previd = 0;
  sts = log_read( &fsm->log, 0, &entry, 1, &ne );
  while( !sts && ne ) {
    if( ss == seq ) {
      return log_truncate( &fsm->log, entry.id, LOG_TRUNC_START );      
    }

    previd = entry.id;
    sts = log_read( &fsm->log, entry.id, &entry, 1, &ne );
  }

  return -1;
}

struct snapshot_header {
#define FSM_SNAPSHOT_MAGIC 0x12332122
  uint32_t magic;
#define FSM_SNAPSHOT_VERSION 1
  uint32_t version;
  uint64_t fsmid;
  uint64_t seq;
  uint32_t len;
  int complete;
  uint64_t when;
  uint32_t spare[5];
};

int fsm_snapshot_save( uint64_t fsmid, uint64_t seq, struct log_iov *iov, int niov ) {
  int sts, len, offset, i;
  struct mmf_s mmf;
  char idstr[64], idstr2[64];
  char *path;
  struct snapshot_header hdr;

  sprintf( idstr, "%"PRIx64"-snapshot.dat.new", fsmid );
  path = mmf_default_path( "fsm", idstr, NULL );

  /* start new file */
  mmf_delete_file( path );

  sts = mmf_open( path, &mmf );
  if( sts ) return sts;

  len = 0;
  for( i = 0; i < niov; i++ ) len += iov[i].len;
  
  memset( &hdr, 0, sizeof(hdr) );
  hdr.magic = FSM_SNAPSHOT_MAGIC;
  hdr.version = FSM_SNAPSHOT_VERSION;
  hdr.fsmid = fsmid;
  hdr.seq = seq;
  hdr.len = len;
  mmf_write( &mmf, (char *)&hdr, sizeof(hdr), 0 );

  offset = sizeof(hdr);
  for( i = 0; i < niov; i++ ) {
    mmf_write( &mmf, iov[i].buf, iov[i].len, offset );
    offset += iov[i].len;
  }

  /* final block - mark as complete */
  hdr.complete = 1;
  hdr.when = time( NULL );
  mmf_write( &mmf, (char *)&hdr, sizeof(hdr), 0 );
  
  mmf_close( &mmf );
  
  /* rename over top of old */
  sprintf( idstr2, "%"PRIx64"-snapshot.dat", fsmid );
  mmf_rename( mmf_default_path( "fsm", NULL ), idstr, idstr2 );

  return 0;
}

int fsm_snapshot_load( uint64_t fsmid, struct log_iov *iov, int niov, struct fsm_snapshot_info *info ) {
  int sts;
  struct mmf_s mmf;
  struct snapshot_header hdr;
  char idstr[64];
  char *path;
  int offset, i, tocopy;
  
  sprintf( idstr, "%"PRIx64"-snapshot.dat", fsmid );
  path = mmf_default_path( "fsm", idstr, NULL );

  sts = mmf_open2( path, &mmf, MMF_OPEN_EXISTING );
  if( sts ) return -1;

  sts = -1;
  if( mmf.fsize < sizeof(hdr) ) goto done;
  mmf_read( &mmf, (char *)&hdr, sizeof(hdr), 0 );
  if( hdr.magic != FSM_SNAPSHOT_MAGIC ) goto done;
  if( hdr.version != FSM_SNAPSHOT_VERSION ) goto done;
  if( hdr.fsmid != fsmid ) goto done;
  if( mmf.fsize != (hdr.len + sizeof(hdr)) ) goto done;

  if( info ) {
    info->seq = hdr.seq;
    info->len = hdr.len;
    info->when = hdr.when;
  }
  
  offset = sizeof(hdr);
  for( i = 0; i < niov; i++ ) {
    tocopy = iov[i].len;
    if( tocopy > (hdr.len - offset) ) tocopy = hdr.len - offset;
    if( iov[i].buf ) mmf_read( &mmf, iov[i].buf, tocopy, offset );
    iov[i].len = tocopy;
    offset += tocopy;
  }
  
  sts = 0;
 done:
  mmf_close( &mmf );
  
  return sts;
}
    
