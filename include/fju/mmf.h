
#ifndef MMF_H
#define MMF_H

#ifdef WIN32
#include <WinSock2.h>
#include <Windows.h>
#else
#include <unistd.h>
#endif

#include <stdarg.h>
#include <stdint.h>

#define MMF_DEFAULT_PREFIX "fju"

struct mmf_s {
#ifdef WIN32
	HANDLE fd;
	HANDLE mapping;
#else
	int fd;
#endif
	int msize;
	void *file;
	int fsize;
};

int mmf_open( char *path, struct mmf_s *mmf );
#define MMF_OPEN_ALWAYS   0x0000
#define MMF_OPEN_EXISTING 0x0001
#define MMF_OPEN_ASYNC    0x0002 
int mmf_open2( char *path, struct mmf_s *mmf, uint32_t flags );
int mmf_close( struct mmf_s *mmf );
int mmf_lock( struct mmf_s *mmf );
int mmf_unlock( struct mmf_s *mmf );
int mmf_remap( struct mmf_s *mmf, int size );

/* flush changes to disk. if sync=1 do it synchronously, otherwise initiate async flushing */
#define MMF_SYNC_NOW   1
#define MMF_SYNC_LATER 0
int mmf_sync( struct mmf_s *mmf, int sync );

char *mmf_default_path( char *filename, ... );
int mmf_ensure_dir( char *path );
int mmf_read( struct mmf_s *mmf, char *buf, int size, uint64_t offset );
int mmf_write( struct mmf_s *mmf, char *buf, int size, uint64_t offset );
int mmf_truncate( struct mmf_s *mmf, int size );
int mmf_delete_file( char *path );
int mmf_rename( char *dirpath, char *oldname, char *newname );
int mmf_fsize( struct mmf_s *mmf );

#endif

