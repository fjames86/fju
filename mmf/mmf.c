
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <fju/mmf.h>
#include <stdio.h>

#ifdef WIN32
#include <Shlobj.h>
#else
#include <stdlib.h>

#ifdef __linux__
#define _GNU_SOURCE
#define __USE_GNU 1
#endif 
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#include <sys/file.h>
#include <sys/mman.h>
#include <sys/stat.h>
#endif

#ifdef WIN32
int mmf_open2( char *path, struct mmf_s *mmf, uint32_t flags ) {

	memset( mmf, 0, sizeof(*mmf) );

	mmf->fd = CreateFileA( path, GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, flags & MMF_OPEN_EXISTING ? OPEN_EXISTING : OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL | (flags & MMF_OPEN_ASYNC ? FILE_FLAG_OVERLAPPED|FILE_FLAG_NO_BUFFERING : 0), NULL );
	if( mmf->fd == INVALID_HANDLE_VALUE ) return -1;

	mmf->fsize = (int)GetFileSize( mmf->fd, NULL );
	
	return 0;
}
int mmf_open( char *path, struct mmf_s *mmf ) {
  return mmf_open2( path, mmf, 0 );
}

int mmf_close( struct mmf_s *mmf ) {
	if( mmf->file ) UnmapViewOfFile( mmf->file );
	if( mmf->mapping ) CloseHandle( mmf->mapping );
	if( mmf->fd ) CloseHandle( mmf->fd );
	memset( mmf, 0, sizeof(*mmf) );
	return 0;
}

int mmf_lock( struct mmf_s *mmf ) {
	OVERLAPPED overlap;
	BOOL b;
	memset( &overlap, 0, sizeof(overlap) );	
	b = LockFileEx( mmf->fd, LOCKFILE_EXCLUSIVE_LOCK, 0, 1, 0, &overlap );
	if( !b ) return -1;
	return 0;
}

int mmf_unlock( struct mmf_s *mmf ) {
	OVERLAPPED overlap;
	BOOL b;
	memset( &overlap, 0, sizeof(overlap) );
	b = UnlockFileEx( mmf->fd, 0, 1, 0, &overlap );
	if( !b ) return -1;
	//if( mmf->file ) FlushViewOfFile( mmf->file, mmf->msize );
	return 0;
}
int mmf_sync( struct mmf_s *mmf, int sync ) {
  if( mmf->file ) FlushViewOfFile( mmf->file, mmf->msize );
  
  if( sync ) {
    if( mmf->fd ) FlushFileBuffers( mmf->fd );
  }
  
  return 0;
}

int mmf_remap( struct mmf_s *mmf, int size ) {
	int fsize;

	if( mmf->file ) UnmapViewOfFile( mmf->file );
	if( mmf->mapping ) CloseHandle( mmf->mapping );

	fsize = (int)GetFileSize( mmf->fd, NULL );
	if( fsize < size ) {
		OVERLAPPED overlap;
		memset( &overlap, 0, sizeof(overlap) );
		overlap.Offset = size - 1;
		WriteFile( mmf->fd, "", 1, NULL, &overlap );
		mmf->fsize = size;
	}
	
	mmf->mapping = CreateFileMappingA( mmf->fd, NULL, PAGE_READWRITE, 0, 0, NULL );
	if( !mmf->mapping ) return -1;
	mmf->file = MapViewOfFile( mmf->mapping, FILE_MAP_READ|FILE_MAP_WRITE, 0, 0, size );
	if( !mmf->file ) return -1;
	mmf->msize = size;
	return 0;
}

char *mmf_default_path( char *filename, ... ) {
	static char path[256];

	wchar_t *wp;
	DWORD sts;

	strcpy( path, "" );
	sts = SHGetKnownFolderPath( &FOLDERID_ProgramData, 0, NULL, &wp );
	if( sts ) return path;

	wcstombs( path, wp, sizeof(path) );
	strcat( path, "\\" MMF_DEFAULT_PREFIX );

	mmf_ensure_dir( path );
	
	if( filename ) {
	  va_list args;
	  char *p;
	  
	  strcat( path, "\\" );
	  strcat( path, filename );
	  
	  va_start( args, filename );
	  do {
	    p = va_arg( args, char * );
	    if( p ) {
	      strcat( path, "\\" );
	      strcat( path, p );
	    }
	  } while( p );
	  va_end( args );
	}

	CoTaskMemFree( wp );

	return path;
}

int mmf_ensure_dir( char *path ) {
  CreateDirectoryA( path, NULL );
  return 0;
}

int mmf_read( struct mmf_s *mmf, char *buf, int size, uint64_t offset ) {
    OVERLAPPED overlap;
    uint32_t nbytes;
    BOOL b;
    
    memset( &overlap, 0, sizeof(overlap) );
    overlap.Offset = offset & 0xffffffff;
    overlap.OffsetHigh = (offset >> 32) & 0xffffffff;
    b = ReadFile( mmf->fd, buf, size, &nbytes, &overlap );
    if( !b ) return -1;
    return nbytes;
}

int mmf_write( struct mmf_s *mmf, char *buf, int size, uint64_t offset ) {
    OVERLAPPED overlap;
    uint32_t nbytes;
    BOOL b;
    
    memset( &overlap, 0, sizeof(overlap) );
    overlap.Offset = offset & 0xffffffff;
    overlap.OffsetHigh = (offset >> 32) & 0xffffffff;
    b = WriteFile( mmf->fd, buf, size, &nbytes, &overlap );
    if( !b ) return -1;
    return nbytes;
}

int mmf_truncate( struct mmf_s *mmf, int size ) {
  SetFilePointer( mmf->fd, size, NULL, FILE_BEGIN );
  SetEndOfFile( mmf->fd );
  return 0;
}

int mmf_delete_file( char *path ) {
  DeleteFileA( path );
  return 0;
}

int mmf_rename( char *dirpath, char *oldname, char *newname ) {
  char oldpath[256], newpath[256];
  sprintf( oldpath, "%s\\%s", dirpath, oldname );
  sprintf( newpath, "%s\\%s", dirpath, newname );
  MoveFileExA( oldpath, newpath, MOVEFILE_REPLACE_EXISTING );
  // Maybe use ReplaceFileA( oldpath, newpath, NULL, 0, NULL, NULL ); */
  return 0;
}

int mmf_fsize( struct mmf_s *mmf ) {
  return (int)GetFileSize( mmf->fd, NULL );
}

#else
int mmf_open2( char *path, struct mmf_s *mmf, uint32_t flags ) {
	memset( mmf, 0, sizeof(*mmf) );
	mmf->fd = open( path, O_RDWR|(flags & MMF_OPEN_EXISTING ? 0 : O_CREAT)|(flags & MMF_OPEN_ASYNC ? O_DIRECT : 0), 0666 );
	if( mmf->fd < 0 ) return -1;
	mmf->fsize = lseek( mmf->fd, 0, SEEK_END );
	return 0;
}
int mmf_open( char *path, struct mmf_s *mmf ) {
  return mmf_open2( path, mmf, 0 );
}

int mmf_close( struct mmf_s *mmf ) {
	if( mmf->file ) munmap( mmf->file, mmf->msize );
	if( mmf->fd != -1 ) close( mmf->fd );
	memset( mmf, 0, sizeof(*mmf) );
	return 0;
}

int mmf_lock( struct mmf_s *mmf ) {
	int sts;
	do {
		sts = flock( mmf->fd, LOCK_EX );
	} while( (sts < 0) && (errno == EINTR) );
	return sts;
}
int mmf_unlock( struct mmf_s *mmf ) {
	int sts;
	do {
		sts = flock( mmf->fd, LOCK_UN );
	} while( (sts < 0) && (errno == EINTR) );
	if( sts ) return sts;
	//if( mmf->file ) msync( mmf->file, mmf->msize, MS_SYNC );
	return 0;
}
int mmf_sync( struct mmf_s *mmf, int sync ) {
  if( mmf->file ) msync( mmf->file, mmf->msize, sync ? MS_SYNC : MS_ASYNC );
  
  if( sync ) {
    if( mmf->fd ) fsync( mmf->fd );
  }
  
  return 0;
}

int mmf_remap( struct mmf_s *mmf, int size ) {
	int fsize;

	if( mmf->file ) munmap( mmf->file, mmf->msize );
	
	fsize = lseek( mmf->fd, 0, SEEK_END );
	if( fsize < size ) {
		pwrite( mmf->fd, "", 1, size - 1 );
		mmf->fsize = size;
	}
	
	mmf->file = mmap( NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, mmf->fd, 0 );
	mmf->msize = size;
	return 0;
}

char *mmf_default_path( char *filename, ... ) {
	static char path[256];

	strcpy( path, "/opt/" MMF_DEFAULT_PREFIX );
	mmf_ensure_dir( path );
	if( filename ) {
	  va_list args;
	  char *p;
	  
	  strcat( path, "/" );
	  strcat( path, filename );
	  
	  va_start( args, filename );
	  do {
	    p = va_arg( args, char * );
	    if( p ) {
	      strcat( path, "/" );
	      strcat( path, p );
	    }
	  } while( p );
	  va_end( args );
	}

	return path;
}

int mmf_ensure_dir( char *path ) {
  mkdir( path, 0755 );
  return 0;
}

int mmf_read( struct mmf_s *mmf, char *buf, int size, uint64_t offset ) {
    return pread( mmf->fd, buf, size, offset );
}

int mmf_write( struct mmf_s *mmf, char *buf, int size, uint64_t offset ) {
    return pwrite( mmf->fd, buf, size, offset );
}

int mmf_truncate( struct mmf_s *mmf, int size ) {
  ftruncate( mmf->fd, size );
  return 0;
}

int mmf_delete_file( char *path ) {
  unlink( path );
  return 0;
}

int mmf_rename( char *dirpath, char *oldname, char *newname ) {
  char oldpath[256], newpath[256];
  sprintf( oldpath, "%s/%s", dirpath, oldname );
  sprintf( newpath, "%s/%s", dirpath, newname );
  rename( oldpath, newpath );
  return 0;
}

int mmf_fsize( struct mmf_s *mmf ) {
  return lseek( mmf->fd, 0, SEEK_END );
}

#endif
