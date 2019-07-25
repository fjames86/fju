/*
 * MIT License
 *
 * Copyright (c) 2018 Frank James
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "mmf.h"

#ifdef WIN32
#include <Shlobj.h>
#else
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#include <sys/file.h>
#include <sys/mman.h>
#include <sys/stat.h>
#endif

#ifdef WIN32
int mmf_open( char *path, struct mmf_s *mmf ) {

	memset( mmf, 0, sizeof(*mmf) );

	mmf->fd = CreateFileA( path, GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
	if( mmf->fd == INVALID_HANDLE_VALUE ) return -1;

	return 0;
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
int mmf_sync( struct mmf_s *mmf ) {
  if( mmf->file ) FlushViewOfFile( mmf->file, mmf->msize );
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

#else
int mmf_open( char *path, struct mmf_s *mmf ) {
	memset( mmf, 0, sizeof(*mmf) );
	mmf->fd = open( path, O_RDWR|O_CREAT, 0600 );
	if( mmf->fd < 0 ) return -1;
	return 0;
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
int mmf_sync( struct mmf_s *mmf ) {
  if( mmf->file ) msync( mmf->file, mmf->msize, MS_SYNC );
  return 0;
}

int mmf_remap( struct mmf_s *mmf, int size ) {
	int fsize;

	if( mmf->file ) munmap( mmf->file, mmf->msize );
	
	fsize = lseek( mmf->fd, 0, SEEK_END );
	if( fsize < size ) {
		pwrite( mmf->fd, "", 1, size - 1 );
	}
	mmf->file = mmap( NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, mmf->fd, 0 );
	mmf->msize = size;
	return 0;
}

char *mmf_default_path( char *filename, ... ) {
	static char path[256];

	strcpy( path, "/etc" );
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


#endif
