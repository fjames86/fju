
#include "mmf.h"

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
	memset( &overlap, 0, sizeof(overlap) );	
	LockFileEx( mmf->fd, LOCKFILE_EXCLUSIVE_LOCK, 0, 1, 0, &overlap );
	return 0;
}

int mmf_unlock( struct mmf_s *mmf ) {
	OVERLAPPED overlap;
	memset( &overlap, 0, sizeof(overlap) );
	UnlockFileEx( mmf->fd, 0, 0, 1, &overlap );
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

	mmf->mapping = CreateFileMappingA( mmf->fd, NULL, 0, 0, 0, NULL );
	mmf->file = MapViewOfFile( mmf->mapping, 0, 0, 0, size );
	mmf->msize = size;
	return 0;
}

#else
int mmf_open( char *path, struct mmf_s *mmf ) {
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
	return sts;
}

int mmf_remap( struct mmf_s *mmf, int size ) {
	int fsize;

	if( mmf->file ) munmap( mmf->file, mmf->msize );
	
	fsize = lseek( mmf->fd, 0, SEEK_END );
	if( fsize < size ) {
		pwrite( mff->fd, "", 1, size - 1 );
	}
	mmf->file = mmap( NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, mmf->fd, 0 );
	mmf->msize = size;
	return 0;
}

#endif
