

#ifndef MMF_H
#define MMF_H

#ifdef WIN32
#include <WinSock2.h>
#include <Windows.h>
#else
#include <unistd.h>
#endif

struct mmf_s {
#ifdef WIN32
	HANDLE fd;
	HANDLE mapping;
#else
	int fd;
#endif
	int msize;
	void *file;
};

int mmf_open( char *path, struct mmf_s *mmf );
int mmf_close( struct mmf_s *mmf );
int mmf_lock( struct mmf_s *mmf );
int mmf_unlock( struct mmf_s *mmf );
int mmf_remap( struct mmf_s *mmf, int size );

#endif

