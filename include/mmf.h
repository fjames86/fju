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

#ifndef MMF_H
#define MMF_H

#ifdef WIN32
#include <WinSock2.h>
#include <Windows.h>
#else
#include <unistd.h>
#endif

#include <stdarg.h>

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
int mmf_close( struct mmf_s *mmf );
int mmf_lock( struct mmf_s *mmf );
int mmf_unlock( struct mmf_s *mmf );
int mmf_remap( struct mmf_s *mmf, int size );
int mmf_sync( struct mmf_s *mmf );
char *mmf_default_path( char *filename, ... );
int mmf_ensure_dir( char *path );
int mmf_read( struct mmf_s *mmf, char *buf, int size, uint64_t offset );
int mmf_write( struct mmf_s *mmf, char *buf, int size, uint64_t offset );
int mmf_truncate( struct mmf_s *mmf, int size );

#endif

