
#ifndef FJUI_H
#define FJUI_H

#define _CRT_SECURE_NO_WARNINGS

#define strcasecmp _stricmp

#include <Winsock2.h>
#include <Windows.h>

#include <stdint.h>
#include <inttypes.h>
#include <stdarg.h>

#include <fju/lic.h>
#include <fju/fvm.h>
#include <fju/hostreg.h>
#include <fju/mmf.h>
#include <fju/sec.h>
#include <fju/hrauth.h>
#include <fju/log.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>

void fjui_set_font( HWND hwnd );
void fjui_net_service( void );
void fjui_hwnd_register( char *name, HWND hwnd );
HWND fjui_get_hwnd( char *name );


struct fjui_hostinfo {
	struct fjui_host_info *next;

	uint64_t hostid;
	char name[256];
	struct lic_s licinfo;
	struct fvm_module *modules;
};
struct fjui_hostinfo *fjui_hostinfo_by_name( char *name );
struct fjui_hostinfo *fjui_hostinfo_by_id( uint64_t hostid );
struct fjui_hostinfo *fjui_hostinfo_add( uint64_t hostid );

HINSTANCE fjui_hinstance( void );

void fjui_set_statusbar( int index, char *fmt, ... );
void fjui_call_getlicinfo( uint64_t hostid );
uint64_t fjui_hostid( void );

#endif

