
#ifndef FJUI_H
#define FJUI_H

#include <Winsock2.h>
#include <Windows.h>

#include <stdint.h>

void fjui_set_font( HWND hwnd );
void fjui_net_service( void );
void fjui_hwnd_register( char *name, HWND hwnd );
HWND fjui_get_hwnd( char *name );


struct fjui_summary_info {
	char hostname[256];

};

HINSTANCE fjui_hinstance( void );
#endif

