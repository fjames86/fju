
#define _CRT_SECURE_NO_WARNINGS

#include <WinSock2.h>
#include <Windows.h>
#include <CommCtrl.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/hostreg.h>
#include <fju/hrauth.h>
#include <fju/freg.h>
#include <fju/log.h>
#include <fju/programs.h>
#include <fju/mmf.h>
#include <fju/sec.h>
#include <fju/cht.h>

#include "resource.h"
#include "fjui.h"

#pragma comment(linker,"\"/manifestdependency:type='win32' \
name='Microsoft.Windows.Common-Controls' version='6.0.0.0' \
processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")

#define CMD_EXIT 1
#define CMD_ABORT 2

static LRESULT CALLBACK fjui_main( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam );

struct fjui_hwnd {
	struct fjui_hwnd *next;
	HWND hwnd;
	char name[64];
};

static struct {
	int exiting;
	HINSTANCE hinstance;
	struct fjui_hwnd *hwnds;
	uint64_t hostid;
} glob;

void fjui_hwnd_register( char *name, HWND hwnd ) {
	struct fjui_hwnd *h;
	h = malloc( sizeof(*h) );
	h->hwnd = hwnd;
	strncpy( h->name, name, sizeof(h->name) - 1 );
	h->next = glob.hwnds;
	glob.hwnds = h;
}
HWND fjui_get_hwnd( char *name ) {
	struct fjui_hwnd *h;
	h = glob.hwnds;
	while( h ) {
		if( strcmp( h->name, name ) == 0 ) return h->hwnd;
		h = h->next;
	}
	return 0;
}
HINSTANCE fjui_hinstance( void ) {
	return glob.hinstance;
}
uint64_t fjui_hostid( void ) {
	return glob.hostid;
}

int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow ) {
	MSG msg;
	int sts, timeout;
	WNDCLASSEXW cls;
	HWND h;
	WSADATA wsadata;
	HANDLE evts[1];
	INITCOMMONCONTROLSEX icex;
	struct sockaddr_in sin;

	glob.hinstance = hInstance;

	WSAStartup( MAKEWORD(2,2), &wsadata );

	icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
    icex.dwICC = ICC_TAB_CLASSES|ICC_BAR_CLASSES|ICC_STANDARD_CLASSES|ICC_PROGRESS_CLASS|ICC_UPDOWN_CLASS;
    InitCommonControlsEx( &icex );

	/* open all libraries etc */
	rpcd_init();
	hostreg_open();
	hrauth_register();
	/* Q: do we want to load any other services here e.g. fvm ? */
	fjui_summary_register();

	/* register main class and create the window */
	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = fjui_main;
	cls.hInstance = hInstance;
	cls.lpszClassName = L"FJUI";
	cls.hbrBackground = GetSysColorBrush( COLOR_3DFACE );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	//cls.hIcon = winrpc_icon();
	//cls.hIconSm = winrpc_icon();

	RegisterClassExW( &cls );

	h = CreateWindowExW( 0, L"FJUI", L"fjui", WS_VISIBLE|WS_OVERLAPPEDWINDOW, 200, 200, 560, 420, NULL, 0, hInstance, NULL );
	fjui_hwnd_register( "main", h );

	UpdateWindow( h );
	ShowWindow( h, SW_SHOW );
	SetForegroundWindow( h );

	/* Message loop */
	evts[0] = rpcd_win32event();
	do {
		fjui_net_service();

		timeout = rpc_iterator_timeout( 500 );
		if( timeout > 1000 ) timeout = 500;
		if( timeout < 0 ) timeout = 0;

		sts = MsgWaitForMultipleObjects( 1, evts, FALSE, timeout, QS_ALLINPUT );
		if( sts == 1 ) {
			/* Normal window message */
			do {
				sts = PeekMessageW( &msg, NULL, 0, 0, 0 );
				if( !sts ) break;

				sts = GetMessageW( &msg, NULL, 0, 0 );
				if( sts <= 0 ) {
					glob.exiting = 1;
					break;
				}

				TranslateMessage( &msg );
				DispatchMessageW( &msg );	

			} while( sts );
	
		} else if( sts == 0 ) {
			/* service networking */
			fjui_net_service();
		} else {
			/* Timeout */
		}
						
		/* service interators */
		rpc_iterator_service();

		/* service waiters */
		rpc_waiter_service();

	} while( !glob.exiting );




	return 0;
}

static void fjui_main_create( HWND hwnd ) {
	HWND h, tabh;
	HMENU menu, m;
	TCITEMA tci;

	/* add menu bar */
	menu = CreateMenu();
	m = CreateMenu();
	AppendMenuA( m, MF_STRING, (UINT_PTR)CMD_EXIT, "E&xit" );
	AppendMenuA( menu, MF_POPUP,(UINT_PTR) m, "&File" );
	m = CreateMenu();
	AppendMenuA( m, MF_STRING, (UINT_PTR)0, "&Connect" );
	AppendMenuA( menu, MF_POPUP, (UINT_PTR)m, "&Edit" );
	m = CreateMenu();
	AppendMenuA( m, MF_STRING, (UINT_PTR)CMD_ABORT, "&About" );
	AppendMenuA( menu, MF_POPUP, (UINT_PTR)m, "&Help" );
	SetMenu( hwnd, menu );

	/* create a tab view and make it fill the window */
	tabh = CreateWindowExA( 0, WC_TABCONTROLA, NULL, WS_VISIBLE|WS_CHILD, 5, 5, 100, 100, hwnd, 0, 0, NULL );
	fjui_set_font( tabh );
	fjui_hwnd_register( "tabctrl", tabh );

	memset( &tci, 0, sizeof(tci) );
	tci.mask = TCIF_TEXT;
	tci.pszText = "Summary";
	TabCtrl_InsertItem( tabh, 0, &tci );
	h = CreateWindowA( "FJUISUMMARY", NULL, WS_VISIBLE|WS_CHILD|WS_BORDER, 0, 0, 0, 0, tabh, 0, 0, NULL );
	fjui_hwnd_register( "summary", h );

	memset( &tci, 0, sizeof(tci) );
	tci.mask = TCIF_TEXT;
	tci.pszText = "Log";
	TabCtrl_InsertItem( tabh, 1, &tci );

	memset( &tci, 0, sizeof(tci) );
	tci.mask = TCIF_TEXT;
	tci.pszText = "FVM";
	TabCtrl_InsertItem( tabh, 2, &tci );

	memset( &tci, 0, sizeof(tci) );
	tci.mask = TCIF_TEXT;
	tci.pszText = "Freg";
	TabCtrl_InsertItem( tabh, 3, &tci );

	h = CreateWindowA( STATUSCLASSNAMEA, NULL, WS_VISIBLE|WS_CHILD|SBARS_SIZEGRIP, 0, 0, 0, 0, hwnd, 0, 0, NULL );
	fjui_set_font( h );
	fjui_hwnd_register( "statusbar", h );
	{
		struct hostreg_prop prop;
		struct hostreg_host host;
		int sts;
		int parts[2];

		parts[0] = 0;
		parts[1] = 50;
		SendMessageA( h, SB_SETPARTS, 2, parts );

		hostreg_prop( &prop );
		sts = hostreg_host_by_id( prop.localid, &host );
		fjui_set_statusbar( 0, host.name );
		fjui_set_statusbar( 1, " Disconnected" );
	}

	h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_LISTBOXA, NULL, WS_VISIBLE|WS_CHILD|LBS_NOTIFY, 0, 0, 0, 0, hwnd, 0, 0, NULL );
	fjui_set_font( h );
	fjui_hwnd_register( "hostlb", h );
	{
		struct hostreg_host *hlist;
		int i, n;

		hlist = malloc( sizeof(*hlist) * 32 );
		n = hostreg_host_list( hlist, 32 );
		for( i = 0; i < n; i++ ) {
			SendMessageA( h, LB_ADDSTRING, 0, hlist[i].name );
		}
		free( hlist );
	}

}

void fjui_set_statusbar( int index, char *fmt, ... ) {
	char str[1024];
	va_list args;
	va_start( args, fmt );
	vsprintf( str, fmt, args );
	va_end( args );
	SendMessageA( fjui_get_hwnd( "statusbar" ), SB_SETTEXTA, index|SBT_NOBORDERS, str );
}


static void fjui_main_command( HWND hwnd, int cmd, HWND hcmd ) {
	switch( cmd ) {
	case CMD_EXIT:
		/* menu exit */
		DestroyWindow( hwnd );
		break;
	case CMD_ABORT:
		/* about */
		MessageBoxA( hwnd, 
				"FJU management interface.\n"
				"\n"
				"Copyright (C) Frank James, 2020.",
				"About", 
				MB_OK|MB_ICONINFORMATION );
		break;
	default:
		if( hcmd == fjui_get_hwnd( "hostlb" ) ) {
			int idx;
			char namestr[256];

			/* inform all tabs to update their info to the currently selected host */
			idx = SendMessageA( hcmd, LB_GETCURSEL, 0, 0 );
			SendMessageA( hcmd, LB_GETTEXT, idx, namestr );
			glob.hostid = hostreg_hostid_by_name( namestr );
			fjui_set_statusbar( 0, "%s", namestr );
			fjui_call_getlicinfo( glob.hostid );
		}
		break;
	}

}

static void fjui_main_size( HWND hwnd, int width, int height ) {
	HWND hw, h;
	int parts[2];

	h = fjui_get_hwnd( "statusbar" );
	SetWindowPos( h, HWND_TOP, 0, h - 10, width, 10, 0 );
	parts[0] = width / 2;
	parts[1] = -1;
	SendMessageA( h, SB_SETPARTS, 2, parts );

	h = fjui_get_hwnd( "tabctrl" );
	SetWindowPos( h, HWND_TOP, 105, 5, width - 110, height - 30, 0 );

	h = fjui_get_hwnd( "summary" );
	SetWindowPos( h, HWND_TOP, 10, 30, width - 130, height - 70, 0 );

	h = fjui_get_hwnd( "hostlb" );
	SetWindowPos( h, HWND_TOP, 5, 5, 95, height - 25, 0 );
}

static void fjui_main_notify( HWND hwnd, NMHDR *nmhdr ) {
	HWND h;

	switch( nmhdr->code ) {
	case TCN_SELCHANGE:
		{
			int page = TabCtrl_GetCurSel( fjui_get_hwnd( "tabctrl" ) );
			h = fjui_get_hwnd( "summary" );
			ShowWindow( h, page == 0 ? SW_SHOW : SW_HIDE );
		}
		break;
	}
}

static LRESULT CALLBACK fjui_main( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;

	switch( msg ) {
	case WM_CREATE:
		fjui_main_create( hwnd );
		break;	
	case WM_DESTROY:
		PostQuitMessage( 0 );
		break;
	case WM_COMMAND:
		fjui_main_command( hwnd, LOWORD( wparam ), (HWND)lparam );
		break;
	case WM_SIZE:
		fjui_main_size( hwnd, LOWORD(lparam), HIWORD(lparam) );
		break;
	case WM_NOTIFY:
		fjui_main_notify( hwnd, (NMHDR *)lparam );
		break;
#if 0
	case WM_CTLCOLORSTATIC:
		h = (HWND)lparam;
		if( h == fjui_get_hwnd( "summary" ) ) {
			SetBkMode( h, TRANSPARENT );
            return (BOOL) GetStockObject(LTGRAY_BRUSH);
		}
		break;
#endif
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}

void fjui_set_font( HWND hwnd ) {
	static HFONT hf;

	NONCLIENTMETRICSA ncm;

	if( !hf ) {
		memset( &ncm, 0, sizeof(ncm) );
		ncm.cbSize = sizeof(ncm);
		SystemParametersInfoA( SPI_GETNONCLIENTMETRICS, sizeof(ncm), &ncm, 0 );
		hf = CreateFontIndirectA( &ncm.lfMessageFont );
	}

	SendMessageA( hwnd, WM_SETFONT, (WPARAM)hf, 0 );
}