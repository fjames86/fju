
#define _CRT_SECURE_NO_WARNINGS

#include <WinSock2.h>
#include <Windows.h>
#include <CommCtrl.h>

#include "fjui.h"

/*
 * This implements a summary tab view to show things like registered rpc services,
 * licensing info, fvm modules loaded etc.
 */


static LRESULT CALLBACK fjui_summary_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;

	switch( msg ) {
	case WM_CREATE:
		h = CreateWindowExA( WS_EX_TRANSPARENT, WC_STATICA, "Summary", WS_VISIBLE|WS_CHILD, 5, 5, 52, 15, hwnd, 0, 0, 0 );
		fjui_set_font( h );
		fjui_hwnd_register( "summary_lbl", h );



		break;	
	case WM_COMMAND:
		//fjui_main_command( hwnd, LOWORD( wparam ) );
		break;
	case WM_SIZE:
		//fjui_main_size( hwnd, LOWORD(lparam), HIWORD(lparam) );
		break;
	case WM_CTLCOLORSTATIC:
		h = (HWND)lparam;
		if( h == fjui_get_hwnd( "summary_lbl" ) ) {
			//SetBkMode( (HDC)wparam, COLOR_BACKGROUND );
			//SetBkMode( (HDC)wparam, TRANSPARENT );
			//SetBkColor( (HDC)wparam, TRANSPARENT );
            return (BOOL) GetStockObject(COLOR_BACKGROUND);
		}
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}

static void summary_iter_cb( struct rpc_iterator *iter ) {
	fjui_call_getlicinfo( fjui_hostid() );
}

static struct rpc_iterator summary_iter = 
{
	NULL,
	0,
	30000,
	summary_iter_cb,
	NULL
};

void fjui_summary_register( void ) {
	WNDCLASSEXW cls;

	/* register main class and create the window */
	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = fjui_summary_cb;
	cls.hInstance = fjui_hinstance();
	cls.lpszClassName = L"FJUISUMMARY";
	cls.hbrBackground = GetSysColorBrush( COLOR_WINDOW );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	//cls.hIcon = winrpc_icon();
	//cls.hIconSm = winrpc_icon();

	RegisterClassExW( &cls );
}
