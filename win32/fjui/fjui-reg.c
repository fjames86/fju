
#include "fjui.h"

static void reg_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "reg_tv" );
	SetWindowPos( h, HWND_TOP, 5, 5, width - 10, height - 10, 0 );

}



static LRESULT CALLBACK reg_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;

	switch(	msg ) {
	case WM_CREATE:
		/* create a treeview */
		h = CreateWindowA( WC_TREEVIEWA, NULL, WS_VISIBLE|WS_CHILD, 0, 0, 0, 0, hwnd, 0, 0, NULL );
		fjui_hwnd_register( "reg_tv", h );
		break;	
	case WM_COMMAND:
		break;
	case WM_SIZE:
		reg_size( hwnd, LOWORD(lparam), HIWORD(lparam) );
		break;
	case WM_NOTIFY:
		break;
	case WM_CTLCOLORSTATIC:
		SetBkMode((HDC)wparam, TRANSPARENT );
		return (BOOL)GetStockObject( NULL_PEN );
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}

void fjui_reg_setinfo( struct fjui_hostinfo *info ) {	
}

void fjui_reg_refresh( uint64_t hostid ) {
	//fjui_call_raftlist( hostid );
}

void fjui_reg_register( void ) {
	WNDCLASSEXW cls;

	/* register main class and create the window */
	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = reg_cb;
	cls.hInstance = fjui_hinstance();
	cls.lpszClassName = L"FJUIREG";
	cls.hbrBackground = GetSysColorBrush( COLOR_WINDOW );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	//cls.hIcon = winrpc_icon();
	//cls.hIconSm = winrpc_icon();

	RegisterClassExW( &cls );
}

