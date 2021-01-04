
#include "fjui.h"

static LRESULT CALLBACK reg_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	switch(	msg ) {
	case WM_CREATE:
		/* create a treeview */
		break;	
	case WM_COMMAND:
		break;
	case WM_SIZE:
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

