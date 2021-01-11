
#include "fjui.h"

#define CMD_ENCODE 1
#define CMD_DECODE 2

static void xdr_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "xdr_args" );
	SetWindowPos( h, HWND_TOP, 60, 5, width - 225, 25, 0 );
	h = fjui_get_hwnd( "xdr_b64" );
	SetWindowPos( h, HWND_TOP, 60, 35, width - 65, 25, 0 );
	h = fjui_get_hwnd( "xdr_encode" );
	SetWindowPos( h, HWND_TOP, width - 160, 5, 75, 25, 0 );
	h = fjui_get_hwnd( "xdr_decode" );
	SetWindowPos( h, HWND_TOP, width - 80, 5, 75, 25, 0 );

}

static void xdr_command( HWND hwnd, int id, int cmd ) {
	switch( id ) {
	case CMD_ENCODE:
	break;
	case CMD_DECODE:
	break;
	}
}

static LRESULT CALLBACK xdr_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;
	LVCOLUMNA lvc;

	switch( msg ) {
	case WM_CREATE:
	  h = CreateWindowA( WC_STATICA, "Args:", WS_VISIBLE|WS_CHILD, 5, 5, 50, 25, hwnd, 0, NULL, 0 );
	  fjui_set_font( h );
	  h = CreateWindowA( WC_STATICA, "Base64:", WS_VISIBLE|WS_CHILD, 5, 35, 50, 25, hwnd, 0, NULL, 0 );
	  fjui_set_font( h );
	  
	  h = CreateWindowA( WC_EDITA, "args", WS_VISIBLE|WS_CHILD|WS_BORDER|ES_AUTOHSCROLL, 0,0,0,0, hwnd, 0, NULL, 0 );
	  fjui_set_font( h );
	  fjui_hwnd_register( "xdr_args", h );
	  
	  h = CreateWindowA( WC_EDITA, "base64", WS_VISIBLE|WS_CHILD|WS_BORDER|ES_AUTOHSCROLL, 0,0,0,0, hwnd, 0, NULL, 0 );
	  fjui_set_font( h );
	  fjui_hwnd_register( "xdr_b64", h );

	  h = CreateWindowA( WC_BUTTON, "Encode", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON, 0, 0,0,0, hwnd, CMD_ENCODE, NULL, 0 );
	  fjui_set_font( h );
	  fjui_hwnd_register( "xdr_encode", h );

	  h = CreateWindowA( WC_BUTTON, "Decode", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON, 0, 0,0,0, hwnd, CMD_DECODE, NULL, 0 );
	  fjui_set_font( h );
	  fjui_hwnd_register( "xdr_decode", h );
		break;	
	case WM_COMMAND:
		xdr_command( hwnd, LOWORD( wparam ), HIWORD( wparam ) );
		break;
	case WM_SIZE:
		xdr_size(hwnd, LOWORD(lparam), HIWORD(lparam) );	  
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

void fjui_xdr_register( void ) {
	WNDCLASSEXW cls;

	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = xdr_cb;
	cls.hInstance = fjui_hinstance();
	cls.lpszClassName = L"FJUIXDR";
	cls.hbrBackground = GetSysColorBrush( COLOR_3DFACE );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );

	RegisterClassExW( &cls );  
}
