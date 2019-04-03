
#include <WinSock2.h>
#include <Windows.h>
#include <ws2tcpip.h>

#include "fjlogui.h"

static struct {
	int exiting;
	HWND hwnd[32];
#define HWND_MAIN 0

} glob;

static LRESULT CALLBACK main_wndproc( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam );

int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow ) {
	MSG msg;
	int sts;
	WNDCLASSEXW cls;
	HWND h;
	WSADATA wsadata;
	HANDLE evts[1];
	INITCOMMONCONTROLSEX icex;
	struct sockaddr_in sin;

	WSAStartup( MAKEWORD(2,2), &wsadata );

	icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
    icex.dwICC = ICC_TAB_CLASSES|ICC_BAR_CLASSES|ICC_STANDARD_CLASSES|ICC_PROGRESS_CLASS|ICC_UPDOWN_CLASS;
    InitCommonControlsEx( &icex );

	
	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = main_wndproc;
	cls.hInstance = hInstance;
	cls.lpszClassName = L"FJLOGUI";
	cls.hbrBackground = GetSysColorBrush( COLOR_3DFACE );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	cls.hIcon = fjlogui_icon();
	cls.hIconSm = fjlogui_icon();

	RegisterClassExW( &cls );

	h = CreateWindowExW( 0, L"FJLOGUI", L"fjlog", WS_VISIBLE|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX, 200, 200, 560, 420, NULL, 0, hInstance, NULL );
	UpdateWindow( h );
	ShowWindow( h, SW_SHOW );
	SetForegroundWindow( h );

	glob.hwnd[HWND_MAIN] = h;

	/* Message loop */
	do {
		sts = WaitMessage();
		//sts = MsgWaitForMultipleObjects( 0, evts, FALSE, 500, QS_ALLINPUT );
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
		} else {
			/* Timeout - do nothing */
		}

	} while( !glob.exiting );


	return 0;
}

void winrpc_set_font( HWND hwnd ) {
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


static void main_create( HWND hwnd ) {

}

static void main_command( HWND hwnd, int cmd ) {

}

static LRESULT CALLBACK main_wndproc( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	switch( msg ) {
	case WM_CREATE:
		main_create( hwnd );
		break;	
	case WM_DESTROY:
		PostQuitMessage( 0 );
		break;
	case WM_COMMAND:
		main_command( hwnd, LOWORD( wparam ) );
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}