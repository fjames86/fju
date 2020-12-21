
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

#pragma comment(linker,"\"/manifestdependency:type='win32' \
name='Microsoft.Windows.Common-Controls' version='6.0.0.0' \
processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")

static LRESULT CALLBACK fjui_main( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam );

static struct {
	int exiting;
} glob;

int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow ) {
	MSG msg;
	int sts, timeout;
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

	/* open all libraries etc */
	rpcd_init();
	hostreg_open();
	hrauth_register();

	
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

	h = CreateWindowExW( 0, L"FJUI", L"fjui", WS_VISIBLE|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX, 200, 200, 560, 420, NULL, 0, hInstance, NULL );
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


static LRESULT CALLBACK fjui_main( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	switch( msg ) {
	case WM_CREATE:
		//fjui_main_create( hwnd );
		break;	
	case WM_DESTROY:
		PostQuitMessage( 0 );
		break;
	case WM_COMMAND:
		//fjui_main_command( hwnd, LOWORD( wparam ) );
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}

