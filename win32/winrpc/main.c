
#include <WinSock2.h>
#include <Windows.h>
#include <ws2tcpip.h>

#include "winrpc.h"
#include <fju/rpc.h>

static struct {
	int exiting;
	HWND hwnds[16];
#define WINRPC_MAIN   0
#define WINRPC_REPORT 1
#define WINRPC_IP 2
#define WINRPC_PORT 3

	HANDLE evt;
	SOCKET fd;

	char buf[32*1024];
} winrpc;
static LRESULT CALLBACK winrpc_main_wndproc( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam );
static void winrpc_service( void );

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
	cls.lpfnWndProc = winrpc_main_wndproc;
	cls.hInstance = hInstance;
	cls.lpszClassName = L"WINRPC";
	cls.hbrBackground = GetSysColorBrush( COLOR_3DFACE );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	cls.hIcon = winrpc_icon();
	cls.hIconSm = winrpc_icon();

	RegisterClassExW( &cls );

	h = CreateWindowExW( 0, L"WINRPC", L"WinRPC", WS_VISIBLE|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX, 200, 200, 560, 420, NULL, 0, hInstance, NULL );
	UpdateWindow( h );
	ShowWindow( h, SW_SHOW );
	SetForegroundWindow( h );

	winrpc.hwnds[WINRPC_MAIN] = h;
	winrpc.evt = WSACreateEvent();
	winrpc.fd = socket( AF_INET, SOCK_DGRAM, 0 );

	memset( &sin, 0, sizeof(sin) );
	sin.sin_family = AF_INET;	
	bind( winrpc.fd, (struct sockaddr *)&sin, sizeof(sin) );
	sts = 1;
	setsockopt( winrpc.fd, SOL_SOCKET, SO_BROADCAST, (char *)&sts, sizeof(sts) );

	SendMessageA( winrpc.hwnds[WINRPC_IP], IPM_SETADDRESS, 0, ntohl( INADDR_BROADCAST ) );


	WSAEventSelect( winrpc.fd, winrpc.evt, FD_READ );

	/* Message loop */
	evts[0] = winrpc.evt;
	do {
		sts = MsgWaitForMultipleObjects( 1, evts, FALSE, 500, QS_ALLINPUT );
		if( sts == 1 ) {
			/* Normal window message */
			do {
				sts = PeekMessageW( &msg, NULL, 0, 0, 0 );
				if( !sts ) break;

				sts = GetMessageW( &msg, NULL, 0, 0 );
				if( sts <= 0 ) {
					winrpc.exiting = 1;
					break;
				}

				TranslateMessage( &msg );
				DispatchMessageW( &msg );	

			} while( sts );
	
		} else if( sts == 0 ) {
			/* service networking */
			winrpc_service();
		} else {
			/* Timeout - do nothing */
		}

	} while( !winrpc.exiting );


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

static void winrpc_main_create( HWND hwnd ) {
	HWND h;
	LVCOLUMNW lvc;
	HMENU menu, m;

	/* add menu bar */
	menu = CreateMenu();
	m = CreateMenu();
	AppendMenuA( m, MF_STRING, (UINT_PTR)2, "E&xit" );
	AppendMenuA( menu, MF_POPUP,(UINT_PTR) m, "&File" );
	m = CreateMenu();
	AppendMenuA( m, MF_STRING, (UINT_PTR)3, "&About" );
	AppendMenuA( menu, MF_POPUP, (UINT_PTR)m, "&Help" );
	SetMenu( hwnd, menu );
	
	h = CreateWindowW( WC_BUTTONW, L"Discover", WS_VISIBLE|WS_CHILD, 25, 25, 100, 23, hwnd, (HMENU)1, NULL, NULL );
	winrpc_set_font( h );

	h = CreateWindowW( WC_IPADDRESSW, NULL, WS_VISIBLE|WS_CHILD, 130, 25, 120, 23, hwnd, NULL, NULL, NULL );
	winrpc_set_font( h );
	winrpc.hwnds[WINRPC_IP] = h;

	h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_EDITA, "111", WS_VISIBLE|WS_CHILD|ES_NUMBER, 260, 25, 45, 23, hwnd, NULL, NULL, NULL );
	winrpc_set_font( h );
	winrpc.hwnds[WINRPC_PORT] = h;

	winrpc.hwnds[WINRPC_REPORT] = CreateWindowExA( WS_EX_CLIENTEDGE, WC_LISTVIEWA, "", WS_VISIBLE|WS_CHILD|LVS_SINGLESEL|LVS_REPORT|LVS_SHOWSELALWAYS|LVS_NOSORTHEADER,
		25, 60, 500, 280, hwnd, NULL, NULL, NULL );
	ListView_SetExtendedListViewStyle( winrpc.hwnds[WINRPC_REPORT], LVS_EX_FULLROWSELECT );

	memset( &lvc, 0, sizeof(lvc) );
	lvc.pszText = L"Host";
	lvc.cchTextMax = (int)wcslen( lvc.pszText );
	lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
	lvc.cx = 120;
	lvc.iSubItem = 0;
	ListView_InsertColumn( winrpc.hwnds[WINRPC_REPORT], 0, &lvc );

	lvc.pszText = L"Program";
	lvc.cchTextMax = (int)wcslen( lvc.pszText );
	lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
	lvc.cx = 80;
	lvc.iSubItem = 1;
	ListView_InsertColumn( winrpc.hwnds[WINRPC_REPORT], 1, &lvc );

	lvc.pszText = L"Version";
	lvc.cchTextMax = (int)wcslen( lvc.pszText );
	lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
	lvc.cx = 80;
	lvc.iSubItem = 2;
	ListView_InsertColumn( winrpc.hwnds[WINRPC_REPORT], 2, &lvc );

	lvc.pszText = L"Protocol";
	lvc.cchTextMax = (int)wcslen( lvc.pszText );
	lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
	lvc.cx = 80;
	lvc.iSubItem = 3;
	ListView_InsertColumn( winrpc.hwnds[WINRPC_REPORT], 3, &lvc );

	lvc.pszText = L"Port";
	lvc.cchTextMax = (int)wcslen( lvc.pszText );
	lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
	lvc.cx = 80;
	lvc.iSubItem = 4;
	ListView_InsertColumn( winrpc.hwnds[WINRPC_REPORT], 4, &lvc );


}

static void winrpc_send_discover( void ) {
	struct rpc_inc inc;
	struct sockaddr_in sin;
	int sts;
	char ipstr[64];

	memset( &inc, 0, sizeof(inc) );
	xdr_init( &inc.xdr, winrpc.buf, sizeof(winrpc.buf) );
	rpc_init_call( &inc, 100000, 2, 4, NULL );

	memset( &sin, 0, sizeof(sin) );
	sin.sin_family = AF_INET;

	GetWindowTextA( winrpc.hwnds[WINRPC_PORT], ipstr, sizeof(ipstr) - 1 );
	sin.sin_port = htons( (u_short)strtoul( ipstr, NULL, 10 ) );

	/* get configured address */
	SendMessageA( winrpc.hwnds[WINRPC_IP], IPM_GETADDRESS, 0, (LPARAM)&sts );
	sin.sin_addr.s_addr = htonl( sts );

	sts = sendto( winrpc.fd, inc.xdr.buf, inc.xdr.offset, 0, (struct sockaddr *)&sin, sizeof(sin) );
	if( sts < 0 ) {
		sts = WSAGetLastError();
		sts++;
	}
}

static void winrpc_main_command( HWND hwnd, int cmd ) {
	switch( cmd ) {
	case 1:
		/* discover button */

		/* clear report */
		ListView_DeleteAllItems( winrpc.hwnds[WINRPC_REPORT] );

		/* send discover call */
		winrpc_send_discover();

		break;
	case 2:
		/* menu exit */
		DestroyWindow( hwnd );
		break;
	case 3:
		/* about */
		MessageBoxA( hwnd, 
				"Win32 ONC/RPC discovery utility.\n"
				"\n"
				"Copyright (C) Frank James, 2018.",
				"About", 
				MB_OK|MB_ICONINFORMATION );
		break;
	}

}

static LRESULT CALLBACK winrpc_main_wndproc( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	switch( msg ) {
	case WM_CREATE:
		winrpc_main_create( hwnd );
		break;	
	case WM_DESTROY:
		PostQuitMessage( 0 );
		break;
	case WM_COMMAND:
		winrpc_main_command( hwnd, LOWORD( wparam ) );
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}

static void winrpc_service( void ) {
	WSANETWORKEVENTS events;
	struct sockaddr_in sin;
	int len, sts;
	struct rpc_inc inc;
	int b;
	uint32_t prog, vers, prot, port;
	LVITEMW lvi;
	wchar_t wstr[256];
	int idx;

	/* service networking */
	WSAEnumNetworkEvents( winrpc.fd, winrpc.evt, &events );
	if( events.lNetworkEvents & FD_READ ) {
		len = sizeof(sin);
		sts = recvfrom( winrpc.fd, winrpc.buf, sizeof(winrpc.buf), 0, (struct sockaddr *)&sin, &len );
		if( sts < 0 ) return;

		memset( &inc, 0, sizeof(inc) );
		xdr_init( &inc.xdr, winrpc.buf, sts );
		sts = rpc_decode_msg( &inc.xdr, &inc.msg );		
		sts = rpc_process_reply( &inc );
		if( sts ) return;

		/* decode results */
		sts = xdr_decode_boolean( &inc.xdr, &b );
		if( sts ) return;
		while( b ) {
			sts = xdr_decode_uint32( &inc.xdr, &prog );
			if( !sts ) sts = xdr_decode_uint32( &inc.xdr, &vers );
			if( !sts ) sts = xdr_decode_uint32( &inc.xdr, &prot );
			if( !sts ) sts = xdr_decode_uint32( &inc.xdr, &port );
			if( sts ) return;

			/* add to report */
			memset( &lvi, 0, sizeof(lvi) );
			lvi.mask = LVIF_TEXT|LVIF_PARAM|LVIF_PARAM;
			lvi.iItem = 0;
			lvi.iSubItem = 0;
			InetNtopW( AF_INET, &sin.sin_addr, wstr, 255 );
			swprintf( wstr + wcslen( wstr ), 255 - wcslen( wstr ), L":%d", ntohs( sin.sin_port ) );
			lvi.pszText = wstr;			
			idx = (int)SendMessageA( winrpc.hwnds[WINRPC_REPORT], LVM_INSERTITEMW, 0, (LPARAM)(const LV_ITEMW *)(&lvi) );

			lvi.iItem = idx;
			lvi.mask = LVIF_TEXT;
			lvi.iSubItem = 1;
			swprintf( wstr, 255, L"%d", prog );
			lvi.pszText = wstr;			
			SendMessageA( winrpc.hwnds[WINRPC_REPORT], LVM_SETITEMW, 0, (LPARAM)(const LV_ITEMW *)(&lvi) );

			lvi.iItem = idx;
			lvi.mask = LVIF_TEXT;
			lvi.iSubItem = 2;
			swprintf( wstr, 255, L"%d", vers );
			lvi.pszText = wstr;			
			SendMessageA( winrpc.hwnds[WINRPC_REPORT], LVM_SETITEMW, 0, (LPARAM)(const LV_ITEMW *)(&lvi) );

			lvi.iItem = idx;
			lvi.mask = LVIF_TEXT;
			lvi.iSubItem = 3;
			swprintf( wstr, 255, L"%d (%s)", prot, prot == IPPROTO_UDP ? L"UDP" : prot == IPPROTO_TCP ? L"TCP" : L"Other" );
			lvi.pszText = wstr;			
			SendMessageA( winrpc.hwnds[WINRPC_REPORT], LVM_SETITEMW, 0, (LPARAM)(const LV_ITEMW *)(&lvi) );

			lvi.iItem = idx;
			lvi.mask = LVIF_TEXT;
			lvi.iSubItem = 4;
			swprintf( wstr, 255, L"%d", port );
			lvi.pszText = wstr;			
			SendMessageA( winrpc.hwnds[WINRPC_REPORT], LVM_SETITEMW, 0, (LPARAM)(const LV_ITEMW *)(&lvi) );

			sts = xdr_decode_boolean( &inc.xdr, &b );
			if( sts ) return;
		}

	}
}

