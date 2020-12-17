
#define _CRT_SECURE_NO_WARNINGS 

#include <WinSock2.h>
#include <Windows.h>
#include <ws2tcpip.h>
#include <time.h>

#include <fju/rpc.h>
#include <fju/log.h>
#include <fju/hostreg.h>
#include <fju/hrauth.h>
#include <fju/rpcd.h>


#include "fjlogui.h"

static void load_log( char *path, uint64_t id );

static struct {
	int exiting;
	HWND hwnd[32];
#define HWND_MAIN 0
#define HWND_ELIST 1
	uint64_t seq;
	uint64_t lastid;
	UINT htimer;
	int logopen;
	struct log_s log;
	SOCKET fd;
	char buf[32*1024];
	uint64_t hostid;
	uint64_t hshare;
	int port;
	HANDLE evts[1];
} glob;

static LRESULT CALLBACK main_wndproc( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam );
static void main_service_networking( void );
static void add_log_entry( struct log_entry *entry );
static void nls_call_read( uint64_t hostid, uint64_t hshare, uint64_t seq, uint64_t lastid );

int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow ) {
	MSG msg;
	int sts;
	WNDCLASSEXW cls;
	HWND h;
	WSADATA wsadata;
	INITCOMMONCONTROLSEX icex;
	struct sockaddr_in sin;

	WSAStartup( MAKEWORD(2,2), &wsadata );
	hostreg_open();

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

	h = CreateWindowExW( 0, L"FJLOGUI", L"fjlog", WS_VISIBLE|WS_OVERLAPPEDWINDOW, 200, 200, 560, 420, NULL, 0, hInstance, NULL );
	UpdateWindow( h );
	ShowWindow( h, SW_SHOW );
	SetForegroundWindow( h );

	glob.hwnd[HWND_MAIN] = h;

	glob.fd = socket( AF_INET,SOCK_DGRAM,0 );

	memset( &sin,0,sizeof( sin ) );
	sin.sin_family = AF_INET;
	bind( glob.fd, (struct sockaddr *)&sin, sizeof(sin) );
	glob.evts[0] = WSACreateEvent();
	WSAEventSelect( glob.fd, glob.evts[0], FD_READ );

	/* Message loop */
	do {
		WSAEventSelect( glob.fd, glob.evts[0], FD_READ );

		//sts = WaitMessage();
		sts = MsgWaitForMultipleObjects( 1, glob.evts, FALSE, 500, QS_ALLINPUT );
		if( sts == (WAIT_OBJECT_0 + 1) ) {
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
		} else if( sts == WAIT_OBJECT_0 ) {
			/* service networking */
			main_service_networking();
		} else {
			/* Timeout - do nothing */
			rpc_waiter_service();
		}

	} while( !glob.exiting );

	WSACloseEvent( glob.evts[0] );

	return 0;
}

static void main_service_networking( void ) {
	int sts,len;
	struct sockaddr_in sin;
	struct rpc_inc inc;
	WSANETWORKEVENTS events;

	WSAEnumNetworkEvents( glob.fd, glob.evts[0], &events );

	if( events.lNetworkEvents & FD_READ ) {
		len = sizeof( sin );
		sts = recvfrom( glob.fd, glob.buf, sizeof(glob.buf), 0, (struct sockaddr *)&sin, &len );
		if( sts < 0 ) {
			return;
		}

		memset( &inc, 0, sizeof(inc) );
		xdr_init( &inc.xdr,glob.buf,sts );
		sts = rpc_process_incoming( &inc );
	}

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
	HMENU menu, m;
	HWND h;
	LVCOLUMNW lvc;

	menu = CreateMenu();
	m = CreateMenu();
	AppendMenuA( m, MF_STRING, CMD_OPEN, "Open" );
	AppendMenuA( m, MF_STRING|MF_DISABLED, CMD_CLOSE, "Close" );
	AppendMenuA( m, MF_STRING, CMD_CLEAR, "Clear" );
	AppendMenuA( m, MF_SEPARATOR, 0, 0 );
	AppendMenuA( m, MF_STRING, CMD_QUIT, "Quit" );
	AppendMenuA( menu, MF_POPUP, m, "File" );

	m = CreateMenu();
	AppendMenuA( m, MF_STRING, CMD_ABOUT, "About" );
	AppendMenuA( menu, MF_POPUP, m, "Help" );

	SetMenu( hwnd, menu );

	/* create entry list */
	h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_LISTVIEWA, NULL, WS_VISIBLE|WS_CHILD|LVS_SINGLESEL|LVS_REPORT|LVS_SHOWSELALWAYS|LVS_NOSORTHEADER, 5, 5, 400, 400, hwnd, 0, NULL, 0 );
	glob.hwnd[HWND_ELIST] = h;
	ListView_SetExtendedListViewStyle( h, LVS_EX_FULLROWSELECT );

	/* add columns */
	memset( &lvc, 0, sizeof(lvc) );
	lvc.pszText = "Time";
	lvc.cchTextMax = (int)wcslen( lvc.pszText );
	lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
	lvc.cx = 120;
	lvc.iSubItem = 0;
	ListView_InsertColumn( h, 0, &lvc );

	lvc.pszText = "PID";
	lvc.cchTextMax = (int)wcslen( lvc.pszText );
	lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
	lvc.cx = 80;
	lvc.iSubItem = 1;
	ListView_InsertColumn(h, 1, &lvc );

	lvc.pszText = "Level";
	lvc.cchTextMax = (int)wcslen( lvc.pszText );
	lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
	lvc.cx = 80;
	lvc.iSubItem = 2;
	ListView_InsertColumn(h, 2, &lvc );

	lvc.pszText = "Message";
	lvc.cchTextMax = (int)wcslen( lvc.pszText );
	lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
	lvc.cx = 200;
	lvc.iSubItem = 3;
	ListView_InsertColumn(h, 3, &lvc );

	SetTimer( hwnd, &glob.htimer, 1000, 0 );

	DragAcceptFiles( hwnd, TRUE );

	load_log( mmf_default_path( "fju.log", NULL ), 0 );
}

static void add_log_entry( struct log_entry *entry ) {
	LVITEMA lvi;
	time_t now;
	struct tm *tm;
	char str[1024];
	int idx, count;

	count = SendMessageA( glob.hwnd[HWND_ELIST], LVM_GETITEMCOUNT, 0, 0 );
	if( count >= 32*1024 ) {
		SendMessageA( glob.hwnd[HWND_ELIST], LVM_DELETEITEM, 0, 0 );
	}

	memset( &lvi, 0, sizeof(lvi) );
	lvi.mask = LVIF_TEXT|LVIF_PARAM|LVIF_PARAM;
	lvi.iItem = 0x7ffffffe;
	lvi.iSubItem = 0;
	sec_timestr( entry->timestamp, str );
	lvi.pszText = str;			
	idx = (int)SendMessageA( glob.hwnd[HWND_ELIST], LVM_INSERTITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

	lvi.iItem = idx;
	lvi.mask = LVIF_TEXT;
	lvi.iSubItem = 1;
	sprintf( str, "%u", entry->pid );
	lvi.pszText = str;			
	SendMessageA( glob.hwnd[HWND_ELIST], LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

	lvi.iItem = idx;
	lvi.mask = LVIF_TEXT;
	lvi.iSubItem = 2;
	switch( entry->flags & LOG_LVL_MASK ) {
	case LOG_LVL_DEBUG: 
		strcpy( str, "DEBUG" );
		break;
	case LOG_LVL_ERROR: 
		strcpy( str, "ERROR" );
		break;
	case LOG_LVL_FATAL: 
		strcpy( str, "FATAL" );
		break;
	case LOG_LVL_INFO: 
		strcpy( str, "INFO" );
		break;
	case LOG_LVL_TRACE: 
		strcpy( str, "TRACE" );
		break;
	case LOG_LVL_WARN: 
		strcpy( str, "WARN" );
		break;
	default:
		strcpy( str, "UNKNOWN" );
		break;
	}
	lvi.pszText = str;			
	SendMessageA( glob.hwnd[HWND_ELIST], LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

	lvi.iItem = idx;
	lvi.mask = LVIF_TEXT;
	lvi.iSubItem = 3;
	if(entry->flags & LOG_BINARY) sprintf( str,"binary count=%d",entry->iov[0].len );
	lvi.pszText = entry->flags & LOG_BINARY ? str : entry->iov[0].buf;			
	SendMessageA( glob.hwnd[HWND_ELIST], LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );
}

static void load_log( char *path, uint64_t id ) {
	struct log_prop prop;
	struct log_entry entry;
	struct log_iov iov[1];
	int sts, n, ne;
	char *buf;

	/* clear any existing entries */
	ListView_DeleteAllItems( glob.hwnd[HWND_ELIST] );

	/* set log properties data */
	if( id == 0 ) {
		if( glob.logopen ) {
			log_close( &glob.log );
			glob.logopen = 0;
		}

		sts = log_open( path, NULL, &glob.log );
		if( sts ) return;
		glob.logopen = 1;	

		/* enable the close menu item */
		EnableMenuItem( GetSubMenu( GetMenu( glob.hwnd[HWND_MAIN] ), 0 ), CMD_CLOSE, MF_BYCOMMAND|MF_ENABLED );

	} else if( !glob.logopen ) return;

	SetWindowText( glob.hwnd[HWND_MAIN], path );

	log_prop( &glob.log, &prop );
	glob.seq = prop.seq;

	/* read entries until no more */
	buf = malloc( 32*1024 );
	do {
		memset( &entry, 0, sizeof(entry) );
		iov[0].buf = buf;
		iov[0].len = 32*1024;
		entry.iov = iov;
		entry.niov = 1;
		sts = log_read( &glob.log, id, &entry, 1, &ne );
		if( sts || !ne ) break;

		add_log_entry( &entry );

		id = entry.id;
	} while( 1 );
	glob.lastid = id;

	free( buf );
}

static void main_command( HWND hwnd, int cmd ) {
	int sts;

	switch( cmd ) {
	case CMD_QUIT:
		DestroyWindow( hwnd );
		break;
	case CMD_OPEN:
	{
		OPENFILENAME ofn;
		char path[512];
		BOOL b;
		DWORD sts;

		memset( &ofn, 0, sizeof(ofn) );		
		ofn.lStructSize = sizeof(ofn);
		ofn.hwndOwner = hwnd;
		ofn.lpstrFilter = "Log files\0*.log\0\0";
		ofn.nFilterIndex = 1;
		strcpy( path, "log.log" );
		ofn.lpstrFile = path;
		ofn.nMaxFile = sizeof(path);
		ofn.lpstrTitle = "Open Log File";
		ofn.Flags = OFN_FILEMUSTEXIST;
		b = GetOpenFileName( &ofn );
		if( b ) {
			/* load file */
			load_log( path, 0 );
		}
	}
		break;
	case CMD_ABOUT:
		MessageBoxA( hwnd, "fjlog gui", "About", MB_OK | MB_ICONINFORMATION );
		break;
	case CMD_CLOSE:
		if( glob.logopen ) {
			ListView_DeleteAllItems( glob.hwnd[HWND_ELIST] );
			SetWindowTextA( hwnd, "fjlog" );
			log_close( &glob.log );
			glob.logopen = 0;
			EnableMenuItem( GetSubMenu( GetMenu( glob.hwnd[HWND_MAIN] ), 0 ), CMD_CLOSE, MF_BYCOMMAND|MF_DISABLED );		
		}
		break;
	case CMD_CLEAR:
	SendMessageA( glob.hwnd[HWND_ELIST], LVM_DELETEALLITEMS, 0, 0 );
		break;
	}
}

static void main_dropfiles( HWND hwnd, HDROP hdrop ) {
	int nfiles;
	char path[256];

	nfiles = DragQueryFileA( hdrop, -1, NULL, 0 );
	if( nfiles > 0 ) {
		DragQueryFileA( hdrop, 0, path, sizeof(path) );

		/* load log entries for this file */
		load_log( path, 0 );
	}
	DragFinish( hdrop );

}

static void main_resize( HWND hwnd, int type, int w, int h ) {

	(void)(type);

	SetWindowPos( glob.hwnd[HWND_ELIST], HWND_TOP, 5, 5, w - 10, h - 10, 0 );
}

static void main_timer( HWND hwnd ) {
	int sts, ne;
	struct log_prop prop;
	uint64_t id;
	struct log_entry entry;
	struct log_iov iov[1];
	char *buf;

	/* check seq */
	if( !glob.logopen ) return;

	sts = log_prop( &glob.log, &prop );
	if( sts ) return;

	if( prop.seq == glob.seq ) return;
	glob.seq = prop.seq;

	/* read entries until no more */
	id = glob.lastid;
	buf = malloc( 32*1024 );
	do {
		memset( &entry, 0, sizeof(entry) );
		iov[0].buf = buf;
		iov[0].len = 32*1024;
		entry.iov = iov;
		entry.niov = 1;
		sts = log_read( &glob.log, id, &entry, 1, &ne );
		if( sts || !ne ) break;

		add_log_entry( &entry );

		id = entry.id;
	} while( 1 );
	glob.lastid = id;
	free( buf );
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
	case WM_DROPFILES:
		main_dropfiles( hwnd, (HDROP)wparam );
		break;
	case WM_SIZE:
		main_resize( hwnd, wparam, LOWORD( lparam ), HIWORD( lparam ) );
		break;
	case WM_TIMER:
		main_timer( hwnd );
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}
