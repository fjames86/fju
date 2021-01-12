
#include "fjui.h"

static void log_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "log_name" );
	SetWindowPos( h, HWND_TOP, 100, 5, 200, 23, 0 );
	
	h = fjui_get_hwnd( "log_lv" );
	SetWindowPos( h, HWND_TOP, 5, 30, width - 10, height - 40, 0 );

}


static LRESULT CALLBACK log_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;
	LVCOLUMNA lvc;

	switch(	msg ) {
	case WM_CREATE:
		/* create a listview for hte log entries */
		h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_LISTVIEWA, NULL, WS_VISIBLE|WS_CHILD|LVS_SINGLESEL|LVS_REPORT|LVS_SHOWSELALWAYS|LVS_NOSORTHEADER, 
		5, 50, 400, 400, hwnd, 0, NULL, 0 );
		fjui_hwnd_register( "log_lv", h );
		ListView_SetExtendedListViewStyle( h, LVS_EX_FULLROWSELECT );

		/* add columns */
		memset( &lvc, 0, sizeof(lvc) );
		lvc.pszText = "Timestamp";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
		lvc.cx = 120;
		lvc.iSubItem = 0;
		ListView_InsertColumn( h, 0, &lvc );

		lvc.pszText = "ID";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 100;
		lvc.iSubItem = 1;
		ListView_InsertColumn(h, 1, &lvc );

		lvc.pszText = "Level";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 50;
		lvc.iSubItem = 2;
		ListView_InsertColumn(h, 2, &lvc );

		lvc.pszText = "Message";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 600;
		lvc.iSubItem = 3;
		ListView_InsertColumn(h, 3, &lvc );

		h = CreateWindowA( WC_STATICA, "Log name:", WS_VISIBLE|WS_CHILD, 5, 5, 75, 25, hwnd, 0, NULL, 0 );
		fjui_set_font( h );
		
		h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_EDITA, "fju", WS_VISIBLE|WS_CHILD|WS_BORDER|ES_AUTOHSCROLL, 0, 0, 0, 0, hwnd, 0, NULL, 0 );
		fjui_set_font( h );
		fjui_hwnd_register( "log_name", h );
		
		break;	
	case WM_COMMAND:
		break;
	case WM_SIZE:
		log_size( hwnd, LOWORD(lparam), HIWORD(lparam) );
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

void fjui_log_setinfo( struct fjui_hostinfo *info ) {	
}

void fjui_log_refresh( uint64_t hostid ) {
  char name[64];
  GetWindowTextA( fjui_get_hwnd( "log_name" ), name, sizeof(name) );
  
  ListView_DeleteAllItems( fjui_get_hwnd( "log_lv" ) );
  fjui_call_logread( hostid, name, 0 );
}

int fjui_log_addentry( uint64_t hostid, uint64_t msgid, uint32_t flags, uint64_t timestamp, char *msg, int len, int index ) {
	LVITEMA lvi;
	char str[1024];
	int idx;
	HWND hwnd;
	
	if( hostid != fjui_hostid() ) return -1;

	hwnd = fjui_get_hwnd( "log_lv" );

	idx = ListView_GetItemCount( hwnd );
	while( idx > 1024 ) {
	  ListView_DeleteItem( hwnd, idx - 1 );
	  idx--;
	}

	/* don't add this item if it was already added */
	memset( &lvi, 0, sizeof(lvi) );
	lvi.iItem = index;
	lvi.mask = LVIF_PARAM;
	if( ListView_GetItem( hwnd, &lvi ) && lvi.lParam == msgid ) return 0;
		
	
	memset( &lvi, 0, sizeof(lvi) );
	lvi.mask = LVIF_TEXT|LVIF_PARAM;
	lvi.iItem = index; //0x7ffffffe;
	lvi.iSubItem = 0;
	lvi.lParam = msgid;
	sec_timestr( timestamp, str );	
	lvi.pszText = str;			
	idx = (int)SendMessageA( hwnd, LVM_INSERTITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

	lvi.iItem = idx;
	lvi.mask = LVIF_TEXT;
	lvi.iSubItem = 1;
	sprintf( str, "%"PRIx64"", msgid );
	lvi.pszText = str;			
	SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

	lvi.iItem = idx;
	lvi.mask = LVIF_TEXT;
	lvi.iSubItem = 2;
	sprintf( str, "%s",
		(flags & LOG_LVL_MASK) == LOG_LVL_TRACE ? "Trace" : 
		(flags & LOG_LVL_MASK) == LOG_LVL_DEBUG ? "Debug" : 
		(flags & LOG_LVL_MASK) == LOG_LVL_INFO ? "Info" : 
		(flags & LOG_LVL_MASK) == LOG_LVL_WARN ? "Warn" : 
		(flags & LOG_LVL_MASK) == LOG_LVL_ERROR ? "Error" : 
		(flags & LOG_LVL_MASK) == LOG_LVL_FATAL ? "Fatal" : 
		"Other" );
	lvi.pszText = str;			
	SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

	lvi.iItem = idx;
	lvi.mask = LVIF_TEXT;
	lvi.iSubItem = 3;
	if( flags & LOG_BINARY ) {
		if( len > 512 ) len = 512;
		base64_encode( msg, len, str );	
		lvi.pszText = str;
	} else {
		lvi.pszText = msg;			
	}
	SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

	return 0;
}

static void log_iter_cb( struct rpc_iterator *iter ) {
  char name[64];
  GetWindowTextA( fjui_get_hwnd( "log_name" ), name, sizeof(name) );
  fjui_call_logread( fjui_hostid(), name, 0 );
}

static struct rpc_iterator log_iter = 
{
	NULL,
	0,
	5000,
	log_iter_cb,
	NULL
};


void fjui_log_register( void ) {
	WNDCLASSEXW cls;

	/* register main class and create the window */
	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = log_cb;
	cls.hInstance = fjui_hinstance();
	cls.lpszClassName = L"FJUILOG";
	cls.hbrBackground = GetSysColorBrush( COLOR_WINDOW );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	//cls.hIcon = winrpc_icon();
	//cls.hIconSm = winrpc_icon();

	RegisterClassExW( &cls );

	rpc_iterator_register( &log_iter );
}

