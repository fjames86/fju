
#include "fjui.h"

static void raft_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "raft_lv" );
	SetWindowPos( h, HWND_TOP, 5, 5, width - 10, height - 10, 0 );

}

static LRESULT CALLBACK raft_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;
	LVCOLUMNA lvc;

	switch(	msg ) {
	case WM_CREATE:
		/* create listview for the entries */
		h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_LISTVIEWA, NULL, WS_VISIBLE|WS_CHILD|LVS_SINGLESEL|LVS_REPORT|LVS_SHOWSELALWAYS|LVS_NOSORTHEADER, 
		5, 50, 400, 400, hwnd, 0, NULL, 0 );
		fjui_hwnd_register( "raft_lv", h );
		ListView_SetExtendedListViewStyle( h, LVS_EX_FULLROWSELECT );

		/* add columns */
		memset( &lvc, 0, sizeof(lvc) );
		lvc.pszText = "CLID";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
		lvc.cx = 120;
		lvc.iSubItem = 0;
		ListView_InsertColumn( h, 0, &lvc );

		lvc.pszText = "State";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 1;
		ListView_InsertColumn(h, 1, &lvc );

		lvc.pszText = "Leader";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 100;
		lvc.iSubItem = 2;
		ListView_InsertColumn(h, 2, &lvc );

		lvc.pszText = "Cookie";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 3;
		ListView_InsertColumn(h, 3, &lvc );

		lvc.pszText = "CommitSeq";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 120;
		lvc.iSubItem = 4;
		ListView_InsertColumn(h, 4, &lvc );

		lvc.pszText = "Term";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 120;
		lvc.iSubItem = 5;
		ListView_InsertColumn(h, 5, &lvc );
		
		break;	
	case WM_COMMAND:
		break;
	case WM_SIZE:
		raft_size( hwnd, LOWORD(lparam), HIWORD(lparam) );
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

void fjui_raft_setinfo( struct fjui_hostinfo *info ) {	
	HWND hwnd;
	int i, idx, j, k;
	LVITEMA lvi;
	char str[1024];

	hwnd = fjui_get_hwnd( "raft_lv" );
	ListView_DeleteAllItems( hwnd );
	for( i = 0; i < info->nraft; i++ ) {
		memset( &lvi, 0, sizeof(lvi) );
		lvi.mask = LVIF_TEXT|LVIF_PARAM;
		lvi.iItem = 0x7ffffffe;
		lvi.iSubItem = 0;
		lvi.lParam = i;
		sprintf( str, "%"PRIx64"", info->raft[i].clid );
		lvi.pszText = str;			
		idx = (int)SendMessageA( hwnd, LVM_INSERTITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 1;
		sprintf( str, "%s",
			info->raft[i].state == RAFT_STATE_LEADER ? "Leader" : 
			info->raft[i].state == RAFT_STATE_CANDIDATE ? "Candidate" : 
			info->raft[i].state == RAFT_STATE_FOLLOWER ? "Follower" : 
			"Other" );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 2;
		if( hostreg_name_by_hostid( info->raft[i].leaderid, str ) == NULL ) sprintf( str, "%"PRIx64"", info->raft[i].leaderid );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 3;
		sprintf( str, "%s", info->raft[i].cookie );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 4;
		sprintf( str, "%"PRIu64"", info->raft[i].commitseq );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 5;
		sprintf( str, "%"PRIu64"", info->raft[i].term );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );		
	}

}

void fjui_raft_refresh( uint64_t hostid ) {
	fjui_call_raftlist( hostid );
}

void fjui_raft_register( void ) {
	WNDCLASSEXW cls;

	/* register main class and create the window */
	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = raft_cb;
	cls.hInstance = fjui_hinstance();
	cls.lpszClassName = L"FJUIRAFT";
	cls.hbrBackground = GetSysColorBrush( COLOR_WINDOW );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	//cls.hIcon = winrpc_icon();
	//cls.hIconSm = winrpc_icon();

	RegisterClassExW( &cls );
}

