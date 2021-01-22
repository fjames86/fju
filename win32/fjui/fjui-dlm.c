

#include "fjui.h"

static void dlm_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "dlm_lv" );
	SetWindowPos( h, HWND_TOP, 5, 5, width - 10, height - 10, 0 );

}

static LRESULT CALLBACK dlm_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;
	LVCOLUMNA lvc;
	HIMAGELIST himl;
	  
	switch( msg ) {
	case WM_CREATE:
		h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_LISTVIEWA, NULL, WS_VISIBLE|WS_CHILD|LVS_SINGLESEL|LVS_REPORT|LVS_SHOWSELALWAYS|LVS_NOSORTHEADER, 
		5, 50, 400, 400, hwnd, 0, NULL, 0 );
		fjui_hwnd_register( "dlm_lv", h );
		ListView_SetExtendedListViewStyle( h, LVS_EX_FULLROWSELECT );

		/* TODO: have differnt icons for locked/waiting/released */
		himl = fjui_create_imagelist( 16, "class", "method", NULL );
		ListView_SetImageList( h, himl, LVSIL_SMALL );
		
		/* add columns */
		memset( &lvc, 0, sizeof(lvc) );
		lvc.pszText = "LockID";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
		lvc.cx = 300;
		lvc.iSubItem = 0;
		lvc.iImage = 0;
		ListView_InsertColumn( h, 0, &lvc );

		lvc.pszText = "ResID";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 1;
		ListView_InsertColumn(h, 1, &lvc );

		lvc.pszText = "Mode";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 2;
		ListView_InsertColumn(h, 2, &lvc );

		lvc.pszText = "Cookie";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 3;
		ListView_InsertColumn(h, 3, &lvc );

		break;	
	case WM_COMMAND:
		break;
	case WM_SIZE:
		dlm_size(hwnd, LOWORD(lparam), HIWORD(lparam) );
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

void fjui_dlm_register( void ) {
	WNDCLASSEXW cls;

	/* register main class and create the window */
	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = dlm_cb;
	cls.hInstance = fjui_hinstance();
	cls.lpszClassName = L"FJUIDLM";
	cls.hbrBackground = GetSysColorBrush( COLOR_WINDOW );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	//cls.hIcon = winrpc_icon();
	//cls.hIconSm = winrpc_icon();

	RegisterClassExW( &cls );
}

void fjui_dlm_refresh( uint64_t hostid ) {	
	HWND hwnd;
	int i, idx;
	LVITEMA lvi;
	char str[1024];
	struct fjui_hostinfo *info;

	hwnd = fjui_get_hwnd( "dlm_lv" );
	ListView_DeleteAllItems( hwnd );

	info = fjui_hostinfo_by_id( hostid );
	if( !info ) return;

	for( i = 0; i < info->nlock; i++ ) {
		memset( &lvi, 0, sizeof(lvi) );
		lvi.mask = LVIF_TEXT|LVIF_PARAM|LVIF_IMAGE;
		lvi.iItem = 0x7ffffffe;
		lvi.iSubItem = 0;
		lvi.lParam = i;
		lvi.iImage = 0;
		sprintf( str, "%"PRIx64"", info->lock[i].lockid );
		lvi.pszText = str;			
		idx = (int)SendMessageA( hwnd, LVM_INSERTITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 1;
		sprintf( str, "%"PRIx64"", info->lock[i].resid );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 2;
		sprintf( str, "%u", info->lock[i].mode );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 3;
		sprintf( str, "%s", info->lock[i].cookie );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

	}

}