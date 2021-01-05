
#include "fjui.h"

static void reg_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "reg_tv" );
	SetWindowPos( h, HWND_TOP, 5, 5, width - 10, height - 10, 0 );

}

void reg_deletechildren( HTREEITEM parent ) {
	HWND htree;
	HTREEITEM hitem;

	htree = fjui_get_hwnd( "reg_tv" );

	hitem = TreeView_GetChild( htree, parent );
	while( hitem ) {
		TreeView_DeleteItem( htree, hitem );
		hitem = TreeView_GetChild( htree, parent );
	}
}

HTREEITEM reg_additem( char *txt, uint64_t itemid, uint32_t flags, char *buf, int len, HTREEITEM parent ) {
	TVINSERTSTRUCTA tvins;
	HTREEITEM hitem;
	HWND htree;
	TVITEMA tvi;
	char str[256];

	htree = fjui_get_hwnd( "reg_tv" );

	/* try and find the item, if we do then don't add it a second time */
	hitem = TreeView_GetChild( htree, parent );
	while( hitem ) {
		memset( &tvi, 0, sizeof(tvi) );
		tvi.pszText = str;
		tvi.cchTextMax = sizeof(str);
		tvi.mask = TVIF_TEXT|TVIF_PARAM;
		tvi.hItem = hitem;
		if( TreeView_GetItem( htree, &tvi ) && 
			(strcasecmp( str, txt ) == 0) ) {
			if( tvi.lParam != itemid ) {
				memset( &tvi, 0, sizeof(tvi) );
				tvi.hItem = hitem;
				tvi.mask = TVIF_TEXT|TVIF_PARAM;
				tvi.pszText = txt;
				tvi.cchTextMax = strlen( txt ) + 1;
				tvi.lParam = itemid;
				TreeView_SetItem( htree, &tvi );
			}
			return hitem;
		}

		hitem = TreeView_GetNextSibling( htree, hitem );
	}

	memset( &tvins, 0, sizeof(tvins) );
	tvins.hParent = parent;
	tvins.hInsertAfter = TVI_LAST;
	tvins.item.mask = TVIF_TEXT|TVIF_PARAM;
	tvins.item.pszText = txt;
	tvins.item.cchTextMax = strlen( txt ) + 1;
	tvins.item.lParam = itemid;

	hitem = (HTREEITEM)SendMessageA( htree, TVM_INSERTITEMA, 0, (LPARAM)&tvins ); 
	return hitem;
}

static void reg_notify( HWND hwnd, NMHDR *nmhdr ) {
	switch( nmhdr->code ) {
	case TVN_SELCHANGEDA:
	{
		NMTREEVIEWA *nmtv = (NMTREEVIEWA *)nmhdr;
		fjui_call_reglist( fjui_hostid(), nmtv->itemNew.lParam, nmtv->itemNew.hItem );
	}
	break;
	}
}


static LRESULT CALLBACK reg_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;

	switch(	msg ) {
	case WM_CREATE:
		/* create a treeview */
		h = CreateWindowA( WC_TREEVIEWA, NULL, WS_VISIBLE|WS_CHILD|TVS_HASLINES|TVS_LINESATROOT|TVS_HASBUTTONS, 0, 0, 0, 0, hwnd, 0, 0, NULL );
		fjui_hwnd_register( "reg_tv", h );
		reg_additem( "/", 0, FREG_TYPE_KEY, NULL, 0, TVI_ROOT );
		break;	
	case WM_COMMAND:
		break;
	case WM_SIZE:
		reg_size( hwnd, LOWORD(lparam), HIWORD(lparam) );
		break;
	case WM_NOTIFY:
		reg_notify( hwnd, (NMHDR *)lparam );
		break;
	case WM_CTLCOLORSTATIC:
		SetBkMode((HDC)wparam, TRANSPARENT );
		return (BOOL)GetStockObject( NULL_PEN );
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}

void fjui_reg_refresh( uint64_t hostid ) {
	HTREEITEM hroot;

	hroot = TreeView_GetRoot( fjui_get_hwnd( "reg_tv" ) );
	reg_deletechildren( hroot );

	fjui_call_reglist( hostid, 0, hroot );
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

