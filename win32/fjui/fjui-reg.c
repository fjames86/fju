
#include "fjui.h"

#define CMD_ADD 1 
#define CMD_REM 2

static void reg_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "reg_tv" );
	SetWindowPos( h, HWND_TOP, 5, 5, 200, height - 10, 0 );
	h = fjui_get_hwnd( "reg_lv" );
	SetWindowPos( h, HWND_TOP, 210, 5, width - 220, height - 100, 0 );
	h = fjui_get_hwnd( "reg_add" );
	SetWindowPos( h, HWND_TOP, 210, height - 90, 75, 25, 0 );
	h = fjui_get_hwnd( "reg_name" );
	SetWindowPos( h, HWND_TOP, 300, height - 90, 100, 22, 0 );
	h = fjui_get_hwnd( "reg_type" );
	SetWindowPos( h, HWND_TOP, 410, height - 90, 100, 22, 0 );
	h = fjui_get_hwnd( "reg_value" );
	SetWindowPos( h, HWND_TOP, 520, height - 90, width - 540, 22, 0 );
	h = fjui_get_hwnd( "reg_rem" );
	SetWindowPos( h, HWND_TOP, 210, height - 60, 75, 25, 0 );
	
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

void reg_additem( char *txt, uint64_t itemid, uint32_t flags, char *buf, int len, HTREEITEM parent ) {
	TVINSERTSTRUCTA tvins;
	HTREEITEM hitem;
	HWND htree, h;
	TVITEMA tvi;
	char str[256];
	LVITEMA lvi;
	int idx;

	if((flags & FREG_TYPE_MASK) == FREG_TYPE_KEY) {

		htree = fjui_get_hwnd( "reg_tv" );

		/* try and find the item, if we do then don't add it a second time */
		hitem = TreeView_GetChild( htree,parent );
		while(hitem) {
			memset( &tvi,0,sizeof( tvi ) );
			tvi.pszText = str;
			tvi.cchTextMax = sizeof( str );
			tvi.mask = TVIF_TEXT|TVIF_PARAM;
			tvi.hItem = hitem;
			if(TreeView_GetItem( htree,&tvi ) &&
				(strcasecmp( str,txt ) == 0)) {
				if(tvi.lParam != itemid) {
					memset( &tvi,0,sizeof( tvi ) );
					tvi.hItem = hitem;
					tvi.mask = TVIF_TEXT|TVIF_PARAM;
					tvi.pszText = txt;
					tvi.cchTextMax = strlen( txt ) + 1;
					tvi.lParam = itemid;
					TreeView_SetItem( htree,&tvi );
				}
				return;
			}

			hitem = TreeView_GetNextSibling( htree,hitem );
		}

		memset( &tvins,0,sizeof( tvins ) );
		tvins.hParent = parent;
		tvins.hInsertAfter = TVI_LAST;
		tvins.item.mask = TVIF_TEXT|TVIF_PARAM|TVIF_IMAGE|TVIF_SELECTEDIMAGE;
		tvins.item.pszText = txt;
		tvins.item.cchTextMax = strlen( txt ) + 1;
		tvins.item.lParam = itemid;
		tvins.item.iImage = ((flags & FREG_TYPE_MASK) == FREG_TYPE_KEY) ? 0 : 1;
		tvins.item.iSelectedImage = ((flags & FREG_TYPE_MASK) == FREG_TYPE_KEY) ? 0 : 1;
		hitem = (HTREEITEM)SendMessageA( htree,TVM_INSERTITEMA,0,(LPARAM)&tvins );
		return;
	}

	h = fjui_get_hwnd( "reg_lv" );

	memset( &lvi,0,sizeof( lvi ) );
	lvi.mask = LVIF_TEXT|LVIF_PARAM;
	lvi.iItem = 0x7ffffffe;
	lvi.iSubItem = 0;
	lvi.lParam = itemid;
	sprintf( str,"%s", txt );
	lvi.pszText = str;
	idx = (int)SendMessageA( h,LVM_INSERTITEMA,0,(LPARAM)(const LV_ITEMA *)(&lvi) );

	lvi.iItem = idx;
	lvi.mask = LVIF_TEXT;
	lvi.iSubItem = 1;
	sprintf( str,"%s",
		(flags & FREG_TYPE_MASK) == FREG_TYPE_UINT32 ? "U32" :
		(flags & FREG_TYPE_MASK) == FREG_TYPE_UINT64 ? "U64" :
		(flags & FREG_TYPE_MASK) == FREG_TYPE_STRING ? "String" :
		(flags & FREG_TYPE_MASK) == FREG_TYPE_OPAQUE ? "Opaque" :
		"Other" );
	lvi.pszText = str;
	SendMessageA( h,LVM_SETITEMA,0,(LPARAM)(const LV_ITEMA *)(&lvi) );

	lvi.iItem = idx;
	lvi.mask = LVIF_TEXT;
	lvi.iSubItem = 2;
	switch( flags & FREG_TYPE_MASK ) {
	case FREG_TYPE_UINT32:
		sprintf( str, "%u (0x%08x)", *((uint32_t *)buf), *((uint32_t *)buf) );
		break;
	case FREG_TYPE_UINT64:
		sprintf( str, "%"PRIu64" (0x%0"PRIx64")", *((uint64_t *)buf), *((uint64_t *)buf) );
		break;
	case FREG_TYPE_STRING:
		sprintf( str, "%s", buf );
		break;
	case FREG_TYPE_OPAQUE:
		base64_encode( buf, len > 128 ? 128 : len, str );
		if( len > 128 ) strcat( str, "..." );
		break;
	}
	lvi.pszText = str;
	SendMessageA( h,LVM_SETITEMA,0,(LPARAM)(const LV_ITEMA *)(&lvi) );

}

static void reg_notify( HWND hwnd, NMHDR *nmhdr ) {
	switch( nmhdr->code ) {
	case TVN_SELCHANGEDA:
	{
		NMTREEVIEWA *nmtv = (NMTREEVIEWA *)nmhdr;
		fjui_call_reglist( fjui_hostid(), nmtv->itemNew.lParam, nmtv->itemNew.hItem );

		ListView_DeleteAllItems( fjui_get_hwnd( "reg_lv" ) );
	}
	break;
	case LVN_ITEMCHANGED:
	  {
	    /* set name/type/value to currently selected item */
	    HWND h;
	    int idx;
	    LVITEMA lvi;
	    char str[1024];
	    
	    h = fjui_get_hwnd( "reg_lv" );
	    idx = ListView_GetNextItem( h, -1, LVNI_SELECTED );
	    if( idx >= 0 ) {
		memset( &lvi, 0, sizeof(lvi) );
		lvi.mask = LVIF_TEXT;
		lvi.pszText = str;
		lvi.cchTextMax = sizeof(str);
		lvi.iItem = idx;
		lvi.iSubItem = 0;
		ListView_GetItem( h, &lvi );
		SetWindowTextA( fjui_get_hwnd( "reg_name" ), str );
		memset( &lvi, 0, sizeof(lvi) );
		lvi.mask = LVIF_TEXT;
		lvi.pszText = str;
		lvi.cchTextMax = sizeof(str);
		lvi.iItem = idx;
		lvi.iSubItem = 1;
		ListView_GetItem( h, &lvi );
		SetWindowTextA( fjui_get_hwnd( "reg_type" ), str );
		memset( &lvi, 0, sizeof(lvi) );
		lvi.mask = LVIF_TEXT;
		lvi.pszText = str;
		lvi.cchTextMax = sizeof(str);
		lvi.iItem = idx;
		lvi.iSubItem = 2;
		ListView_GetItem( h, &lvi );
		SetWindowTextA( fjui_get_hwnd( "reg_value" ), str );
	    }

	  }
	  break;
	}
}

static void reg_getselected( uint64_t *parentid, uint64_t *itemid, HTREEITEM *hparent ) {
	int idx;
	LVITEMA lvi;
	HTREEITEM hitem;
	TVITEMA tvi;

	*itemid = 0;
	*parentid = 0;
	if( hparent ) *hparent = 0;
	
	idx = ListView_GetNextItem( fjui_get_hwnd( "reg_lv" ), -1, LVNI_SELECTED );
	if( idx >= 0 ) {
		memset( &lvi, 0, sizeof(lvi) );
		lvi.mask = LVIF_PARAM;
		lvi.iItem = idx;
		ListView_GetItem( fjui_get_hwnd( "reg_lv" ), &lvi );

		*itemid = lvi.lParam;
	}

	hitem = TreeView_GetSelection( fjui_get_hwnd( "reg_tv" ) );
	if( hitem ) {
		memset( &tvi, 0, sizeof(tvi) );
		tvi.hItem = hitem;
		tvi.mask = TVIF_PARAM;
		TreeView_GetItem( fjui_get_hwnd( "reg_tv" ), &tvi );

		*parentid = tvi.lParam;
		if( hparent ) *hparent = hitem;
	}

}


static void reg_command( HWND hwnd, int id, int cmd ) {
	uint64_t parentid, itemid;

	switch( id ) {
	case CMD_ADD:
	{
		char name[64], type[64], value[256];
		uint32_t flags, u32;
		uint64_t u64;
		char *buf;
		int len;
		char bufp[256], *term;

		reg_getselected( &parentid, &itemid, NULL );
		GetWindowTextA( fjui_get_hwnd( "reg_name" ), name, sizeof(name) );
		GetWindowTextA( fjui_get_hwnd( "reg_type" ), type, sizeof(type) );
		GetWindowTextA( fjui_get_hwnd( "reg_value" ), value, sizeof(value) );


		if( strcasecmp( type, "key" ) == 0 ) {
			flags = FREG_TYPE_KEY;
			buf = NULL;
			len = 0;
			//parentid = itemid; //?
		} else if( strcasecmp( type, "u32" ) == 0 ) {
			flags = FREG_TYPE_UINT32;
			u32 = strtoul( value, &term, 0 );
			buf = &u32;
			len = 4;
		} else if( strcasecmp( type, "u64" ) == 0 ) {
			flags = FREG_TYPE_UINT64;
			u64 = strtoull( value, &term, 0 );
			buf = &u64;
			len = 8;
		} else if( strcasecmp( type, "string" ) == 0 ) {
			flags = FREG_TYPE_STRING;
			buf = value;
			len = strlen( buf ) + 1;
		} else if( strcasecmp( type, "opaque" ) == 0 ) {
			flags = FREG_TYPE_OPAQUE;
			len = base64_decode( bufp, sizeof(bufp), value );
			if( len < 0 ) return;
			buf = bufp;
		} else return;

		if( parentid ) fjui_call_regput( fjui_hostid(), parentid, name, flags, buf, len );
	}
	break;
	case CMD_REM:
	  reg_getselected( &parentid, &itemid, NULL );
		if( parentid && itemid ) {
			fjui_call_regrem( fjui_hostid(), parentid, itemid );
		}
	break;
	}
}


static LRESULT CALLBACK reg_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;
	HIMAGELIST himl;
	LVCOLUMNA lvc;

	switch(	msg ) {
	case WM_CREATE:
		/* create a treeview */
		h = CreateWindowA( WC_TREEVIEWA, NULL, WS_VISIBLE|WS_CHILD|TVS_HASLINES|TVS_LINESATROOT|TVS_HASBUTTONS|TVS_SHOWSELALWAYS, 0, 0, 0, 0, hwnd, 0, 0, NULL );
		fjui_hwnd_register( "reg_tv", h );
		//himl = fjui_create_imagelist( images, 2, 16 );
		//TreeView_SetImageList( h, himl, TVSIL_NORMAL );
		reg_additem( "/", 0, FREG_TYPE_KEY, NULL, 0, TVI_ROOT );

		h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_LISTVIEWA, NULL, WS_VISIBLE|WS_CHILD|LVS_SINGLESEL|LVS_REPORT|LVS_SHOWSELALWAYS|LVS_NOSORTHEADER, 
		5, 50, 400, 400, hwnd, 0, NULL, 0 );
		fjui_hwnd_register( "reg_lv", h );
		ListView_SetExtendedListViewStyle( h, LVS_EX_FULLROWSELECT );

		/* add columns */
		memset( &lvc, 0, sizeof(lvc) );
		lvc.pszText = "Name";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
		lvc.cx = 100;
		lvc.iSubItem = 0;
		ListView_InsertColumn( h, 0, &lvc );

		lvc.pszText = "Type";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 100;
		lvc.iSubItem = 1;
		ListView_InsertColumn(h, 1, &lvc );

		lvc.pszText = "Value";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 300;
		lvc.iSubItem = 2;
		ListView_InsertColumn(h, 2, &lvc );

		h = CreateWindowA( WC_BUTTON, "Put", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON, 0, 0, 0, 0, hwnd, CMD_ADD, 0, 0 );
		fjui_set_font( h );
		fjui_hwnd_register( "reg_add", h );
		h = CreateWindowA( WC_BUTTON, "Delete", WS_VISIBLE|WS_CHILD|BS_PUSHBUTTON, 0, 0, 0, 0, hwnd, CMD_REM, 0, 0 );
		fjui_set_font( h );
		fjui_hwnd_register( "reg_rem", h );
		h = CreateWindowA( WC_EDITA, "Name", WS_VISIBLE|WS_CHILD|WS_BORDER, 0, 0, 0, 0, hwnd, 0, 0, 0 );
		fjui_set_font( h );
		fjui_hwnd_register( "reg_name", h );
		h = CreateWindowA( WC_COMBOBOXA, "", WS_VISIBLE|WS_CHILD|CBS_DROPDOWN, 0, 0, 0, 0, hwnd, 0, 0, 0 );
		fjui_set_font( h );
		SendMessageA( h, CB_ADDSTRING, 0, (LPARAM)"U32" ); 
		SendMessageA( h, CB_ADDSTRING, 0, (LPARAM)"U64" ); 
		SendMessageA( h, CB_ADDSTRING, 0, (LPARAM)"String" ); 
		SendMessageA( h, CB_ADDSTRING, 0, (LPARAM)"Opaque" ); 
		SendMessageA( h, CB_ADDSTRING, 0, (LPARAM)"Key" ); 
		SendMessageA( h, CB_SETCURSEL, 0, 0 );
		fjui_hwnd_register( "reg_type", h );

		h = CreateWindowA( WC_EDITA, "Value", WS_VISIBLE|WS_CHILD|WS_BORDER|ES_AUTOHSCROLL, 0, 0, 0, 0, hwnd, 0, 0, 0 );
		fjui_set_font( h );
		fjui_hwnd_register( "reg_value", h );

		break;	
	case WM_COMMAND:
		reg_command( hwnd, LOWORD( wparam ), HIWORD( wparam ) );
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
	ListView_DeleteAllItems( fjui_get_hwnd( "reg_lv" ) );
	
	fjui_call_reglist( hostid, 0, hroot );
}

void fjui_reg_refresh_selected( uint64_t hostid ) {
  uint64_t parentid, itemid;
  HTREEITEM hitem;
  
  reg_getselected( &parentid, &itemid, &hitem );
  if( parentid && hitem ) fjui_call_reglist( hostid, parentid, hitem );
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

