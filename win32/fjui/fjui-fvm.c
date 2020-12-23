
#include "fjui.h"

static void fvm_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "fvm_lv" );
	SetWindowPos( h, HWND_TOP, 5, 5, width - 10, height - 100, 0 );

	h = fjui_get_hwnd( "fvm_calltxt" );
	SetWindowPos( h, HWND_TOP, 100, height - 50, width - 150, 25, 0 );

	h = fjui_get_hwnd( "fvm_callbtn" );
	SetWindowPos( h, HWND_TOP, 5, height - 50, 75, 25, 0 );
}

static void fvm_notify( HWND hwnd, NMHDR *nmhdr ) {
	HWND h;

	switch( nmhdr->code ) {
	case LVN_ITEMACTIVATE:
		{
			int idx;
			LVITEMA lvi;
			char str[1024];

			h = fjui_get_hwnd( "fvm_lv" );
			idx = SendMessageA( h, LVM_GETNEXTITEM, -1, LVNI_SELECTED );

			if( idx >= 0 ) {
				memset( &lvi, 0, sizeof(lvi) );
				lvi.iItem = idx;
				lvi.mask = LVIF_TEXT;
				lvi.pszText = str;
				lvi.cchTextMax = 1024;
				SendMessageA( h, LVM_GETITEMA, 0, &lvi );

				SetWindowTextA( fjui_get_hwnd( "fvm_calltxt" ), str );
			}
		}
		break;
	case LVN_ITEMCHANGED:
		{
			LVITEMA lvi;
			NMLISTVIEW *arg = (NMLISTVIEW *)nmhdr;
			char str[1024];

			if( (arg->uNewState & LVIS_SELECTED) && !(arg->uOldState & LVIS_SELECTED) ) {
				memset( &lvi, 0, sizeof(lvi) );
				lvi.iItem = arg->iItem;
				lvi.mask = LVIF_TEXT;
				lvi.pszText = str;
				lvi.cchTextMax = 1024;
				SendMessageA( nmhdr->hwndFrom, LVM_GETITEMA, 0, &lvi );

				SetWindowTextA( fjui_get_hwnd( "fvm_calltxt" ), str );
			}
		}
		break;
	}
}

static LRESULT CALLBACK fvm_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;
	LVCOLUMNA lvc;

	switch( msg ) {
	case WM_CREATE:
		h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_LISTVIEWA, NULL, WS_VISIBLE|WS_CHILD|LVS_SINGLESEL|LVS_REPORT|LVS_SHOWSELALWAYS|LVS_NOSORTHEADER, 
		5, 50, 400, 400, hwnd, 0, NULL, 0 );
		fjui_hwnd_register( "fvm_lv", h );
		ListView_SetExtendedListViewStyle( h, LVS_EX_FULLROWSELECT );

		/* add columns */
		memset( &lvc, 0, sizeof(lvc) );
		lvc.pszText = "Module";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
		lvc.cx = 300;
		lvc.iSubItem = 0;
		ListView_InsertColumn( h, 0, &lvc );

		lvc.pszText = "ProgID";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 1;
		ListView_InsertColumn(h, 1, &lvc );

		lvc.pszText = "DataSize";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 2;
		ListView_InsertColumn(h, 2, &lvc );

		lvc.pszText = "TextSize";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 3;
		ListView_InsertColumn(h, 3, &lvc );

		lvc.pszText = "Timestamp";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 120;
		lvc.iSubItem = 4;
		ListView_InsertColumn(h, 4, &lvc );


		h = CreateWindowA( WC_EDITA, "", WS_VISIBLE|WS_CHILD|WS_BORDER, 100, 0, 100, 25, hwnd, 0, 0, NULL );
		fjui_set_font( h );
		fjui_hwnd_register( "fvm_calltxt", h );

		h = CreateWindowA( WC_BUTTONA, "Call", WS_VISIBLE|WS_CHILD|WS_BORDER, 5, 0, 75, 25, hwnd, 0, 0, NULL );
		fjui_set_font( h );
		fjui_hwnd_register( "fvm_callbtn", h );

		break;	
	case WM_COMMAND:
		break;
	case WM_SIZE:
		fvm_size(hwnd, LOWORD(lparam), HIWORD(lparam) );
		break;
	case WM_NOTIFY:
		fvm_notify( hwnd, (NMHDR *)lparam );
		break;
	case WM_CTLCOLORSTATIC:
		SetBkMode((HDC)wparam, TRANSPARENT );
		return (BOOL)GetStockObject( NULL_PEN );
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}

void fjui_fvm_setinfo( struct fjui_hostinfo *info ) {	
	HWND hwnd;
	int i, idx, j, k;
	LVITEMA lvi;
	char str[1024];

	hwnd = fjui_get_hwnd( "fvm_lv" );
	ListView_DeleteAllItems( hwnd );
	for( i = 0; i < info->nmodule; i++ ) {
		memset( &lvi, 0, sizeof(lvi) );
		lvi.mask = LVIF_TEXT|LVIF_PARAM|LVIF_PARAM;
		lvi.iItem = 0x7ffffffe;
		lvi.iSubItem = 0;
		sprintf( str, "%s", info->modules[i].name );
		lvi.pszText = str;			
		idx = (int)SendMessageA( hwnd, LVM_INSERTITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 1;
		sprintf( str, "%u", info->modules[i].progid );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 2;
		sprintf( str, "%u", info->modules[i].datasize );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 3;
		sprintf( str, "%u", info->modules[i].textsize );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 4;
		sec_timestr( info->modules[i].timestamp, str );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		for( j = 0; j < info->modules[i].nprocs; j++ ) {
			memset( &lvi, 0, sizeof(lvi) );
			lvi.mask = LVIF_TEXT|LVIF_PARAM|LVIF_PARAM;
			lvi.iItem = 0x7ffffffe;
			lvi.iSubItem = 0;
			sprintf( str, "%s/%s(", info->modules[i].name, info->modules[i].procs[j].name );
			for( k = 0; k < FVM_SIGINFO_NARGS(info->modules[i].procs[j].siginfo); k++ ) {
				if( k > 0 ) sprintf( str + strlen( str ), ", " );
				if( FVM_SIGINFO_ISVAR(info->modules[i].procs[j].siginfo, k) ) sprintf( str + strlen( str ), "Var " );
				switch( FVM_SIGINFO_VARTYPE(info->modules[i].procs[j].siginfo, k) ) {
					case 0: 
					sprintf( str + strlen( str ), "Int" );
					break;
					case 1: 
					sprintf( str + strlen( str ), "String" );
					break;
					case 2: 
					sprintf( str + strlen( str ), "Opaque" );
					break;
					case 3: 
					sprintf( str + strlen( str ), "Other" );
					break;
				}
			}
			sprintf( str + strlen( str ), ")" );
			lvi.pszText = str;			
			idx = (int)SendMessageA( hwnd, LVM_INSERTITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

			lvi.iItem = idx;
			lvi.mask = LVIF_TEXT;
			lvi.iSubItem = 1;
			sprintf( str, "%x", info->modules[i].procs[j].address );
			lvi.pszText = str;			
			SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );
		}

	}

}

void fjui_fvm_refresh( uint64_t hostid ) {
	fjui_call_fvmlist( hostid );
}

void fjui_fvm_register( void ) {
	WNDCLASSEXW cls;

	/* register main class and create the window */
	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = fvm_cb;
	cls.hInstance = fjui_hinstance();
	cls.lpszClassName = L"FJUIFVM";
	cls.hbrBackground = GetSysColorBrush( COLOR_WINDOW );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	//cls.hIcon = winrpc_icon();
	//cls.hIconSm = winrpc_icon();

	RegisterClassExW( &cls );
}

