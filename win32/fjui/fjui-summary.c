
#define _CRT_SECURE_NO_WARNINGS

#include <WinSock2.h>
#include <Windows.h>
#include <CommCtrl.h>

#include "fjui.h"

/*
 * This implements a summary tab view to show things like registered rpc services,
 * licensing info, fvm modules loaded etc.
 */

static void summary_size( HWND hwnd, int width, int height ) {
	HWND h;

	//h = fjui_get_hwnd( "rpcbind_lv" );
	//SetWindowPos( h, HWND_TOP, 5, 75, 200, 100, 0 );
}

static LRESULT CALLBACK fjui_summary_cb( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	HWND h;
	LVCOLUMNA lvc;

	switch( msg ) {
	case WM_CREATE:
		h = CreateWindowExA( WS_EX_TRANSPARENT, WC_STATICA, "Licensed", WS_VISIBLE|WS_CHILD, 5, 5, 55, 15, hwnd, 0, 0, 0 );
		fjui_set_font( h );

		h = CreateWindowExA( WS_EX_TRANSPARENT, WC_STATICA, "", WS_VISIBLE|WS_CHILD, 100, 5, 100, 15, hwnd, 0, 0, 0 );
		fjui_set_font( h );
		fjui_hwnd_register( "licensed_lbl", h );

		h = CreateWindowExA( WS_EX_TRANSPARENT, WC_STATICA, "Expiry", WS_VISIBLE|WS_CHILD, 5, 25, 55, 15, hwnd, 0, 0, 0 );
		fjui_set_font( h );

		h = CreateWindowExA( WS_EX_TRANSPARENT, WC_STATICA, "", WS_VISIBLE|WS_CHILD, 100, 25, 150, 20, hwnd, 0, 0, 0 );
		fjui_set_font( h );
		fjui_hwnd_register( "expiry_lbl", h );

		h = CreateWindowExA( WS_EX_CLIENTEDGE, WC_LISTVIEWA, NULL, WS_VISIBLE|WS_CHILD|LVS_SINGLESEL|LVS_REPORT|LVS_SHOWSELALWAYS|LVS_NOSORTHEADER, 
		5, 50, 400, 400, hwnd, 0, NULL, 0 );
		fjui_hwnd_register( "rpcbind_lv", h );
		ListView_SetExtendedListViewStyle( h, LVS_EX_FULLROWSELECT );

		/* add columns */
		memset( &lvc, 0, sizeof(lvc) );
		lvc.pszText = "Progam";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH;
		lvc.cx = 120;
		lvc.iSubItem = 0;
		ListView_InsertColumn( h, 0, &lvc );

		lvc.pszText = "Version";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 1;
		ListView_InsertColumn(h, 1, &lvc );

		lvc.pszText = "Protocol";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 2;
		ListView_InsertColumn(h, 2, &lvc );

		lvc.pszText = "Name";
		lvc.cchTextMax = (int)strlen( lvc.pszText );
		lvc.mask = LVCF_TEXT|LVCF_WIDTH|LVCF_SUBITEM;
		lvc.cx = 80;
		lvc.iSubItem = 3;
		ListView_InsertColumn(h, 3, &lvc );

		break;	
	case WM_COMMAND:
		//fjui_main_command( hwnd, LOWORD( wparam ) );
		break;
	case WM_SIZE:
		summary_size(hwnd, LOWORD(lparam), HIWORD(lparam) );
		break;
	case WM_CTLCOLORSTATIC:
#if 0
		h = (HWND)lparam;
		if( h == fjui_get_hwnd( "summary_lbl" ) ) {
			//SetBkMode( (HDC)wparam, COLOR_BACKGROUND );
			//SetBkMode( (HDC)wparam, TRANSPARENT );
			//SetBkColor( (HDC)wparam, TRANSPARENT );
            return (BOOL) GetStockObject(COLOR_BACKGROUND);
		}
#else
		//SetBkColor((HDC)wparam, GetStockObject(COLOR_BACKGROUND) );
		SetBkMode((HDC)wparam, TRANSPARENT );
		return (BOOL)GetStockObject( NULL_PEN );
#endif
		break;
	}

	return DefWindowProcW( hwnd, msg, wparam, lparam );
}

void fjui_summary_refresh( uint64_t hostid ) {
	fjui_call_getlicinfo( hostid );
	fjui_call_connlist( hostid );
	fjui_call_rpcbindlist( hostid );
}

void fjui_set_label( char *lblname, char *text ) {
	HWND hwnd, hparent;
	RECT rect;

	hwnd = fjui_get_hwnd( lblname );
	if( !hwnd ) return;
	SetWindowTextA( hwnd, text );
	GetClientRect(hwnd,&rect);
	hparent = GetParent(hwnd);
	MapWindowPoints(hwnd, hparent, (POINT *)&rect, 2 );
	InvalidateRect(hparent,&rect,TRUE);
}

static char *rpcbind_name_by_prog( uint32_t prog, char *str ) {
  struct freg_entry entry;
  int sts;
  uint32_t progid;
  uint64_t id, hkey;

  strcpy( str, "" );

  sts = freg_subkey( NULL, 0, "/fju/rpc/progreg", FREG_CREATE, &hkey );
  if( !sts ) {
    id = 0;
    sts = freg_next( NULL, hkey, id, &entry );
    while( !sts ) {
      if( (entry.flags & FREG_TYPE_MASK) == FREG_TYPE_UINT32 ) {
	sts = freg_get( NULL, entry.id, NULL, (char *)&progid, sizeof(progid), NULL );
	if( !sts && progid == prog ) {
	strcpy( str, entry.name );
	return str;
	}
      }
      
      id = entry.id;
      sts = freg_next( NULL, hkey, id, &entry );
    }
  }
  
  return str;
}

void fjui_summary_setinfo( struct fjui_hostinfo *hinfo ) {
	char str[64];
	LVITEMA lvi;
	HWND hwnd;
	int idx, i;

	fjui_set_label( "licensed_lbl", hinfo && hinfo->hostid ? "True" : "False" );
	fjui_set_label( "expiry_lbl", sec_timestr( hinfo ? hinfo->lic.expire : 0, str ) );

	hwnd = fjui_get_hwnd( "rpcbind_lv" );
	ListView_DeleteAllItems( hwnd );
	
	for( i = 0; i < hinfo->nrpcbind; i++ ) {

		memset( &lvi, 0, sizeof(lvi) );
		lvi.mask = LVIF_TEXT|LVIF_PARAM|LVIF_PARAM;
		lvi.iItem = 0x7ffffffe;
		lvi.iSubItem = 0;
		sprintf( str, "%d", hinfo->rpcbind[i].prog );
		lvi.pszText = str;			
		idx = (int)SendMessageA( hwnd, LVM_INSERTITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 1;
		sprintf( str, "%u", hinfo->rpcbind[i].vers );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 2;
		sprintf( str, "%u %s", hinfo->rpcbind[i].prot, hinfo->rpcbind[i].prot == IPPROTO_UDP ? "UDP" : "TCP" );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );

		lvi.iItem = idx;
		lvi.mask = LVIF_TEXT;
		lvi.iSubItem = 3;
		rpcbind_name_by_prog( hinfo->rpcbind[i].prog, str );
		lvi.pszText = str;			
		SendMessageA( hwnd, LVM_SETITEMA, 0, (LPARAM)(const LV_ITEMA *)(&lvi) );
	}

}

static void summary_iter_cb( struct rpc_iterator *iter ) {
	fjui_summary_refresh( fjui_hostid() );
}

static struct rpc_iterator summary_iter = 
{
	NULL,
	0,
	30000,
	summary_iter_cb,
	NULL
};

void fjui_summary_register( void ) {
	WNDCLASSEXW cls;

	/* register main class and create the window */
	memset( &cls, 0, sizeof(cls) );
	cls.cbSize = sizeof(cls);
	cls.lpfnWndProc = fjui_summary_cb;
	cls.hInstance = fjui_hinstance();
	cls.lpszClassName = L"FJUISUMMARY";
	cls.hbrBackground = GetSysColorBrush( COLOR_WINDOW );
	cls.hCursor = LoadCursorW( NULL, (LPCWSTR)IDC_ARROW );
	//cls.hIcon = winrpc_icon();
	//cls.hIconSm = winrpc_icon();

	RegisterClassExW( &cls );

	//rpc_iterator_register( &summary_iter );
}
