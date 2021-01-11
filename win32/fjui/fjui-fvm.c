
#include "fjui.h"

#define CMD_CALL 1
#define CMD_LOAD 2
#define CMD_UNLOAD 3


static void fvm_size( HWND hwnd, int width, int height ) {
	HWND h;

	h = fjui_get_hwnd( "fvm_lv" );
	SetWindowPos( h, HWND_TOP, 5, 5, width - 10, height - 100, 0 );

	h = fjui_get_hwnd( "fvm_loadbtn" );
	SetWindowPos( h, HWND_TOP, 5, height - 90, 75, 25, 0 );
	h = fjui_get_hwnd( "fvm_unloadbtn" );
	SetWindowPos( h, HWND_TOP, 100, height - 90, 75, 25, 0 );

	h = fjui_get_hwnd( "fvm_calltxt" );
	SetWindowPos( h, HWND_TOP, 100, height - 50, width - 150, 25, 0 );
	h = fjui_get_hwnd( "fvm_callbtn" );
	SetWindowPos( h, HWND_TOP, 5, height - 50, 75, 25, 0 );
	
	
}

static void fvm_callsig( char *modname, char *procname, uint64_t siginfo, char *str ) {
	int k;
	sprintf( str, "%s/%s(", modname, procname );
	for( k = 0; k < FVM_SIGINFO_NARGS(siginfo); k++ ) {
		if( FVM_SIGINFO_ISVAR(siginfo, k) ) continue;
		switch( FVM_SIGINFO_VARTYPE(siginfo, k) ) {
			case 0: 
			if( FVM_SIGINFO_VARTYPE(siginfo,k + 1) == 2 ) continue;
			if( k > 0 ) sprintf( str + strlen( str ), ", " );
			sprintf( str + strlen( str ), "0" );
			break;
			case 1: 
			if( k > 0 ) sprintf( str + strlen( str ), ", " );
			sprintf( str + strlen( str ), "String" );
			break;
			case 2: 
			if( k > 1 ) sprintf( str + strlen( str ), ", " );
			sprintf( str + strlen( str ), "Opaque" );
			break;
			case 3: 
			if( k > 0 ) sprintf( str + strlen( str ), ", " );
			sprintf( str + strlen( str ), "Other" );
			break;
		}
	}
	sprintf( str + strlen( str ), ")" );
}

static void fvm_notify( HWND hwnd, NMHDR *nmhdr ) {
	HWND h;

	switch( nmhdr->code ) {
	case LVN_ITEMACTIVATE:
		{
			int idx;
			LVITEMA lvi;
			char str[1024];
			struct fvm_module *mp;
			struct fjui_hostinfo *info;
			
			h = fjui_get_hwnd( "fvm_lv" );
			idx = SendMessageA( h, LVM_GETNEXTITEM, -1, LVNI_SELECTED );

			if( idx >= 0 ) {
				memset( &lvi, 0, sizeof(lvi) );
				lvi.iItem = idx;
				lvi.mask = LVIF_TEXT|LVIF_PARAM;
				lvi.pszText = str;
				lvi.cchTextMax = 1024;
				SendMessageA( h, LVM_GETITEMA, 0, &lvi );
				
				info = fjui_hostinfo_by_id( fjui_hostid() );

				mp = (struct fvm_module *)&info->modules[lvi.lParam & 0xffff];
				fvm_callsig( mp->name, mp->procs[(lvi.lParam >> 16) & 0xffff].name, mp->procs[(lvi.lParam >> 16) & 0xffff].siginfo, str );
				SetWindowTextA( fjui_get_hwnd( "fvm_calltxt" ), str );
			}
		}
		break;
	case LVN_ITEMCHANGED:
		{
			LVITEMA lvi;
			NMLISTVIEW *arg = (NMLISTVIEW *)nmhdr;
			char str[1024];
			struct fvm_module *mp;
			struct fjui_hostinfo *info;

			if( (arg->uNewState & LVIS_SELECTED) && !(arg->uOldState & LVIS_SELECTED) ) {
				memset( &lvi, 0, sizeof(lvi) );
				lvi.iItem = arg->iItem;
				lvi.mask = LVIF_TEXT|LVIF_PARAM;
				lvi.pszText = str;
				lvi.cchTextMax = 1024;
				SendMessageA( nmhdr->hwndFrom, LVM_GETITEMA, 0, &lvi );

				info = fjui_hostinfo_by_id( fjui_hostid() );
				if( !info ) return;
				mp = (struct fvm_module *)&info->modules[lvi.lParam & 0xffff];
				fvm_callsig( mp->name, mp->procs[(lvi.lParam >> 16) & 0xffff].name, mp->procs[(lvi.lParam >> 16) & 0xffff].siginfo, str );

				SetWindowTextA( fjui_get_hwnd( "fvm_calltxt" ), str );
			}
		}
		break;
	}
}

static void fvm_command( HWND hwnd, int id, int cmd, HWND hcmd ) {
	char str[1024];
	char modname[64], procname[64], *p;
	struct xdr_s args;
	char argbuf[4096];
	uint64_t siginfo;
	struct fjui_hostinfo *info;
	int i, j;
	uint32_t u32;
	char *term;

	switch( id ) {
	case CMD_CALL:
		GetWindowTextA( fjui_get_hwnd( "fvm_calltxt" ), str, sizeof(str) );
		/* parse into module/procname/args */
		p = strtok( str, "/" );
		if( !p ) break;
		strcpy( modname, p );

		p = strtok( NULL, "(" );
		if( !p ) break;
		strcpy( procname, p );

		/* now parse args delimited by comma. we need to know the signature */
		siginfo = 0;
		info = fjui_hostinfo_by_id( fjui_hostid() );
		if( !info ) return;
		for( i = 0; i < info->nmodule; i++ ) {
			if( strcasecmp( info->modules[i].name, modname ) == 0 ) {
				for( j = 0; j < info->modules[i].nprocs; j++ ) {
					if( strcasecmp( info->modules[i].procs[j].name, procname ) == 0 ) {
						siginfo = info->modules[i].procs[j].siginfo;
						break;
					}
				}
			}
			if( siginfo ) break;
		}

		xdr_init( &args, (uint8_t *)argbuf, sizeof(argbuf) );
		for( i = 0; i < FVM_SIGINFO_NARGS(siginfo); i++ ) {
			if( FVM_SIGINFO_ISVAR( siginfo, i ) ) continue;

			switch( FVM_SIGINFO_VARTYPE( siginfo, i ) ) {
			case 0: 
			/* integer */
			if( FVM_SIGINFO_VARTYPE(siginfo,i + 1) == 2 ) continue;

			p = strtok( NULL, ",)" );
			if( !p ) {
				MessageBoxA( hwnd, "Expected param", "Error", MB_OK|MB_ICONERROR );
				return;
			}

			u32 = strtol( p, &term, 0 );
			if( *term ) {
				MessageBoxA( hwnd, "Failed to parse u32", "Error", MB_OK|MB_ICONERROR );
				return;
			}

			xdr_encode_uint32( &args, u32 );
			break;
			case 1:
			/* string */
			p = strtok( NULL, ",)" );
			if( !p ) {
				MessageBoxA( hwnd, "Expected param", "Error", MB_OK|MB_ICONERROR );
				return;
			}

			xdr_encode_string( &args, p );
			break;
			case 2:
			/* opaque */
			p = strtok( NULL, ",)" );
			if( !p ) {
				MessageBoxA( hwnd, "Expected param", "Error", MB_OK|MB_ICONERROR );
				return;
			}

			u32 = base64_decode( args.buf + args.offset + 4, args.count - args.offset - 4, p );
			if( (int)u32 < 0 ) {
				MessageBoxA( hwnd, "Failed to decode base64", "Error", MB_OK|MB_ICONERROR );
				return;
			}
			xdr_encode_uint32( &args, u32 );
			if( u32 % 4 ) u32 += 4 - (u32 % 4);
			args.offset += u32;
			break;
			}
		}

		fjui_call_fvmrun( fjui_hostid(), modname, procname, &args );


	break;
	case CMD_LOAD:
	  {
	    OPENFILENAMEA ofn;
	    char fname[256];
	    memset( &ofn, 0, sizeof(ofn) );
	    ofn.lStructSize = sizeof(ofn);
	    ofn.hwndOwner = hwnd;
	    ofn.lpstrFilter = "FVM module file (*.fvm)\0*.fvm\0\0\0";
	    ofn.lpstrFile = fname;
	    ofn.nMaxFile = sizeof(fname);
	    ofn.lpstrTitle = "FVM module";
	    ofn.Flags = OFN_FILEMUSTEXIST|OFN_PATHMUSTEXIST;
	    if( GetOpenFileNameA( &ofn ) ) {
	      struct mmf_s mmf;
	      int sts;
	      
	      sts = mmf_open2( fname, &mmf, MMF_OPEN_EXISTING );
	      if( sts ) {
		MessageBoxA( hwnd, "Error", "Failed to open file", MB_OK|MB_ICONERROR );
	      } else {
		mmf_remap( &mmf, mmf.fsize );
		fjui_call_fvmload( fjui_hostid(), mmf.file, mmf.fsize, 0, 0 );
		mmf_close( &mmf );
	      }
	    }
	  }
	  break;
	case CMD_UNLOAD:
	  {
	    HWND h;
	    int idx;
	    LVITEMA lvi;
	    struct fjui_hostinfo *hinfo;
	    
	    h = fjui_get_hwnd( "fvm_lv" );
	    idx = SendMessageA( h, LVM_GETNEXTITEM, -1, LVNI_SELECTED );
	    
	    if( idx >= 0 ) {
	      memset( &lvi, 0, sizeof(lvi) );
	      lvi.iItem = idx;
	      lvi.mask = LVIF_PARAM;
	      SendMessageA( h, LVM_GETITEMA, 0, &lvi );
	      idx = (lvi.lParam & 0xffff);
	      hinfo = fjui_hostinfo_by_id( fjui_hostid() );
	      if( hinfo && idx < hinfo->nmodule ) {
		fjui_call_fvmunload( fjui_hostid(), hinfo->modules[idx].name );
	      }
	    }
	  }
	  break;
	}
}

void fjui_fvm_setcallres( struct xdr_s *xdr ) {
	HWND h;
	LVITEMA lvi;
	int idx;
	struct fjui_hostinfo *info;
	struct fvm_module *mp;
	char str[1024];
	int i;
	uint64_t siginfo;
	uint32_t u32;
	int nn;
	char *bufp;

	if( !xdr ) {
		SetWindowTextA( fjui_get_hwnd( "fvm_calltxt" ), "Timeout" );
		return;
	}

	SetWindowTextA( fjui_get_hwnd( "fvm_calltxt" ), "" );

	h = fjui_get_hwnd( "fvm_lv" );
	idx = SendMessageA( h, LVM_GETNEXTITEM, -1, LVNI_SELECTED );
	if( idx < 0 ) return;

	memset( &lvi, 0, sizeof(lvi) );
	lvi.iItem = idx;
	lvi.mask = LVIF_TEXT|LVIF_PARAM;
	SendMessageA( h, LVM_GETITEMA, 0, &lvi );
				
	info = fjui_hostinfo_by_id( fjui_hostid() );
	mp = (struct fvm_module *)&info->modules[lvi.lParam & 0xffff];
	sprintf( str, "%s/%s(", mp->name, mp->procs[(lvi.lParam >> 16) & 0xffff].name );
	siginfo = mp->procs[(lvi.lParam >> 16) & 0xffff].siginfo;
	nn = 0;
	for( i = 0; i < FVM_SIGINFO_NARGS(siginfo); i++ ) {
		if( !FVM_SIGINFO_ISVAR(siginfo,i) ) continue;
		if( FVM_SIGINFO_VARTYPE(siginfo,i+1) == 2 ) continue;
		if( nn > 0 ) sprintf( str + strlen( str ), "," );
		switch( FVM_SIGINFO_VARTYPE(siginfo,i) ) {
		case 0:
		xdr_decode_uint32( xdr, &u32 );
		sprintf( str + strlen( str ), "%u", u32 );
		break;
		case 1:
		xdr_decode_string( xdr, str + strlen( str ), 64 );
		break;
		case 2:
		xdr_decode_opaque_ref( xdr, &bufp, &u32 );
		base64_encode( bufp, u32, str + strlen( str ) );
		//sprintf( str + strlen( str ), "Opaque(%u)", u32 );
		break;
		}
		nn++;
	}
	sprintf( str + strlen( str ), ")" );
	SetWindowTextA( fjui_get_hwnd( "fvm_calltxt" ), str );
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


		h = CreateWindowA( WC_EDITA, "", WS_VISIBLE|WS_CHILD|WS_BORDER|ES_AUTOHSCROLL, 100, 0, 100, 25, hwnd, 0, 0, NULL );
		fjui_set_font( h );
		fjui_hwnd_register( "fvm_calltxt", h );

		h = CreateWindowA( WC_BUTTONA, "Call", WS_VISIBLE|WS_CHILD|WS_BORDER, 5, 0, 75, 25, hwnd, CMD_CALL, 0, NULL );
		fjui_set_font( h );
		fjui_hwnd_register( "fvm_callbtn", h );

		h = CreateWindowA( WC_BUTTONA, "Load...", WS_VISIBLE|WS_CHILD|WS_BORDER, 5, 0, 75, 25, hwnd, CMD_LOAD, 0, NULL );
		fjui_set_font( h );
		fjui_hwnd_register( "fvm_loadbtn", h );
		h = CreateWindowA( WC_BUTTONA, "Unload", WS_VISIBLE|WS_CHILD|WS_BORDER, 5, 0, 75, 25, hwnd, CMD_UNLOAD, 0, NULL );
		fjui_set_font( h );
		fjui_hwnd_register( "fvm_unloadbtn", h );

		break;	
	case WM_COMMAND:
		fvm_command( hwnd, LOWORD( wparam ), HIWORD( wparam ), (HWND)lparam );
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
		lvi.mask = LVIF_TEXT|LVIF_PARAM;
		lvi.iItem = 0x7ffffffe;
		lvi.iSubItem = 0;
		lvi.lParam = i;
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
			lvi.mask = LVIF_TEXT|LVIF_PARAM;
			lvi.iItem = 0x7ffffffe;
			lvi.iSubItem = 0;
			lvi.lParam = (j << 16) | i;
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

