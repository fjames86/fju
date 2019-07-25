/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
*/

#define _CRT_SECURE_NO_WARNINGS 

#include <WinSock2.h>
#include <Windows.h>
#include <ws2tcpip.h>
#include <time.h>

#include <rpc.h>
#include <log.h>
#include <hostreg.h>
#include <hrauth.h>
#include <nls.h>
#include <rpcd.h>


#include "fjlogui.h"

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



struct nls_read_cxt {
  uint64_t hostid;
  uint64_t hshare;
  uint64_t seq;
  uint64_t lastid;
};

static int nls_decode_prop( struct xdr_s *xdr, struct nls_share *share, struct log_prop *prop ) {
  int sts;
  
  sts = xdr_decode_uint64( xdr, &share->hshare );
  if( !sts ) sts = xdr_decode_string( xdr, share->name, sizeof(share->name) );  

  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->version );  
  if( !sts ) sts = xdr_decode_uint64( xdr, &prop->seq );
  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->lbacount );
  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->start );
  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->count );
  if( !sts ) sts = xdr_decode_uint64( xdr, &prop->last_id );
  if( !sts ) sts = xdr_decode_uint32( xdr, &prop->flags );

  return sts;
}

static void nls_read_cb( struct xdr_s *xdr, void *cxt ) {
  int sts, b;
  struct log_s log;
  uint64_t id, lastid, previd, seq;
  uint32_t flags;
  char *bufp = NULL;
  int lenp;
  struct log_entry e;
  struct log_iov iov[1];
  struct nls_read_cxt *nlscxtp = (struct nls_read_cxt *)cxt;
  int logopen = 0;
  struct nls_remote remote;
  struct nls_share rshare;
  struct log_prop prop;
  int nmsgs = 0, eof;
  
  /* do nothing if call timed out? */
  if( !xdr ) {
    MessageBoxA( glob.hwnd[HWND_MAIN], "RPC timeout", "Error", MB_OK|MB_ICONERROR );
    goto done;
  }

  /* decode results */
  sts = xdr_decode_boolean( xdr, &b );
  if( sts || !b ) {
	  MessageBoxA( glob.hwnd[HWND_MAIN], "Error Status", "Error", MB_OK|MB_ICONERROR );
    goto done;
  }

  sts = nls_decode_prop( xdr, &rshare, &prop );
  if( sts ) {
    goto done;
  }


  sts = xdr_decode_boolean( xdr, &b );
  if( sts ) goto done;
  while( b ) {
    sts = xdr_decode_uint64( xdr, &id );
    if( !sts ) sts = xdr_decode_uint64( xdr, &previd );
    if( !sts ) sts = xdr_decode_uint64( xdr, &seq );
    if( !sts ) sts = xdr_decode_uint32( xdr, &flags );
    if( !sts ) sts = xdr_decode_opaque_ref( xdr, (uint8_t **)&bufp, &lenp );
    if( sts ) {
      goto done;
    }

    memset( &e, 0, sizeof(e) );
    iov[0].buf = bufp;
    iov[0].len = lenp;
    e.iov = iov;
    e.niov = 1;
    e.flags = flags;      
	add_log_entry( &e );

    lastid = id;
    nmsgs++;
    
    sts = xdr_decode_boolean( xdr, &b );
    if( sts ) goto done;
  }
  sts = xdr_decode_boolean( xdr, &eof );
  
  /* Did we read all available messages? If not then continue */
  if( eof || nmsgs == 0 || seq == nlscxtp->seq ) {
    goto done;
  }

  nlscxtp->lastid = lastid;
  nls_call_read( nlscxtp->hostid, nlscxtp->hshare, nlscxtp->seq, nlscxtp->lastid );
  
 done:
  free( cxt );
}

/* send a read command to server */
static void nls_call_read( uint64_t hostid, uint64_t hshare, uint64_t seq, uint64_t lastid ) {
  int sts;
  struct nls_read_cxt *nlscxtp;
  struct hrauth_call hcall;
  struct xdr_s xdr;
  uint8_t xdr_buf[32];
  struct hrauth_call_opts opts;

  nlscxtp = malloc( sizeof(*nlscxtp) );
  nlscxtp->hostid = hostid;
  nlscxtp->hshare = hshare;
  nlscxtp->lastid = lastid;
  nlscxtp->seq = seq;

  memset( &hcall, 0, sizeof(hcall) );
  hcall.hostid = hostid;
  hcall.prog = NLS_RPC_PROG;
  hcall.vers = NLS_RPC_VERS;
  hcall.proc = 3;
  hcall.donecb = nls_read_cb;
  hcall.cxt = nlscxtp;
  hcall.timeout = 1000;
  hcall.service = HRAUTH_SERVICE_PRIV;
  xdr_init( &xdr, xdr_buf, sizeof(xdr_buf) );
  xdr_encode_uint64( &xdr, hshare );
  xdr_encode_uint64( &xdr, lastid );
  xdr_encode_uint32( &xdr, sizeof(glob.buf) );  

  memset( &opts, 0, sizeof(opts) );
  opts.mask = HRAUTH_CALL_OPT_FD|HRAUTH_CALL_OPT_TMPBUF|HRAUTH_CALL_OPT_PORT;
  opts.fd = glob.fd;  
  xdr_init( &opts.tmpbuf, glob.buf, sizeof(glob.buf) );
  opts.port = glob.port;
  sts = hrauth_call_udp2( &hcall, &xdr, &opts );
  if( sts ) {
    free( nlscxtp );
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
	AppendMenuA( m,MF_STRING, CMD_CONNECT, "Connect..." );
	AppendMenuA( m, MF_STRING|MF_DISABLED, CMD_CLOSE, "Close" );
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
}

static void add_log_entry( struct log_entry *entry ) {
	LVITEMA lvi;
	time_t now;
	struct tm *tm;
	char str[1024];
	int idx;

	memset( &lvi, 0, sizeof(lvi) );
	lvi.mask = LVIF_TEXT|LVIF_PARAM|LVIF_PARAM;
	lvi.iItem = 0x7ffffffe;
	lvi.iSubItem = 0;
	now = (time_t)entry->timestamp;
	tm = localtime( &now );
	strftime( str, sizeof(str), "%Y-%m-%d %H:%M:%S", tm );
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

static int call_list_shares( uint64_t hostid, int port, struct nls_share *share, int n ) {
 struct rpc_inc inc;
  int sts, handle, b, i;
  struct nls_share tmpshare;
  struct hostreg_host host;
  struct sockaddr_in sin;
  struct log_prop prop;
  struct rpc_call_opts copts;

  sts = hostreg_host_by_id( hostid,&host );
  if(sts) return sts;

  memset( &inc, 0, sizeof(inc) );
  xdr_init( &inc.xdr, glob.buf, sizeof(glob.buf) );

  rpc_init_call( &inc, NLS_RPC_PROG, NLS_RPC_VERS, 1, &handle );
  memset( &sin,0,sizeof( sin ) );
  sin.sin_family = AF_INET;
  sin.sin_port = htons( port );
  sin.sin_addr.s_addr = host.addr[0];
  rpc_complete_call( &inc, handle );

  memcpy( &inc.raddr, &sin, sizeof(struct sockaddr_in) );
  inc.raddr_len = sizeof(struct sockaddr_in);
    
  memset( &copts, 0, sizeof(copts) );
  copts.mask = RPC_CALL_OPT_TIMEOUT|RPC_CALL_OPT_FD;
  copts.timeout = 1000;
  copts.fd = glob.fd;
  sts = rpc_call_udp2( &inc, &copts );
  if( sts ) {
	  goto done;
  }

  sts = rpc_decode_msg( &inc.xdr, &inc.msg );
  if(sts) {
	  goto done;
  }

  sts = rpc_process_reply( &inc );
  if( sts ) {
	  goto done;
  }
  
  /* decode result from xdr */
  sts = xdr_decode_boolean( &inc.xdr, &b );
  if( sts ) {
	  goto done;
  }
  i = 0;
  while( b ) {
    sts = nls_decode_prop( &inc.xdr, &tmpshare, &prop );
	if( sts ) {
		goto done;
	}
	if( i < n ) {
		share[i] = tmpshare;
	}
	sts = xdr_decode_boolean( &inc.xdr, &b );
	if( sts ) {
		goto done;
	}
	i++;
  }
  sts = i;
    
 done:

  return sts;
}

static INT_PTR WINAPI connect_dialog_wndproc( HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam ) {
	int sts;

	switch( msg ) {
	case WM_INITDIALOG:
		{
		int n,m;
		struct hostreg_host *hlist;
		n = hostreg_host_list( NULL,0 );
		hlist = malloc( sizeof( *hlist ) * n );
		m = hostreg_host_list( hlist,n );
		if(m < n) n = m;

		SendMessageA( GetDlgItem( hwnd,IDC_COMBO_HOST ),CB_RESETCONTENT,0,0 );
		for( m = 0; m < n; m++ ) {
			SendMessageA( GetDlgItem( hwnd,IDC_COMBO_HOST ),CB_ADDSTRING,0, hlist[m].name );
		}
		free( hlist );

		SendMessageA( GetDlgItem( hwnd,IDC_COMBO_HOST ),CB_SETCURSEL,0,0 );

		SetDlgItemTextA( hwnd,IDC_EDIT1,"8000" );
		}
		break;
	case WM_COMMAND:
		switch( LOWORD(wparam) ) {
			case IDOK:
			{
				char str[128];
				int sts;
				struct hostreg_host host;
				char name[64];
				uint64_t hshare;

				GetDlgItemTextA( hwnd,IDC_COMBO_HOST,str,sizeof( str ) );
				sts = hostreg_host_by_name( str,&host );
				glob.hostid = host.id;

				GetDlgItemTextA( hwnd,IDC_COMBO_LOG,str,sizeof( str ) );
				sscanf( str,"%s - %llx", name, &hshare );
				glob.hshare = hshare;
			}
			/* fall through */
			case IDCANCEL:
			EndDialog( hwnd, LOWORD(wparam) );
			return TRUE;
			break;
		case IDC_COMBO_HOST:
			/* set log list */
		{
			int sts;
			char str[128];
			struct hostreg_host host;
			struct nls_share shares[32];
			int port;
			int i;

			if( HIWORD(wparam) == CBN_SELCHANGE ) {
				GetDlgItemTextA( hwnd,IDC_COMBO_HOST,str,sizeof( str ) );
				sts = hostreg_host_by_name( str,&host );
				GetDlgItemTextA( hwnd,IDC_EDIT1,str,sizeof( str ) );
				glob.port = strtoul( str,NULL,10 );
				sts = call_list_shares( host.id,glob.port,shares,32 );
				if(sts < 0) {
					MessageBoxA( hwnd,"Failed to contact remote host","Error",MB_OK|MB_ICONERROR );
				} else {
					SendMessageA( GetDlgItem( hwnd,IDC_COMBO_LOG ),CB_RESETCONTENT,0,0 );
					if(sts > 32) sts= 32;
					for(i = 0; i < sts; i++) {
						sprintf( str,"%s - %llx",shares[i].name,shares[i].hshare );
						SendMessageA( GetDlgItem( hwnd,IDC_COMBO_LOG ),CB_ADDSTRING,0,str );
					}
					if(sts > 0) SendMessageA( GetDlgItem( hwnd,IDC_COMBO_LOG ),CB_SETCURSEL,0,0 );
				}
			}

		}
			break;
		}
		break;
	}
	return FALSE;
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
	case CMD_CONNECT:
		/* prompt for host with selection dialog */
		sts = DialogBoxA( NULL, MAKEINTRESOURCE(IDD_DIALOG_CONNECT), hwnd, connect_dialog_wndproc );
		if( sts == IDOK ) {
			/* close log (if any) */
			if( glob.logopen ) {
				log_close( &glob.log );
				EnableMenuItem( GetSubMenu( GetMenu( glob.hwnd[HWND_MAIN] ), 0 ), CMD_CLOSE, MF_BYCOMMAND|MF_DISABLED );	
				glob.logopen = 0;
			}

			/* clear messages */
			ListView_DeleteAllItems( glob.hwnd[HWND_ELIST] );

			/* start reading */
			glob.seq = 0;
			glob.lastid = 0;
			nls_call_read( glob.hostid, glob.hshare, glob.seq, glob.lastid );
		}

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
