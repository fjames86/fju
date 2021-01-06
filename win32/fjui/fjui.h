
#ifndef FJUI_H
#define FJUI_H

#define _CRT_SECURE_NO_WARNINGS

#define strcasecmp _stricmp

#include <Winsock2.h>
#include <Windows.h>
#include <CommCtrl.h>

#include <stdint.h>
#include <inttypes.h>
#include <stdarg.h>

#include <fju/lic.h>
#include <fju/fvm.h>
#include <fju/hostreg.h>
#include <fju/mmf.h>
#include <fju/sec.h>
#include <fju/hrauth.h>
#include <fju/log.h>
#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/raft.h>
#include <fju/freg.h>

#include "resource.h"

void fjui_set_font( HWND hwnd );
void fjui_net_service( void );
void fjui_hwnd_register( char *name, HWND hwnd );
HWND fjui_get_hwnd( char *name );


struct fjui_hostinfo {
	struct fjui_host_info *next;

	uint64_t hostid;
	char name[256];
	
	struct lic_s lic;
	
	struct fvm_module modules[32];
	int nmodule;

	struct {
		uint64_t connid;
		uint32_t dirtype;
		uint32_t cstate;
		uint64_t rx, tx;
		uint32_t coffset, ccount;
		uint32_t type;
		struct sockaddr_in sin;
	} conn[16];
	int nconn;

	struct {
		uint32_t prog, vers, prot, port;
	} rpcbind[64];
	int nrpcbind;

	struct raft_cluster raft[32];
	int nraft;
};
struct fjui_hostinfo *fjui_hostinfo_by_name( char *name );
struct fjui_hostinfo *fjui_hostinfo_by_id( uint64_t hostid );
struct fjui_hostinfo *fjui_hostinfo_add( uint64_t hostid );

HINSTANCE fjui_hinstance( void );

void fjui_set_statusbar( int index, char *fmt, ... );
void fjui_call_getlicinfo( uint64_t hostid );
uint64_t fjui_hostid( void );
void fjui_summary_setinfo( struct fjui_hostinfo *hinfo );
void fjui_call_raftlist( uint64_t hostid );
void fjui_call_rpcbindlist( uint64_t hostid );
void fjui_call_fvmlist( uint64_t hostid );
void fjui_call_connlist( uint64_t hostid );
void fjui_summary_refresh( uint64_t hostid );
void fjui_fvm_register( void );
void fjui_set_label( char *lblname, char *text );
void fjui_fvm_setinfo( struct fjui_hostinfo *info );
void fjui_fvm_refresh( uint64_t hostid );
void fjui_call_fvmrun( uint64_t hostid, char *modname, char *procname, struct xdr_s *args );
void fjui_fvm_setcallres( struct xdr_s *xdr );

void fjui_raft_register( void );
void fjui_raft_setinfo( struct fjui_hostinfo *info );
void fjui_raft_refresh( uint64_t hostid );

void fjui_log_register( void );
void fjui_log_setinfo( struct fjui_hostinfo *info );
void fjui_log_refresh( uint64_t hostid );

void fjui_reg_register( void );
void fjui_reg_refresh( uint64_t hostid );
void fjui_call_reglist( uint64_t hostid, uint64_t hitem, HTREEITEM hparent );
void reg_additem( char *txt, uint64_t itemid, uint32_t flags, char *buf, int len, HTREEITEM parent );
void reg_deletechildren( HTREEITEM parent );

void fjui_call_logread( uint64_t hostid, uint64_t lastid );
int fjui_log_addentry( uint64_t hostid, uint64_t msgid, uint32_t flags, uint64_t timestamp, char *msg, int len );

void fjui_init_bitmaps( void );
HBITMAP fjui_get_bitmap( char *name );
HICON fjui_get_icon( char *name );
HIMAGELIST fjui_create_imagelist( char **images, int nimages, int bitwidth );

void fjui_call_regrem( uint64_t hostid, uint64_t parentid, uint64_t itemid );
void fjui_call_regput( uint64_t hostid, uint64_t parentid, char *name, uint32_t flags, char *buf, int len );

#endif

