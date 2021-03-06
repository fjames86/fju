
#ifndef WINRPC_H
#define WINRPC_H


#include <WinSock2.h>
#include <Windows.h>
#include <CommCtrl.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "resource.h"

#pragma comment(linker,"\"/manifestdependency:type='win32' \
name='Microsoft.Windows.Common-Controls' version='6.0.0.0' \
processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")

HICON winrpc_icon( void );
HBITMAP winrpc_bm( void );

#endif
