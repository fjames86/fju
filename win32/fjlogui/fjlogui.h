
#ifndef FJLOGUI_H
#define FJLOGUI_H



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

HICON fjlogui_icon(void);

#define CMD_QUIT 1
#define CMD_OPEN 2 
#define CMD_ABOUT 3
#define CMD_CLOSE 4
#define CMD_CLEAR 5

#endif


