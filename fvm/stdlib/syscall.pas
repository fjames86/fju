
{ -*- mode:fvm -*- }

Const LogLvlTrace = 0x00;
Const LogLvlDebug = 0x01;
Const LogLvlInfo = 0x02;
Const LogLvlWarn = 0x03;
Const LogLvlError = 0x04;
Const LogLvlFatal = 0x05;
Const LogBinary = 0x10;
Declare Syscall LogWrite(logname : string, flags : u32, len : u32, buf : opaque) : 1;

Declare Syscall LogNext(logname : string, previdHigh : u32, previdLow : u32, var idHigh : u32, var IdLow : u32) : 2;
Declare Syscall LogRead(logname : string, idHigh : u32, idLow : u32, len : u32, buf : opaque, var lenp : u32 ) : 3;

Const FregTypeOpaque = 0;
Const FregTypeU32 = 1;
Const FregTypeU64 = 2;
Const FregTypeString = 3;
Const FregTypeKey = 4;
Const FregMaxName = 64;
Declare Syscall FregNext(path : string, name : string, entryname : string, var entryType : u32, var result : u32) : 4;
Declare Syscall FregReadInt(path : string, var val : u32, var result : int) : 5;
Declare Syscall FregReadString(path : string, val : string, len : int, var result : int) : 6;
Declare Syscall FregReadOpaque(path : string, len : u32, buf : opaque, var lenp : int) : 7;
Declare Syscall FregWriteInt(path : string, val : u32) : 8;
Declare Syscall FregWriteString(path : string, val : string) : 9;
Declare Syscall FregWriteOpaque(path : string, len : u32, val : opaque) : 10;
Declare Syscall FregSubkey(path : string) : 11;
Declare Syscall FregReadU64(path : string, var resultHigh : u32, var resultLow : u32) : 12;
Declare Syscall FregWriteU64(path : string, valHigh : u32, valLow : u32) : 13;

Declare Syscall HostregLocalId(var idHigh : u32, var idLow : u32) : 14;
Declare Syscall HostregNameById(idHigh : u32, idLow : u32, var name : string) : 15;
Declare Syscall HostregIdByName(name : string, var idHigh : u32, idLow : u32 ) : 16;

Declare Syscall RpcNow(var nowHigh : u32, var nowLow : u32) : 17;

Declare Syscall SecRandU32(var r : u32) : 18;

Declare Syscall Sprintf(dest : string, fmt : string, arg1 : u32, arg2 : u32, arg3 : u32, arg4 : u32) : 19;

Declare Syscall XdrDecodeU32(var len : u32, var buf : opaque, var val : u32) : 20;
Declare Syscall XdrDecodeU64(var len : u32, var buf : opaque, var valHigh : u32, var valLow : u32) : 21;
Declare Syscall XdrDecodeString(var len : u32, var buf : opaque, val : string, strlen : u32) : 22;
Declare Syscall XdrDecodeOpaque(var len : u32, var buf : opaque, var lenp : u32, var bufp : opaque) : 23;
Declare Syscall XdrEncodeU32(var len : u32, var buf : opaque, val : u32) : 24;
Declare Syscall XdrEncodeU64(var len : u32, var buf : opaque, valHigh : u32, valLow : u32) : 25;
Declare Syscall XdrEncodeString(var len : u32, var buf : opaque, val : string) : 26;
Declare Syscall XdrEncodeOpaque(var len : u32, var buf : opaque, lenp : u32, bufp : opaque) : 27;

Declare Syscall LogLastId(logname : string, var idHigh : int, var idLow : int) : 28;

Declare Syscall ChtRead(keylen : int, keybuf : opaque, datalen : int, databuf : opaque, var datalenp : int) : 29;
Declare Syscall ChtWrite(keylen : int, keybuf : opaque, datalen : int, databuf : opaque) : 30;
Declare Syscall ChtDelete(keylen : int, keybuf : opaque) : 31;

Declare Syscall Puts(str : string) : 32;

Declare Syscall FvmRun(modname : string, procname : string, arglen : int, argbuf : opaque, reslen : int, resbuf : opaque, var rlen : int) : 34;
