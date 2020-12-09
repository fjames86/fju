
{ -*- mode:fvm -*- }

Const LogLvlTrace = 0x00;
Const LogLvlDebug = 0x01;
Const LogLvlInfo = 0x02;
Const LogLvlWarn = 0x03;
Const LogLvlError = 0x04;
Const LogLvlFatal = 0x05;
Const LogBinary = 0x10;
Declare Syscall LogWrite(flags : u32, len : u32, buf : opaque) : 1;

Declare Syscall LogReadNext(previdHigh : u32, previdLow : u32, var len : u32, var buf : opaque, var idHigh : u32, var IdLow : u32) : 2;
Declare Syscall LogRead(idHigh : u32, idLow : u32, var len : u32, var buf : opaque ) : 3;

Const FregTypeOpaque = 0;
Const FregTypeU32 = 1;
Const FregTypeU64 = 2;
Const FregTypeString = 3;
Const FregTypeKey = 4;
Declare Syscall FregNext(path : string, name : string, var entryname : string, var entryType : u32, var entryIdHigh : u32, var entryIdLow : u32) : 4;
Declare Syscall FregReadU32(path : string, var result : u32) : 5;
Declare Syscall FregReadString(path : string, var result : string) : 6;
Declare Syscall FregReadOpaque(path : string, var len : u32, var result : opaque) : 7;
Declare Syscall FregWriteU32(path : string, val : u32) : 8;
Declare Syscall FregWriteString(path : string, val : string) : 9;
Declare Syscall FregWriteOpaque(path : string, len : u32, val : opaque) : 10;
Declare Syscall FregSubkey(path : string, name : string) : 11;
Declare Syscall FregReadU64(path : string, var resultHigh : u32, var resultLow : u32) : 12;
Declare Syscall FregWriteU64(path : string, valHigh : u32, valLow : u32) : 13;

Declare Syscall HostregLocalId(var idHigh : u32, var idLow : u32) : 14;
Declare Syscall HostregNameById(idHigh : u32, idLow : u32, var name : string) : 15;
Declare Syscall HostregIdByName(name : string, var idHigh : u32, idLow : u32 ) : 16;

Declare Syscall RpcNow(var nowHigh : u32, var nowLow : u32) : 17;

Declare Syscall SecRandU32(var r : u32) : 18;

Declare Syscall Sprintf(fmt : string, arg1 : u32, arg2 : u32, arg3 : u32, arg4 : u32, var result : string) : 19;
