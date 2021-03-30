
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
Declare Syscall LogRead(logname : string, idHigh : u32, idLow : u32, len : u32, buf : opaque, var flags : int, var lenp : u32 ) : 3;
Declare Syscall LogLastId(logname : string, var idHigh : int, var idLow : int) : 4;

Const FregTypeOpaque = 0;
Const FregTypeU32 = 1;
Const FregTypeU64 = 2;
Const FregTypeString = 3;
Const FregTypeKey = 4;
Const FregMaxName = 64;
Declare Syscall FregNext(path : string, name : string, entryname : string, var entryType : u32, var result : u32) : 5;
Declare Syscall FregReadInt(path : string, var val : u32, var result : int) : 6;
Declare Syscall FregReadString(path : string, val : string, len : int, var result : int) : 7;
Declare Syscall FregReadOpaque(path : string, len : u32, buf : opaque, var lenp : int) : 8;
Declare Syscall FregWriteInt(path : string, val : u32) : 9;
Declare Syscall FregWriteString(path : string, val : string) : 10;
Declare Syscall FregWriteOpaque(path : string, len : u32, val : opaque) : 11;
Declare Syscall FregSubkey(path : string) : 12;
Declare Syscall FregReadU64(path : string, var resultHigh : u32, var resultLow : u32) : 13;
Declare Syscall FregWriteU64(path : string, valHigh : u32, valLow : u32) : 14;
Declare Syscall HostregLocalId(var idHigh : u32, var idLow : u32) : 15;
Declare Syscall HostregNameById(idHigh : u32, idLow : u32, name : string) : 16;
Declare Syscall HostregIdByName(name : string, var idHigh : u32, var idLow : u32 ) : 17;
Declare Syscall RpcNow(var nowHigh : u32, var nowLow : u32) : 18;
Declare Syscall SecRandU32(var r : u32) : 19;
Declare Syscall Sprintf(dest : string, fmt : string, arg1 : u32, arg2 : u32, arg3 : u32, arg4 : u32) : 20;

Const ChtKeySize = 16;
Declare Syscall ChtRead(keybuf : opaque, datalen : int, databuf : opaque, var datalenp : int) : 21;
Declare Syscall ChtWrite(keybuf : opaque, datalen : int, databuf : opaque) : 22;
Declare Syscall ChtDelete(keybuf : opaque) : 23;
Declare Syscall Puts(str : string) : 24;
Declare Syscall FvmRun(modname : string, procname : string, arglen : int, argbuf : opaque, reslen : int, resbuf : opaque, var rlen : int) : 25;
Declare Syscall RaftCommand(idHigh : int, idLow : int, len : int, buf : opaque) : 26;
Declare Syscall FvmClRun(idHigh : int, idLow : int, modname : string, procname : string, len : int, buf : opaque) : 27;
Declare Syscall FvmClRunOthers(idHigh : int, idLow : int, modname : string, procname : string, len : int, buf : opaque) : 28;

Declare Syscall LogReadInfo(logname : string, idHigh : int, idLow : int, var len : int, var flags : int, var timestampHigh : int, var timestampLow : int ) : 29;
Declare Syscall LogPrev(logname : string, idHigh : int, idLow : int, var high : int, var low : int ) : 30;
Declare Syscall ChtList(startkey : opaque,keybuf : opaque, nkeybuf : int, var nkeys : int) : 31;

Const DmbLocal = 0x1;   { Not published remotely }
Const DmbRemote = 0x2;  { Not published locally }
Declare Syscall DmbPublish(msgid : int, flags : int, len : int, buf : opaque, var seqH : int, var seqL : int) : 32;

{ 
  * Subscribe to a specific message or all messages if msgid=0. When subscribing to a specific message 
  * the procedure is invoked as if the message buffer were the procedure args. 
  * if subscrining to all messages the proc is passed args (msgid : int, len : int, buf : opaque) 
}
Declare Syscall DmbSubscribe(modname : string, procname : string, msgid : int) : 33;
Declare Syscall DmbUnsubscribe(modname : string, procname : string) : 34;

{ Get Info about the registered host. seq is the message seq last acked by host }
Declare Syscall DmbHostInfo(hostH : int, hostL : int, var seqH : int, var seqL : int) : 35;

{ Get info about currently invoked dmb message. Only valid when called from a dmb msg handler }
Declare Syscall DmbMsgInfo(var hostH : int, var hostL : Int, var seqH : int, var seqL : int) : 36;

{ Send an RPC call to another host. HostH/HostL names the host. Prog/vers/proc names the procedure. len/buf names the args. resultproc names a procedure called when reply received }
Declare Syscall RpcCall(hostH : int, hostL : int, prog : int, vers : int, proc : int, len : int, buf : opaque, resultproc : string) : 37;
