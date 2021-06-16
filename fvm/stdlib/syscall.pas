
{ -*- mode:fvm -*- }

Const LogLvlTrace = 0x00;
Const LogLvlDebug = 0x01;
Const LogLvlInfo = 0x02;
Const LogLvlWarn = 0x03;
Const LogLvlError = 0x04;
Const LogLvlFatal = 0x05;
Const LogBinary = 0x10;
Declare Syscall LogWrite(handle : int, flags : u32, len : u32, buf : opaque) : 1;

Declare Syscall LogNext(handle : int, previdHigh : u32, previdLow : u32, var idHigh : u32, var IdLow : u32) : 2;
Declare Syscall LogRead(handle : int, idHigh : u32, idLow : u32, len : u32, buf : opaque, var flags : int, var lenp : u32 ) : 3;
Declare Syscall LogLastId(handle : int, var idHigh : int, var idLow : int) : 4;

{
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
}
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
{ Note syscalls 27/28 are unused }

Record LogInfo =
    SeqH : int;
    SeqL : int;
    TimestampH : int;
    TimestampL : int;
    MsgLen : int;
    Flags : int;
End;
Declare Syscall LogReadInfo(handle : int, idh : int, idl : int, loginfo : opaque) : 29;
Declare Syscall LogPrev(handle : int, idHigh : int, idLow : int, var high : int, var low : int ) : 30;
Declare Syscall ChtList(startkey : opaque,keybuf : opaque, nkeybuf : int, var nkeys : int) : 31;
Const DmbLocal = 0x1;   { Not published remotely }
Const DmbRemote = 0x2;  { Not published locally }
Declare Syscall DmbPublish(msgid : int, flags : int, len : int, buf : opaque, var seqH : int, var seqL : int) : 32;
{ 
  * Subscribe to a specific message or all messages if msgid=0. 
  * Pass flag DmbFlagApply to use the message buffer as args to the procedure. 
  * Pass flag DmbFlagRaw (default) procedure is passed args as signature (msgid : int, len : int, buf : opaque) 
  * 
}
Const DmbFlagRaw = 0x0000; { subscriber must have signature (msgid,len,buf) }
Const DmbFlagApply = 0x0001; { subscriber is passed msg buffer directly as args }
Declare Syscall DmbSubscribe(procaddr : int, msgid : int, flags : int) : 33;
Declare Syscall DmbUnsubscribe(procaddr : int) : 34;
{ Get Info about the registered host. seq is the message seq last acked by host }
Declare Syscall DmbHostInfo(hostH : int, hostL : int, var seqH : int, var seqL : int) : 35;
{ Get info about currently invoked dmb message. Only valid when called from a dmb msg handler }
Declare Syscall DmbMsgInfo(var hostH : int, var hostL : Int, var seqH : int, var seqL : int) : 36;
{ Send an RPC call to another host. HostH/HostL names the host. Prog/vers/proc names the procedure. len/buf names the args. resultprocaddr is optional and is address of a procedure called when reply received }
Record RpcCallInfo =
       HostH : int;
       HostL : int;
       Prog : int;
       Vers : int;
       Proc : int;
       Timeout : int;
End;
Declare Syscall RpcCall(info : int, len : int, buf : opaque, resultprocaddr : int, private : int) : 37;
Declare Syscall Sleep(timeout : int, cbaddr : int, private : int) : 38;

Const SHA1HashSize = 20;
Record SecIov =
       Len : int;
       Buf : int;
End;

{ iov is address of vector of SecIov records. Hash points to a vector of Sha1HashSize }
Declare Syscall SHA1(niov : int, iov : int, hash : opaque) : 39;

Const AesKeySize = 16;
Declare Syscall AesEncrypt(len : int, buf : opaque, key : opaque) : 40;
Declare Syscall AesDecrypt(len : int, buf : opaque, key : opaque) : 41;

Declare Syscall Open(filename : string, var fd : int) : 42;
Declare Syscall Close(fd : int) : 43;
Declare Syscall Read(fd : int, len : int, buf : opaque, offset : int) : 44;
Declare Syscall Write(fd : int, len : int, buf : opaque, offset : int) : 45;
Declare Syscall FileSize(fd : int, var len : int) : 46;
Declare Syscall LogOpen(name : string, var handle : int) : 47;
Declare Syscall LogClose(handle : int) : 48;
Declare Syscall Timestr(timeh : int, timel : int, str : string) : 49;
Declare Syscall TimeNow(var high : int, var low : int) : 50;

{
 * Register a raft application.
 * Command,snapsave and snapload are addresses of procedures that are invoked by raft framework.
 * Command(clidHigh,clidLow,seqHigh,seqLow,len,buf)
 * Snapsave(clidHigh,clidLow,termH,termL,seqH,seqL);
 * Snapload(clidH,clidL,len,buf)
}
Declare Syscall RaftAppRegister(appid : int, name : string, command : int, snapsave : int, snapload : int) : 51;
Declare Syscall RaftAppUnregister(appid : int) : 52;
Declare Syscall RaftClidByAppid(appid : int, var clidH : int, var clidL : int) : 53;

Declare Syscall FregOpen(path : string, var handle : int) : 54;
Declare Syscall FregClose(handle : int) : 55;

Const FregTypeOpaque = 0;
Const FregTypeU32 = 1;
Const FregTypeU64 = 2;
Const FregTypeString = 3;
Const FregTypeKey = 4;
Const FregTypeMask = 0xf;
Const FregMaxName = 64;

Record FregEntry =
  IDH : int;
  IDL : int;
  Len : int;
  Flags : int;
  ParentIDH : int;
  ParentIDL : int;
  Name : string[FregMaxName];
End;

Declare Syscall FregEntryByName(handle : int, parentH : int, parentL : int, name : string, entry : int, var result : int) : 56;
Declare Syscall FregGet(handle : int, idH : int, idL : int, len : int, buf : opaque, var flags : int, var lenp : int) : 57;
Declare Syscall FregPut(handle : int, parentH : int, parentL : int, name : string, flags : int, len : int, buf : opaque, var idH : int, var idL : int) : 58;
Declare Syscall FregSubkey(handle : int, parentH : int, parentL : int, name : string, var idH : int, var idL : int) : 59;
Declare Syscall FregRem(handle : int, idH : int, idL : int, var result : int) : 60;
Declare Syscall FregEntryByID(handle : int, idH : int, idL : int, entry : int) : 61;
Declare Syscall FregGetByName(handle : int, parentH : int, parentL : int, name : string, flags : int, len : int, buf : opaque, var lenp : int) : 62;
Declare Syscall FregNext(handle : int, parentH : int, parentL : int, idH : int, idL : int, entry : int, var result : int) : 63;
Declare Syscall RaftSnapshotSave(clidH : int, clidL : int, termH : int, term : int, seqH : int, seqL : int, len : int, buf : opaque, offset : int) : 64;
Declare Syscall RaftClusterAddMember(clidH : int, clidL : int, hostH : int, hostL : int) : 65;
Declare Syscall RaftClusterRemMember(clidH : int, clidL : int, hostH : int, hostL : int) : 66;
Declare Syscall FregPath(regh : int, idH : int, idL : int, path : string, len : int ) : 67;
Declare Syscall FvmSaveData( id : int, var saveid : int, var result : int ) : 68;
Declare Syscall FvmLoadData( id : int, var result : int ) : 69;
Declare Syscall FvmRemoveData( id : int, var result : int ) : 70;
