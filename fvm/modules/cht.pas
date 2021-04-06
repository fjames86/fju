
{ -*- mode: fvm; -*- }

{
 * This module exports CHT syscalls as procedures so it can be
 * invoked from e.g. raft commands.
}

Program Cht(0,0,Init,Exit,ChtRead,ChtWrite,ChtDelete,ChtList);
Begin

Include "syscall.pas";
Include "string.pas";
Include "log.pas";
Include "dmb.pas";

Const ChtMsgWrite = DmbCatCht + 1;
Const ChtMsgDelete = DmbCatCht + 2;

Declare Procedure ChtWrite(keylen : int, keybuf : opaque, datalen : int, databuf : opaque);

Procedure Init()
Begin
	Syscall DmbSubscribe(&ChtWrite,ChtMsgWrite);
End;

Procedure Exit()
Begin
	Syscall DmbUnsubscribe(&ChtWrite);
End;

Procedure ChtRead(keylen : int, keybuf : opaque, var datalen : int, var databuf : opaque)
Begin
	var l : int;
	var buf : opaque[2048];

	If keylen = ChtKeySize Then Begin
            Syscall ChtRead(keybuf,2048,buf,l);
	    datalen = l;
	    databuf = buf;
	End Else Begin
	    datalen = 0;
	    databuf = 0;
        End;
End;

Procedure ChtWrite(keylen : int, keybuf : opaque, datalen : int, databuf : opaque)
Begin
	If Keylen = ChtKeySize Then Syscall ChtWrite(keybuf,datalen,databuf);
End;

Procedure ChtDelete(keylen : int, keybuf : opaque)
Begin
	If Keylen = ChtKeySize Then Syscall ChtDelete(keybuf);
End;

Procedure ChtList(startlen : int, startkey : opaque, var len : int, var keys : opaque)
Begin
	var nk : int;
	var keybuf : opaque[4096];
	var skey : opaque[ChtKeySize];
	
	If startlen <> ChtKeySize Then Begin
	   Call Memset(skey,0,ChtKeySize);
	   startkey = skey;
	End;
	
	Syscall ChtList(startkey,keybuf,4096/ChtKeySize,nk);

	If nk = 0 Then Call LogWritef(LogLvlDebug,"No entries found",0,0,0,0);
	   
	len = nk * ChtKeySize;
	keys = keybuf;
End;

End.


{ Local Variables: }
{ fvm-output-path: "~/fju/bin/cht.fvm" }
{ End: }
