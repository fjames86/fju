
{ -*- mode:fvm -*- }

{
 * This module exports CHT syscalls as procedures so it can be
 * invoked from e.g. raft commands.
}

Program Cht(0,0,ChtRead,ChtWrite,ChtDelete);
Begin

Include "syscall.pas";

Procedure ChtRead(keylen : int, keybuf : opaque, var datalen : int, var databuf : opaque)
Begin
	var l : int;
	var buf : opaque[2048];

	Syscall ChtRead(keylen,keybuf,2048,buf,l);
	datalen = l;
	databuf = buf;
End;

Procedure ChtWrite(keylen : int, keybuf : opaque, datalen : int, databuf : opaque)
Begin
	Syscall ChtWrite(keylen,keybuf,datalen,databuf);
End;

Procedure ChtDelete(keylen : int, keybuf : opaque)
Begin
	Syscall ChtDelete(keylen,keybuf);
End;

End.
