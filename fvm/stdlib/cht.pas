
{ -*- mode:fvm -*- }

{
 * This module exports CHT syscalls as procedures so it can be
 * invoked from e.g. raft commands.
}

Program Cht(0,0,ChtWrite,ChtDelete);
Begin

Include "syscall.pas";
	
Procedure ChtWrite(keylen : int, keybuf : opaque, datalen : int, databuf : opaque)
Begin
	Syscall ChtWrite(keylen,keybuf,datalen,databuf);
End;

Procedure ChtDelete(keylen : int, keybuf : opaque)
Begin
	Syscall ChtDelete(keylen,keybuf);
End;

End.
