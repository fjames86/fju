
{ -*- mode: fvm; -*- }

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

End.


{ Local Variables: }
{ fvm-output-path: "/opt/fju/fvm/cht.fvm" }
{ End: }
