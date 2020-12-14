
{ -*- mode:fvm -*- }


Program Log(0,0,LogRead,LogWrite);
Begin

Include "syscall.pas";

Procedure LogRead(logname : string, idHigh : int, idLow : int, var lenp : int, var bufp : opaque)
Begin
	var len : int;
	var buf : opaque[2048];
	
	Syscall LogRead(logname,idhigh,idlow,2048,buf,len);
	bufp = buf;
	lenp = len;
End;

Procedure LogWrite(logname : string, flags : int, len : int, buf : opaque)
Begin
	Syscall LogWrite(logname,flags,len,buf);
End;

End.
