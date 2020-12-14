
{ -*- mode:fvm -*- }


Program Log(0,0,LogRead,LogWrite,LogWritef);
Begin

Include "syscall.pas";
Include "string.pas";

Procedure LogRead(logname : string, idHigh : int, idLow : int, var flags : int, var lenp : int, var bufp : opaque)
Begin
	var len : int;
	var buf : opaque[2048];
	
	Syscall LogRead(logname,idhigh,idlow,2048,buf,flags,len);
	bufp = buf;
	lenp = len;
End;

Procedure LogWrite(logname : string, flags : int, len : int, buf : opaque)
Begin
	Syscall LogWrite(logname,flags,len,buf);
End;

Procedure LogWritef(flags : int, fmt : string, arg1 : int, arg2 : int, arg3 : int, arg4 : int)
Begin
	var str : string[1024];
	var len : int;

	Syscall LogWrite(0,LogLvlTrace,5,"Here");
	
	Syscall Sprintf(str,fmt,arg1,arg2,arg3,arg4);
	
	Call Strlen(str,len);
	Syscall LogWrite(0,flags,len,str);
End;


End.