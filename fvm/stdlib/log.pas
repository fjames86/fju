
{ -*- mode: fvm -*- }

Procedure LogWritef(lvl : int, fmt : string, arg1 : int, arg2 : int, arg3 : int, arg4 : int)
Begin
	var len : int;
	var str : string[1024];
	
	Syscall Sprintf(str,fmt,arg1,arg2,arg3,arg4);
	Call Strlen(str,len);
	Syscall LogWrite(0, lvl, len, str);
End;
