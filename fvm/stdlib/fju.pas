
{ -*- text -*- }

{ 
  * This provides an interface to the native procedures i.e. some libfju interfaces.
  * It is not an rpc program but instead a module to be called either from the fvm cluster
  * interface or other fvm programs.
}

Program FJU(PROG_STDLIB,1,FregPutString,LogWriteMsg,LogWrite,ChtWrite);
Begin

{ write an freg string. args: path to entry, value to write }
Procedure FregPutString(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var len : integer;
	var pathp : string;
	var valp : string;
	
	rescount := 0;
	resbuf := 0;

	len := ^argbuf;

	pathp := argbuf;
	valp := argbuf + len + 4;
	If len % 4 <> 0 Then valp := valp + 4 - (len % 4);

	Syscall WriteRegString(pathp,valp);
End;

{ write to the log. args: message }
Procedure LogWriteMsg(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var msgp : string;
	
	rescount := 0;
	resbuf := 0;

	msgp := argbuf;
	Syscall LogStr(msgp);
End;

Procedure LogWrite(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var msgp, lognamep : string;
	var len : integer;
	
	rescount := 0;
	resbuf := 0;

	lognamep := argbuf;

	len := ^lognamep;
	If len % 4 <> 0 Then len := len + 4 - (len % 4);
	argbuf := argbuf + 4 + len;
	argcount := argcount - (4 + len);
	
	msgp := argbuf;
	len := ^msgp;
	msgp := msgp + 4;
	argcount := argcount - (4 + len);

	If argcount >= 0 Then Syscall WriteLog(lognamep, msgp, len);
End;

Procedure ChtWrite(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque)
Begin
	var key : opaque;
	var buf : opaque;
	var size : integer;

	key := argbuf;
	argbuf := argbuf + 16;
	size := ^argbuf;
	argbuf := argbuf + 4;
	buf := argbuf;

	If argcount >= (size + 20) Then Syscall WriteCht(key,buf,size)
	Else Syscall LogStr("ChtWrite bad args");
		

End;



End.


