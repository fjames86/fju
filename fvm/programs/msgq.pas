
{ -*- text -*- }

Program MsgQ(23102222, 1, Service);
Begin

Const MsgQLogName := "msgq";

Procedure Service(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var logidhigh, logidlow, sts : integer;
	var buf : opaque[64];
	var count : integer;

again:
	{ read from the msg log }
	Do 
	Begin
		Syscall NextLogEntry(MsgQLogName, logidLow, logidHigh, sts);
		If sts Then
		Begin
			count := 64;
			Syscall ReadLog(MsgQLogName,logidLow,logidHigh,buf,count);
			{ TODO - do something with it }
		End;
	End
	While sts;
	
	Syscall Yield(1000,0,sts);
	Goto Again;
	
End;

End.

