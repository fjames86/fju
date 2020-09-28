
{ -*- text -*- }

Program EventLog(2300000,1,EventCallback);
Begin

Procedure EventCallback(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var eventid : integer;
	var str : string[64];

	eventid := ^argbuf;

	Syscall Sprintf(str,64,"RPC Event %u", AddressOf eventid);
	Syscall LogStr(str);
	
End;
   
End.
