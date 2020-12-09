{ -*- mode:fvm -*- }


{ 
  * This defines a module which does the following:
  * - Monitors a configurable set of logs (stored as keys in freg /fju/nls/logs)
  * - When a new entry is appended issue an fvm cluster command to run a specified
  * command procedure.
  * - The command includes the originating hostid. Don't write if the message originated locally
  * - Increment internal seqnos so that no subsequent commands are issued after replicating
  * log entries.
  * 
}

Program NLS(10001,1,NlsProcNull,NlsProcList,NlsInit,NlsService,NlsCommand);
Begin

   { includes }
   Include "syscall.pas";
   
   { constants }

   { declarations }
   
   { globals }
   var nlogs : u32;
   var lognames : string[256]; # 32 byte name, 8 names max = 256
   var logids : u32[16];
   
{ procedures }
Procedure NlsProcNull()
Begin
End;

Procedure Memcpy(dlen : u32, dest : opaque, slen : u32, src : opaque)
Begin
	var i : u32;
	var len : u32;

	len = dlen;
	if slen < dlen then len = slen;
	
	i = 0;
	While i < len Do Begin
	      dest[i] = src[i];
	      i = i + 4;
	End;
End;

Procedure NlsProcList(var lognames2 : string)
Begin
	var p : opaque;
  	lognames2 = "";
   	p = lognames + 32;
	Call Memcpy(32, p, "hello", 6);
End;

Procedure GetLogId(logname : string, var logidHigh : u32, var logidLow : u32)
Begin
	
End;

Procedure SetLogId(logname : string, logidHigh : u32, logidLow : u32)
Begin
End;

Procedure PublishCommand(logname : string, idh : u32, idl : u32)
Begin
End;

Procedure CheckLogId(logname : string)
Begin
End;

{ initialization routine - load log names from registry and set log ids }
Procedure NlsInit(argcount : u32, argbuf : opaque, var rescount : u32, var resbuf : opaque )
Begin
End;

Procedure NlsService(argcount : u32, argbuf : opaque, var rescount : u32, var resbuf : opaque )
Begin

End;

{ Command procedure - invoked to replicate log entries }
Procedure NlsCommand(argcount : u32, argbuf : opaque, var rescount : u32, var resbuf : opaque )
Begin
End;




End.


