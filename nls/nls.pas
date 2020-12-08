
{ -*- text -*- }

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

{ constants }
const MaxLog = 8;
const MaxLogId = 64; { 8 * MaxLog }
const MaxLogName = 512; { MaxLog * 64 }

{ globals }
var nlogs : u32;
var msgids : opaque[MaxLog];
var lognames : opaque[MaxLogName];

{ procedures }
Procedure NlsProcNull(argcount : u32, argbuf : opaque, var rescount : u32, var resbuf : opaque )
Begin
	rescount = 0;
	resbuf = 0;
End;

Procedure NlsProcList(argcount : u32, argbuf : opaque, var rescount : u32, var resbuf : opaque )
Begin

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


