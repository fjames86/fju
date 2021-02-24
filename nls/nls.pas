{ -*- mode:fvm -*- }


{ 
  * This defines a module which does the following:
  * - Monitors a configurable set of logs (stored as keys in freg /fju/nls/logs)
  * - When a new entry is appended, issue an fvm cluster command to run a command procedure.
  * - The command includes the originating hostid. Don't write if the message originated locally.
  * - Increment internal seqnos so that no subsequent commands are issued after replicating log entries.
  * 
  * This results in any messages written localy to be replicated across the cluster.
  * Note it doesn't guarantee ordering, only that the message gets written.
  *
  * Uses for this include e.g. a pub/sub system. 
  * "publish" by writing to a specified logname (logname would be the message category). 
  * "subscribe" by monitoring the log for new messages.
}

Program Nls(0x2FFF7773,1,ProcNull,ProcList,Init,Exit,Service,NlsMsgWrite);
Begin

   { includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   Include "log.pas";
   Include "dmb.pas";
   
   { constants }
   Const MaxLog = 32;
   Const MaxLogName = 32;
   Const LogBufferSize = MaxLog * MaxLogName;
   Const LogIDBufferSize = MaxLog * 2;
   
   Const NlsMsgIdWrite = (DmbCatNls + 0); { dmb message identifier } 

   { declarations }
   
   { globals }
   var nlogs : u32;
   var lognames : string[LogBufferSize]; { 32 byte name, 32 names max = 1024 }
   var logids : u32[LogIDBufferSize];
   
{ procedures }

{ ---------------- RPC interface -------------------- }

Procedure ProcNull()
Begin
	Call LogWritef(LogLvlTrace,"NlsProcNull",0,0,0,0);
End;

Procedure ProcList(var lenp : int, var bufp : opaque) 
Begin
	var offset, i : int;
	var buf : opaque[256];

	Call LogWritef(LogLvlTrace,"NlsProcList",0,0,0,0);
	
	offset = 0;
	Call XdrEncodeU32(buf,offset,nlogs);
	i = 0;
	While i < nlogs Do Begin
	      Call XdrEncodeString(buf,offset,lognames + (i*32));
	      Call XdrEncodeU64(buf,offset,logids[2*i], logids[(2*i) + 1]);
	      i = i + 1;
	End;

	bufp = buf;
	lenp = offset;
End;

{ -------------------- Utility functions ------------------------- }

Procedure GetLogId(logname : string, var logidHigh : u32, var logidLow : u32)
Begin
	var i, result : int;
	i = 0;
	While i < nlogs Do
	Begin
		Call Strcmp(lognames + (i*32), logname, result);
		If result Then Begin
		   logidhigh = logids[2*i];
		   logidlow = logids[(2*i) + 1];
		   Return;
		End;
		i = i + 1;
	End;	
End;

Procedure SetLogId(logname : string, logidHigh : u32, logidLow : u32)
Begin
	var i, result : int;
	i = 0;
	While i < nlogs Do
	Begin
		Call Strcmp(lognames + (i*32), logname, result);
		If result Then Begin
		   logids[2*i] = logidHigh;
		   logids[(2*i) + 1] = logidLow;
		   Return;
		End;
		i = i + 1;
	End;
End;

Procedure PublishCommand(logname : string, flags : int, len : int, buf : opaque)
Begin
	var argbuf : opaque[4096];
	var offset : int;
	var hosth, hostl : int;
	var seqH, seqL : int;
	
	Call LogWritef(LogLvlTrace,"NlsPublishCommand len=%u",len,0,0,0);
	
	offset = 0;
	Call XdrEncodeString(argbuf,offset,logname);
	Call XdrEncodeU32(argbuf,offset,flags);
	Call XdrEncodeOpaque(argbuf,offset,buf,len);

	Syscall DmbPublish(NlsMsgIdWrite,DmbRemote,offset,argbuf,seqH,seqL);
End;

Procedure CheckLogId(logname : string)
Begin
	var idhigh, idlow : int;
	var high, low, pub : int;
	var len, flags : int;
	var buf : opaque[4096];
	
	Syscall LogLastId(logname,idhigh,idlow);
	Call GetLogId(logname,high,low);

	{ read entries until we get to the last one, if any }
	While (idhigh <> high) || (idlow <> low) Do
	Begin
		Syscall LogNext(logname,high,low,high,low);
		Syscall LogRead(logname,high,low,4096,buf,flags,len);
		If len Then Begin
		    Call LogWritef(LogLvlTrace,"Nls New Log entry %s %08x%08x",logname,idhigh,idlow,0);

		    Call PublishCommand(logname,flags,len,buf); 
		End Else Begin
		     high = idhigh;
		     low = idlow;
		End;
	End;
	Call SetLogId(logname,idhigh,idlow);

End;

{ -------------------------- Public procedures ----------------------- }

{ initialization routine - load log names from registry and set log ids }
Procedure Init()
Begin
	var ename : string[32];
	var i, etype, result : int;
	var idhigh, idlow : int;
	
	Syscall FregNext("/fju/nls/logs","",ename,etype,result);
	i = 0;
	While ename[0] Do Begin
	    If etype = FregTypeString Then
	    Begin
  	        Call Strcpy(lognames + (i*32), ename);
		Syscall LogLastId(lognames + (i*32), idhigh,idlow);
		logids[2*i] = idhigh;
		logids[(2*i) + 1] = idlow;
	        i = i + 1;
		If i > MaxLog Then Break;
            End;
	    Syscall FregNext("/fju/nls/logs",ename,ename,etype,result);	
	End;
	nlogs = i;

	Syscall DmbSubscribe("Nls","NlsMsgWrite",NlsMsgIdWrite);
End;

Procedure Exit()
Begin
	Syscall DmbUnsubscribe("Nls","NlsMsgWrite");
End;

{ Service routine - periodically check logs for new messages } 
Procedure Service()
Begin
	var i : int;

	{ Call LogWritef(LogLvlTrace,"NlsService",0,0,0,0); }
	
	i = 0;
	While i < nlogs Do
	Begin
		Call CheckLogId(lognames + (i*32));
		i = i + 1;
	End;
End;

{ Command procedure - invoked to replicate log entries }
Procedure NlsMsgWrite(logname : string, flags : int, len : int, buf : opaque)
Begin
	var high, low : int;
	var hostidh, hostidl : int;

	Call LogWritef(LogLvlTrace,"NlsMsgWrite Logname=%s len=%u", logname, len,0,0);

	Syscall LogWrite(logname,flags,len,buf);
	Syscall LogLastId(logname,high,low);
	Call SetLogId(logname,high,low);
End;

End.



{ Local Variables: }
{ fvm-output-path: "/opt/fju/fvm/nls.fvm" }
{ End: }
