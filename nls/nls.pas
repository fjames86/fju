{ -*- mode:fvm -*- }


{ 
  * This defines a module which does the following:
  * - Monitors a configurable set of logs (stored as keys in freg /fju/nls/logs)
  * - When a new entry is appended, issue a dmb command to run a command procedure.
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

Program Nls(0x2FFF7773,1,ProcNull,ProcList,Init,Exit,Service,NlsMsgWrite,List);
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
   
   Const NlsMsgIdWrite = (DmbCatNls + 0); { dmb message identifier } 

   { declarations }
   Record LogEntry =
   	  Name : string[MaxLogName];
	  IDHigh : int;
	  IDLow : int;
	  logh : int;
   End;

   Declare Procedure NlsMsgWrite(logname : string, flags : int, len : int, buf : opaque);
   
   { globals }
   var nlogs : u32;
   var logentries : LogEntry[MaxLog];

   { procedures }


{ ---------------- RPC interface -------------------- }

Procedure ProcNull(alen : int, abuf : opaque, var rlen : int, var rbuf : opaque)
Begin
	Call LogWritef(LogLvlTrace,"NlsProcNull",0,0,0,0);
End;

Procedure ProcList(alen : int, abuf : opaque, var rlen : int, var rbuf : opaque)
Begin
	var offset, i : int;
	var name : string;
	var p : ^LogEntry;
	var buf : opaque[256];

	Call LogWritef(LogLvlTrace,"NlsProcList",0,0,0,0);
	
	offset = 0;
	Call XdrEncodeU32(buf,offset,nlogs);
	i = 0;
	While i < nlogs Do Begin
	      p = LogEntries[i];
	      Call XdrEncodeString(buf,offset, p.Name);
	      Call XdrEncodeU64(buf,offset, p.IDHigh, p.IDLow);
	      i = i + 1;
	End;

	rbuf = buf;
	rlen = offset;
End;

Procedure List(var len : int, var buf : opaque)
Begin
	Call ProcList(0,0,len,buf);
End;

{ -------------------- Utility functions ------------------------- }

Procedure GetEntry(logname : string, var entryp : opaque)
Begin
	var i, result : int;
	var p : ^LogEntry;
	
	i = 0;
	While i < nlogs Do
	Begin
		p = LogEntries[i];
		Call Strcmp(p.Name, logname, result);
		If result Then
		Begin
			entryp = p;
			Return;
		End;
		i = i + 1;
	End;
	entryp = 0;
	Return;
End;


Procedure PublishCommand(logname : string, flags : int, len : int, buf : opaque)
Begin
	var argbuf : opaque[4096];
	var offset : int;
	var hosth, hostl : int;
	var seqH, seqL : int;
	
	Call LogWritef(LogLvlTrace,"NlsPublishCommand log=%s len=%u",logname, len,0,0);
	
	offset = 0;
	Call XdrEncodeString(argbuf,offset,logname);
	Call XdrEncodeU32(argbuf,offset,flags);
	Call XdrEncodeOpaque(argbuf,offset,len,buf);

	Syscall DmbPublish(NlsMsgIdWrite,DmbRemote,offset,argbuf,seqH,seqL);
End;

Procedure CheckLogId(entryp : opaque)
Begin
	var p : ^LogEntry;
	var idhigh, idlow : int;
	var high, low, pub : int;
	var len, flags : int;
	var buf : opaque[4096];

	p = entryp;
	
	Syscall LogLastId(p.logh,idhigh,idlow);
	high = p.IDHigh;
	low = p.IDLow;

	{ read entries until we get to the last one, if any }
	While (idhigh <> high) || (idlow <> low) Do
	Begin
		Syscall LogNext(p.logh,high,low,high,low);
		Syscall LogRead(p.logh,high,low,4096,buf,flags,len);
		If len Then Begin
		    Call LogWritef(LogLvlTrace,"Nls New Log entry log=%s %08x%08x",p.name,idhigh,idlow,0);

		    Call PublishCommand(p.name,flags,len,buf); 
		End Else Begin
		     high = idhigh;
		     low = idlow;
		End;
	End;
	
	p.IDHigh = idhigh;
	p.IDLow = idlow;
End;

{ -------------------------- Public procedures ----------------------- }

{ initialization routine - load log names from registry and set log ids }
Procedure Init()
Begin
	var ename : string[32];
	var i, etype, result : int;
	var idhigh, idlow : int;
	var p : ^LogEntry;
	var logh : int;
	var entry : FregEntry;
	var parentH, parentL : int;
	var idH, idL : int;
	
	Syscall FregEntryByName(0,0,0,"/fju/nls/logs",entry,result);
	parentH = entry.idH;
	parentL = entry.idL;
	idH = 0;
	idL = 0;
	i = 0;
	While result Do
	Begin
		Syscall FregNext(0,parentH,parentL,idH,idL,entry,result);
		If result && ((entry.Flags & FregTypeMask) = FregTypeString) Then
		Begin
			p = LogEntries[i];
			Call Strcpy(p.Name, entry.Name);
			Syscall LogOpen(p.Name,logh);
			Syscall LogLastId(logh,idhigh,idlow);
			p.IDHigh = idhigh;
			p.IDLow = idlow;
			p.logh = logh;
			i = i + 1;
			If i > MaxLog Then Break;
		End;
	End;
	nlogs = i;

	Syscall DmbSubscribe(&NlsMsgWrite,NlsMsgIdWrite,DmbFlagApply);
End;

Procedure Exit()
Begin
	Syscall DmbUnsubscribe(&NlsMsgWrite);
End;

{ Service routine - periodically check logs for new messages } 
Procedure Service()
Begin
	var i : int;
	var p : ^LogEntry;
	
	{ Call LogWritef(LogLvlTrace,"NlsService",0,0,0,0); }
	
	i = 0;
	While i < nlogs Do
	Begin
		p = LogEntries[i];
		Call CheckLogId(p);
		i = i + 1;
	End;
End;

{ Command procedure - invoked to replicate log entries }
Procedure NlsMsgWrite(logname : string, flags : int, len : int, buf : opaque)
Begin
	var p : ^LogEntry;
	var high, low : int;

	Call GetEntry(logname,p);
	If p Then
	Begin
	    Call LogWritef(LogLvlTrace,"NlsMsgWrite Logname=%s len=%u", logname, len,0,0);

	    Syscall LogWrite(p.logh,flags,len,buf);
	    Syscall LogLastId(p.logh,high,low);
	    p.IDHigh = high;
	    p.IDLow = low;
	End;
End;

End.



{ Local Variables: }
{ fvm-output-path: "/opt/fju/fvm/nls.fvm" }
{ End: }
