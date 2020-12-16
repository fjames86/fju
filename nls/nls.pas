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

Program Nls(0x2FFF7773,1,ProcNull,ProcList,Init,Service,Command);
Begin

   { includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   Include "log.pas";
   
   { constants }
   Const MaxLog = 8;
   
   { declarations }
   
   { globals }
   var nlogs : u32;
   var lognames : string[256]; { 32 byte name, 8 names max = 256 }
   var logids : u32[16];
   
{ procedures }
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
	var argbuf : opaque[1024];
	var offset : int;
	var hosth, hostl : int;

	Call LogWritef(LogLvlTrace,"NlsPublishCommand len=%u",len,0,0,0);
	
	Syscall HostregLocalid(hosth,hostl);

	offset = 0;
	Call XdrEncodeString(argbuf,offset,logname);
	Call XdrEncodeU64(argbuf,offset,hosth,hostl);
	Call XdrEncodeU32(argbuf,offset,flags);
	Call XdrEncodeOpaque(argbuf,offset,buf,len);
	
	Syscall FvmClRun(0,0,"Nls","Command",offset,argbuf);
End;

Procedure CheckLogId(logname : string)
Begin
	var idhigh, idlow : int;
	var high, low, pub : int;
	var len, flags : int;
	var buf : opaque[1024];
	
	Syscall LogLastId(logname,idhigh,idlow);
	Call GetLogId(logname,high,low);
	
	{ if new message appeneded then issue command }
	If (idhigh <> high) || (idlow <> low) Then
	Begin
		Call LogWritef(LogLvlTrace,"Nls New Log entry %x%x",idhigh,idlow,0,0);
		
		Syscall LogRead(logname,idhigh,idlow,1024,buf,flags,len);
		If len Then Call PublishCommand(logname,flags,len,buf);
		Call SetLogId(logname,idhigh,idlow);
	End;
	

End;

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
End;

Procedure Service()
Begin
	var i : int;

	Call LogWritef(LogLvlTrace,"NlsService",0,0,0,0);
	
	i = 0;
	While i < nlogs Do
	Begin
		Call CheckLogId(lognames + (i*32));
		i = i + 1;
	End;
End;

{ Command procedure - invoked to replicate log entries }
Procedure Command(logname : string, hosth : int, hostl : int, flags : int, len : int, buf : opaque)
Begin
	var high, low : int;
	var hostidh, hostidl : int;

	Call LogWritef(LogLvlTrace,"NlsCommand Hostid=%x%x Logname=%s len=%u", hosth, hostl, logname, len);
	
	Syscall HostregLocalId(hostidh, hostidl);
	If (hostidh = hosth) && (hostidl = hostl) Then Return;
	
	Syscall LogWrite(logname,flags,len,buf);
	Syscall LogLastId(logname,high,low);
	Call SetLogId(logname,high,low);
End;




End.


