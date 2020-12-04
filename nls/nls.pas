
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

const MaxLog := 8;
const MaxLogId := 64; { 8 * MaxLog }
const MaxLogName := 512; { MaxLog * 64 }

var nlogs : integer;
var msgids : opaque[MaxLog];
var lognames : opaque[MaxLogName];

Procedure NlsProcNull(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	rescount := 0;
	resbuf := 0;
End;

Procedure NlsProcList(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var res : opaque[512];
	var resbufp : opaque;
	var i : integer;
	
	resbufp := res;
	
	Call EncodeInteger(resbufp, nlogs);
	i := 0;
	Do Begin
	   Call EncodeString(resbufp,lognames + i*64);
	   i := i + 1;
 	End While i < nlogs;
	
	resbuf := res;
	rescount := resbufp - res;
End;

Procedure GetLogId(logname : string, var logidHigh : integer, var logidLow : integer)
Begin
	var p : opaque;
	var i, result : integer;

	logidlow := 0;
	logidHigh := 0;

	p := lognames;
	i := 0;
	Do Begin
		Syscall Strcmp(p, logname, result);
		If result = 0 Then
		Begin
			{ Found the log, get its current id }
			p := msgids;
			p := p + (i*8);
			logidHigh := ^p;
			p := p + 4;
			logidLow := ^p;
			Return;
		End;

		i := i + 1;
		p := p + (64*i);
	End While i < nlogs;
	
End;

Procedure SetLogId(logname : string, logidHigh : integer, logidLow : integer)
Begin
	var p : opaque;
	var i, result : integer;

	p := lognames;
	i := 0;
	Do Begin
		Syscall Strcmp(p, logname, result);
		If result = 0 Then
		Begin
			{ Found the log, get its current id }
			p := msgids;
			p := p + (i*8);
			^p := logidHigh;
			p := p + 4;
			^p := logidLow;
			Return;
		End;

		i := i + 1;
		p := p + (64*i);
	End While i < nlogs;
	
End;

Procedure PublishCommand(logname : string, idh : integer, idl : integer)
Begin
    var p, q : opaque;
    var buf : opaque[1024];
    var i, offset, len, count : integer;
    var hostidlow, hostidhigh : integer;
    var logstr : string[256];
    var logstrargs : opaque[12];

    p := logstrargs;
    ^p := logname;
    p := p + 4;
    ^p := idh;
    p := p + 4;
    ^p := idl;
    Syscall Sprintf(logstr,256,"Nls Publish %s %08x%08x", logstrargs);
    Syscall LogDebug(logstr);
    
    p := buf;
    offset := 0;
				
    Syscall LocalId(hostidhigh, hostidlow);
    ^p := hostidhigh;
    p := p + 4;
    offset := offset + 4;
    ^p := hostidlow;
    p := p + 4;
    offset := offset + 4;

    len := ^logname;
    ^p := len;
    p := p + 4;
    offset := offset + 4;

    q := logname + 4;
    i := 0;
    Do Begin
       ^p := ^q;
	p := p + 4;
	q := q + 4;
	offset := offset + 4;
	i := i + 4;
    End While i < len;
				
    count := 512;
    Syscall ReadLog(logname, idh, idl, p + 4, count);
    ^p := count;
    offset := offset + 4 + count;

    { procid=4 is NlsCommand procedure }
    Syscall FvmClusterRun(10001, 4, buf, offset);

End;

Procedure CheckLogId(logname : string)
Begin
	var idlow, idhigh : integer;
	var idl, idh, result, count : integer;
	var buf : opaque[512];
	var hostidlow, hostidhigh : integer;
	var len, offset : integer;
	var p, q : opaque;
	var i : integer;
	
	Syscall LogProp(logname, idhigh, idlow);
	Call GetLogId(logname, idh, idl);

	
	If (Not (idhigh And idh)) And (Not (idlow And idl)) Then
	Begin
		{ read entries }
		Do Begin
			Syscall NextLogEntry(logname, idh, idl, result);
			If result <> 0 Then Call PublishCommand(logname, idh, idl);
		End While result <> 0;

		Call SetLogId(logname, idhigh, idlow);	
	End;

End;

{ initialization routine - load log names from registry and set log ids }
Procedure NlsInit(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var name : string;
	var type : integer;
	var i : integer;
	var idh, idl : integer;
	
	i := 0;
	Do Begin
	   name := lognames + (i*64);
	   Syscall NextRegEntry("/fju/nls/logs", name, type);
	   If type <> 0 Then
	   Begin
		nlogs := nlogs + 1;
		Syscall LogProp( name, idh, idl);
		Call SetLogId(name,idh,idl);
	   End;

	   i := i + 1;
	   If i >= MaxLog Then Return;
	End While i < MaxLog;
End;

Procedure NlsService(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var i : integer;
	var logname : string;
	
	i := 0;
	Do Begin
	   logname := lognames + (i*64);
	   Call CheckLogId(logname);
	   
	   i := i + 1;
	End While i < nlogs;
End;

{ Command procedure - invoked to replicate log entries }
Procedure NlsCommand(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	{ command: hostidhigh, hostidlow, logname, buffer }
	var idh, idl, hostidh, hostidl : integer;
	var p : opaque;
	var len : integer;
	var logname : string;
	
	p := argbuf;
	idh := ^p;
	p := p + 4;
	idl := ^p;
	p := p + 4;
	logname := p;
	len := ^p;
	If (len % 4) <> 0 Then len := len + 4 - (len % 4);
	p := p + 4 + len;
	len := ^p;
	p := p + 4;

	Syscall LocalID(hostidh, hostidl);
	If (Not (hostidh And idh)) And (Not (hostidl And idl)) Then
	Begin
		Syscall WriteLog(logname, p, len);
		Syscall LogProp(logname, idh, idl);
		Call SetLogId(logname, idh, idl);
	End;

End;




End.


