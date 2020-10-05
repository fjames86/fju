
{ -*- text -*- }

Program MsgQ(23102222, 1, Service,Publish,Subscribe);
Begin

Const MsgQLogName := "msgq";

var LogIDLow : integer;
var LogIDHigh : integer;

var nSubs : integer;
var SubsProgid : opaque[64];
var SubsProcid : opaque[64];
var SubsMsgid : opaque[64];

Procedure PublishMsg(msgid : integer, buf : opaque, count : integer)
Begin
	var i, progid, procid, c : integer;
	var q : opaque;

	i := 0;
	While i < nSubs Do
	Begin
		q := SubsMsgid + (i*4);
		If msgid = ^q Then
		Begin
			c := 0;
			q := SubsProgid + 4*i;
			progid := ^q;
			q := SubsProcid + 4*i;
			procid := ^q;
			Syscall Invoke(progid, procid, buf, count, 0, c);
		End;		   
	End;

End;

Procedure Service(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var found : integer;
	var buf : opaque[64];
	var count : integer;
 	var q : opaque; 
	var msgid : integer;
	
	{ read from the msg log }
	Do 
	Begin
		Syscall NextLogEntry(MsgQLogName, LogIDHigh, LogIDLow, found);
		If found Then
		Begin
			count := 64;
			Syscall ReadLog(MsgQLogName, LogIDHigh, LogIDLow, buf, count);
			{ deliver message to subscribers }

			If count < 4 Then
			Begin
			   Syscall LogStr("Bad log entry");
			End
			Else
			Begin
			   q := buf;
			   msgid := ^q;
			   Call PublishMsg(msgid, buf + 4, count - 4 );
			End;
						
		End;
	End
	While 1;

End;

{ write message into queue }
Procedure Publish(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	If argcount < 4 Then
	   Syscall LogStr("Bad log entry")
	Else
	   Syscall WriteLog(MsgQLOgName, argbuf, argcount);
	
End;

{ listen for messages }
Procedure Subscribe(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var msgid, progid, procid : integer;
	var i : integer;
	var p : opaque;

	If nSubs < 16 Then
	Begin
		p := argbuf;		
		msgid := ^p;
		p := p + 4;
		progid := ^p;
		p := p + 4;
		procid := ^p;

		p := SubsMsgId + nSubs*4;
		^p := msgid;
		p := SubsProgid + nSubs*4;
		^p := progid;
		p := SubsProcid + nSubs*4;
		^p := procid;
		
		nSubs := nSubs + 1;
	End;
	
End;

End.

