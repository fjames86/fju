

{ -*- mode: fvm -*- }

Program DmbTest(0,0,Init,Exit,MsgHandler,TestMsg,MsgHandler2,TestMsg2);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   Include "xdr.pas";
   
   { constants }
   Const MsgPing = 0x1;
   Const MsgPong = 0x2;
   Const MsgLogVals = 0x3;
   
   { declarations }
   Declare Procedure MsgHandler(msgid : int, len : int, buf : opaque);
   Declare Procedure MsgHandler2(intval : int, str : string);
   
   { globals }

   { procedures }

   { Initialization routine - register a dmb message handler }
   Procedure Init()
   Begin
	{ Register to receive all messages }
	Syscall DmbSubscribe(&MsgHandler,0,DmbFlagRaw);
	Syscall DmbSubscribe(&MsgHandler2,MsgLogVals,DmbFlagApply);
   End;

   { Exit routine - called when module unloaded }
   Procedure Exit()
   Begin
	Syscall DmbUnsubscribe(&MsgHandler);
	Syscall DmbUnsubscribe(&MsgHandler2);
   End;
   
   { Message handler }
   Procedure MsgHandler(msgid : int, len : int, buf : opaque)
   Begin
	var hostH, hostL, seqH, seqL : int;
	var timestr : string[64];
	var valstr : string[128];
	var nowh, nowl : int;
	var vallen : int;
	
{
	Syscall DmbMsgInfo(hostH,hostL,seqH,seqL);
	Call LogWritef(LogLvlInfo,"DmbTest Host %x%x Msgid %x Len %u", hostH, hostL, msgid, len);
}

	If msgid = MsgPing Then Begin
	   Call LogWritef(LogLvlInfo, "DmbTest Sending reply message", 0,0,0,0 );
	   Syscall DmbPublish(MsgPong, DmbRemote, 0, 0, seqH, seqL);
	End Else If msgid = MsgPong Then Begin
	   { Write an freg entry }
	   Syscall TimeNow(nowh,nowl);
	   Syscall Timestr(nowh,nowl,timestr);
	   Syscall Sprintf(valstr,"DmbTest Received reply %s", timestr,0,0,0);
	   Call Strlen(valstr,vallen);
	   Syscall FregPut(0,0,0,"/dmbtest",FregTypeString,vallen,valstr,0,0);
	End;

   End;

   Procedure TestMsg()
   Begin
	var seqH, seqL : int;
	
	{ Publish a message to remote hosts }
	Call LogWritef(LogLvlInfo,"DmbTest Send test message",0,0,0,0);

	{ Clear an freg entry }
	Syscall FregPut(0,0,0,"/dmbtest",FregTypeString,1,"",0,0);	
	
	Syscall DmbPublish(MsgPing,DmbRemote,0,0,seqH, seqL);
   End;

   Procedure MsgHandler2(intval : int, str : string)
   Begin
	Call LogWritef(LogLvlInfo,"DmbTest2 intval=%u strval=%s",intval,str,0,0);
   End;

   Procedure TestMsg2()
   Begin
	var intval : int;
	var strval : string;
	var offset : int;
	var buf : opaque[64];
	var bufp : opaque;
	var seqH, seqL : int;
	
	intval = 123;
	strval = "hello my test msg";
	offset = 0;
	bufp = buf;
	Call XdrEncodeU32(bufp,offset,intval);
	Call XdrEncodeString(bufp,offset,strval);

	Syscall DmbPublish(MsgLogVals,0,offset,buf,seqH,seqL);
   End;
End.
