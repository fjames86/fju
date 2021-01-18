

{ -*- mode: fvm -*- }

Program DmbTest(0,0,Init,Exit,MsgHandler,TestMsg,MsgHandler2,TestMsg2);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   Include "xdr.pas";
   
   { constants }

   { declarations }
      
   { globals }

   { procedures }

   { Initialization routine - register a dmb message handler }
   Procedure Init()
   Begin
	{ Register to receive all messages }
	Syscall DmbSubscribe("DmbTest","MsgHandler",0);
	Syscall DmbSubscribe("DmbTest","MsgHandler2",0x3);
   End;

   { Exit routine - called when module unloaded }
   Procedure Exit()
   Begin
	Syscall DmbUnsubscribe("DmbTest","MsgHandle");
	Syscall DmbUnsubscribe("DmbTest","MsgHandler2");
   End;
   
   { Message handler }
   Procedure MsgHandler(len : int, buf : opaque)
   Begin
	var hostH, hostL, seqH, seqL, msgid : int;
	var lenp : int;
	
	Syscall DmbMsgInfo(hostH,hostL,seqH,seqL,msgid,lenp);
	Call LogWritef(LogLvlInfo,"DmbTest Host %x%x Msgid %x Len %u", hostH, hostL, msgid, len);

	If msgid = 0x00000001 Then Begin
	   Call LogWritef(LogLvlInfo, "DmbTest Sending reply message", 0,0,0,0 );
	   Syscall DmbPublish(0x00000002, DmbRemote, 0, 0, seqH, seqL);
	End Else If msgid = 0x00000002 Then Begin
	   { Write an freg entry }
	   Syscall FregWriteString("/dmbtest","DmbTest Received Reply");
	End;

   End;

   Procedure TestMsg()
   Begin
	var seqH, seqL : int;
	
	{ Publish a message to remote hosts }
	Call LogWritef(LogLvlInfo,"DmbTest Send test message",0,0,0,0);

	{ Clear an freg entry }
	Syscall FregWriteString("/dmbtest","");
	
	Syscall DmbPublish(0x00000001,DmbRemote,0,0,seqH, seqL);
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

	Syscall DmbPublish(0x3,0,offset,buf,seqH,seqL);
   End;
End.
