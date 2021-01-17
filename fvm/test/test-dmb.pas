

{ -*- mode: fvm -*- }

Program DmbTest(0,0,Init,Exit,MsgHandler,TestMsg);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   
   { constants }

   { declarations }
      
   { globals }

   { procedures }

   { Initialization routine - register a dmb message handler }
   Procedure Init()
   Begin
	{ Register to receive all messages }
	Syscall DmbSubscribe("DmbTest","MsgHandler",0);
   End;

   { Exit routine - called when module unloaded }
   Procedure Exit()
   Begin
	Syscall DmbUnsubscribe("DmbTest","MsgHandle");
   End;
   
   { Message handler }
   Procedure MsgHandler(hostH : int, hostL : int, seqH : int, seqL : int, msgid : int, len : int, buf : opaque)
   Begin
	var seqH, seqL : int;
	
	Call LogWritef(LogLvlInfo,"DmbTest Host %x%x Msgid %x Len %u", hostH, hostL, msgid, len);

	If msgid = 0x00010001 Then Begin
	   Call LogWritef(LogLvlInfo, "DmbTest Sending reply message", 0,0,0,0 );
	   Syscall DmbPublish(0x00010002, DmbRemote, 0, 0, seqH, seqL);
	End Else If msgid = 0x00010002 Then Begin
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
	
	Syscall DmbPublish(0x00010001,DmbRemote,0,0,seqH, seqL);
   End;

End.
