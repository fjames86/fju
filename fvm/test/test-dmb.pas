

{ -*- mode: fvm -*- }

Program DmbTest(0,0,Init,TestProc);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   
   { constants }

   { declarations }
      
   { globals }

   { procedures }
   Procedure Init()
   Begin
	Syscall DmbSubscribe("DmbTest","TestProc",0);
   End;

   { message handler that just logs it } 
   Procedure TestProc(hostH : int, hostL : int, msgid : int, len : int, buf : opaque)
   Begin
	Call LogWritef(LogLvlInfo,"DmbTest Host %x%x Msgid %x Len %u", hostH, hostL, msgid, len);

	If msgid = 0x00010001 Then Begin
	   Call LogWritef(LogLvlInfo, "DmbTest Sending reply message", 0,0,0,0 );
	   Syscall DmbPublish(0x00010002, DmbRemote, 0, 0);
	End;

   End;


End.
