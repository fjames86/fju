

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
   
   Procedure TestProc(hostH : int, hostL : int, seqH : int, seqL : int, msgid : int, flags : int, len : int, buf : opaque)
   Begin
	Call LogWritef(LogLvlInfo,"DmbTest Host %x%x Seq %x%x", hostH, hostL, seqH, seqL);
	Call LogWritef(LogLvlInfo,"DmbTest Msgid %u Flags %x", msgid, flags, 0, 0 );
   End;


End.
