

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
   
   Procedure TestProc(hostH : int, hostL : int, msgid : int, flags : int, len : int, buf : opaque)
   Begin
	Call LogWritef(LogLvlInfo,"DmbTest Host %x%x Msgid %x Flags %x", hostH, hostL, msgid, flags);
   End;


End.
