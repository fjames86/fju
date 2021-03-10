
{ -*- mode: fvm -*- }

Program TestRecords(0,0,TestProc);
Begin
   { Includes }
   Include "syscall.pas";

   { constants }

   { declarations }
   Const clen = 12;
   
   Record fred =
   	  a : int;
	  b : string;
	  c : int[clen];
	  d : opaque[3];
    End;
	  
   { globals }

   { procedures }
   Procedure TestProc()
   Begin

   End;

   { constant values }

End.
