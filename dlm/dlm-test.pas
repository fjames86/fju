
{ -*- mode: fvm -*- }

Program DlmTest(0,0,Main);
Begin
   { Includes }
   Include "syscall.pas";
   Include "dlm.pas";

   { constants }

   { declarations }
      
   { globals }

   { procedures }
   Procedure Main()
   Begin
	Call DLM/Acquire(123,321,0,0,0);
   End;
   
   { constant values }

End.
