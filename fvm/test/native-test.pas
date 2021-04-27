
{ -*- mode: fvm -*- }

Program NativeTest(0,0,Main);
Begin

   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   
   { constants }

   { declarations }
   Declare Syscall NativeTest(var x : int) : 1000;
   Declare Procedure Native/ProcNull();
   
   { globals }

   { procedures }
   Procedure Main()
   Begin
	var x : int;
	
	Syscall NativeTest(x);
	Call LogWritef(LogLvlInfo,"NativeTest: x = %u", x,0,0,0);

	Call Native/ProcNull();
   End;

   { constant values }

End.
