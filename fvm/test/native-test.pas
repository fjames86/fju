
{ -*- mode: fvm -*- }

Program NativeTest(0,0,Main);
Begin

   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   
   { constants }

   { declarations }

   { native system call implementation }
   Declare Syscall NativeTest(var x : int) : 1000;   

   { cross call to native module }
   Declare Procedure Native/ProcNull();
   Declare Procedure Native/TestProc(x : int, var y : int);   
   
   { globals }

   { procedures }
   Procedure Main()
   Begin
	var x : int;
	
	Syscall NativeTest(x);
	Call LogWritef(LogLvlInfo,"NativeTest: syscall x = %u should be 321", x,0,0,0);

	Call Native/ProcNull();

	Call Native/TestProc(123,x);
	Call LogWritef(LogLvlInfo,"NativeTest: x = %u should be 124", x,0,0,0);
   End;

   { constant values }

End.
