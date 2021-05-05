
{ -*- mode: fvm -*- }

Program TestXCall(0,0,Main);
Begin
   { Includes }
   Include "syscall.pas";

   { constants }

   { declarations }
   Declare Procedure HelloWorld/GetHello(var str : string);
   
   { globals }

   { procedures }
   Procedure Main()
   Begin
	var str : string;
	Call HelloWorld/GetHello(str);
	Syscall Puts(str);
   End;

   { constant values }

End.
