
{ -*- mode: fvm -*- }

Program HelloWorld(0,0,Hello,GetHello,GetInt);
Begin

   Include "syscall.pas";

   const xx = -12;
   Procedure Hello()
   Begin
      Syscall Puts("Hello, World!\n");
   End;

   Procedure GetHello(var str : string)
   Begin
	str = "Hello, world!";
   End;

   Procedure GetInt(var r : int)
   Begin
	var x : int;
	x = 2;
	x = x + 1;
	r = xx;
   End;
	
End.
