
{ -*- mode: fvm -*- }

Program HelloWorld(0,0,Hello,GetHello,GetInt,PutHello);
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

   Procedure PutHello(str : string, y : int, var x : int, l : int, op : opaque)
   Begin
	Syscall Puts(str);
	x = y + l;
   End;
   
End.
