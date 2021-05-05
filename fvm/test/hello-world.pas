
{ -*- mode: fvm -*- }

Program HelloWorld(0,0,Hello,GetHello);
Begin

   Include "syscall.pas";

   Procedure Hello()
   Begin	
      Syscall Puts("Hello, World!\n");
   End;

   Procedure GetHello(var str : string)
   Begin
	str = "Hello, world!";
   End;
   
End.
