
{ -*- mode: fvm -*- }

Program HelloWorld(0,0,Hello);
Begin

   Include "syscall.pas";

   Procedure Hello()
   Begin	
      Syscall Puts("Hello, World!\n");
   End;
   
End.
