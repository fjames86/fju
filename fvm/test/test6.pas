
Program test6(123123,1,Main);
Begin

Declare Syscall Puts(str : string) : 1;
   
Procedure Main(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque)
Begin
   Syscall Puts("hello world");
End;

   
End.
