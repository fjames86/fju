
Program TestReg(111111,1,Main);
Begin

Procedure Main(	   argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque)
Begin
   var u :  integer;
var str	:  string[64];
   
      Syscall ReadRegInt("/testint", u);
   u := u + 1;
      Syscall WriteRegInt("/testint", u);

Syscall ReadRegString("/teststr", str, 64 );
   Syscall Puts(str);
   Syscall WriteRegString("/teststr", "my new value" );
End;
   
End.
