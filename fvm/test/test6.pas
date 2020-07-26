
{ -*- text -*- }

{
 This shows how to implement a simple program that prints a string 
}

Program test6(123123,1,Main);
Begin

Procedure Main(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque)
Begin
   Syscall Puts("hello world");
End;

Declare var mymissingglob : integer;

Procedure Proc1(op :  opaque)
Begin		   
   var i : integer;
   i := ^op;
   i := mymissingglob;
      
End;	 

var mymissingglob : integer;

End.
