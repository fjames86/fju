
{ -*- text -*- }

{
 This shows how to implement a simple program that prints a string 
}

Program test6(123123,1,Main);
Begin

Declare Procedure Proc1(op : opaque);

Procedure Main(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque)
Begin
   Syscall Puts("hello world\n");
   Call Proc1(0);
End;

Declare var mymissingglob : integer;

var mystring : string;

Procedure Proc1(op :  opaque)
Begin		   
   var i : integer;
   var mystr : string[64];
   var args : opaque[8];
   var argp : opaque;
   
   i := ^op;
   i := mymissingglob;
   mystring := 123;

   argp := args;
   ^argp := 123;
   argp := argp + 4;
   ^argp := 321;
   Syscall sprintf( mystr, 64, "fred %u:%u\n", args );

   Syscall Puts(mystr);
End;	 

var mymissingglob : integer;

End.
