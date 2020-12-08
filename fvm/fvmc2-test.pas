
Program TestProgram(123,1,TestProc);

Begin

   Declare Procedure TestProc2(var a :  u32);
   
Procedure TestProc(init		     : u32,  var b : u32 )
Begin
   var a : u32;

   a = init;
   Call TestProc2(a);
   b = a;
End;

Procedure TestProc2(var a :  u32)
Begin
   a = 123;
End;

End.		      

