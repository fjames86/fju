
Program TestProgram(123,1,TestProc);

Begin

   Declare Procedure TestProc2(var a :  u32);
   Declare const fred : u32; 
      
Procedure TestProc(init		     : u32,  var b : u32 )
Begin
   var a : u32;

   a = init;
   Call TestProc2(a);
   b = a;

   if b = 12 then
      begin
      b = b + 1;
	 b = 123;
      end
   else
      b = -123;

   do begin
      a = a + 1;
   end while a < 10;
   
   a = fred;
   
End;

Procedure TestProc2(var a :  u32)
Begin
   var str :  string[64];
      
   a = 123;
End;	   

const var fred = 123;
   
End.		      

