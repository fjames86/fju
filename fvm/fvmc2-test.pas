
Program TestProgram(123,1,TestProc);

Begin

   { declarations }
   Include "syscall.pas";

   
   Declare Procedure TestProc2(var a :  u32);
   Declare const var fred : u32; 

   { globals }
   var mystr : string[64];

   { procedures }
      
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

   a = 0;
   while a < 10 do
      begin
	 a = a + 1;
	 if a = 5 then goto done;
      end;

   done:
   a = fred;
   
End;

Procedure TestProc2(var a :  u32)
Begin
   var str :  string[64];
      
   a = 123;
End;	   

const var fred = 123;
   
End.		      

