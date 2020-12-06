
Program TestProgram(123,1,TestProc);

Begin

   { proc comment 1 }
Procedure testProc(a : u32)
Begin
   VAR a : u32;
var u	: u64;

u = 123L;
   
   a = 444 + 123 * 1 -1;
   a = 0x123;
      a >= 123;
   if a = 123 then
      b = "hello";
   else if a > 123 then
      b = "goodbye";
   end;

   a = "hello test string \n \x1234";
ENd;	 

Procedure Proc1(str : string, flags : u32, var result : u32)
Begin
   Syscall Log(flags,str);
   result = 123;
End;

End.		    
