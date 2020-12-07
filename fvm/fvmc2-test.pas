
Program TestProgram(123,1,TestProc);

Begin

   var global1 : u32;
var u32arry :  u32[12];
var str	:  string;
var p :  opaque;
   
   { proc comment 1 }

const var conststr = "hello";
   
Procedure testProc(a : u32, var b : string)
Begin
   VAR a : u32;
var u	: u64;
var opp	:  opaque[64];


u = 123;

a = 444 + 123 * 1 - 1;

   if a = 123 then
      b = "hello"
   else if a > 123 then
      b = "goodbye";
   
Goto label;
   
# single line comment 
   a = "hello test string \n \x1234 \"ggg\"";

label:
ENd;	 

Procedure Proc1(str : string, flags : u32, var result : u32)
Begin
   Syscall Log(flags,str);
   result = 123;
End;

const var xxx = 123;
   
End.

