
{ -*- mode:fvm -*- }

Program TestProgram(0,0,TestProc);

Begin

Include "syscall.pas";

Declare Procedure TestProc2(var a :  u32);

Procedure TestProc(a : u32, var out : string )
Begin
   out = "Hello world";
End;

Procedure TestProc2(var a :  u32)
Begin
   Syscall LogWrite(0,LogBinary,14,"hello, world!");
   a = a ? a + 1 : a - 1;
End;	   

Procedure Test3()
Begin
	var a, b : int;
	b = &a;
	a = *b;
	a = 123 * 321 & 321;
End;

End.		      

