
{ -*- mode:fvm -*- }

Program TestProgram(0,0,TestProc,rpcbindResult,TestRpcCall);

Begin

Include "syscall.pas";
Include "string.pas";
Include "log.pas";

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

Procedure rpcbindResult(len : int, buf : opaque)
Begin
	Call LogWritef(LogLvlInfo,"rpcbindResult len=%u",len,0,0,0);
End;

Procedure TestRpcCall()
Begin
	var idh, idl : int;

	Syscall HostregIdByName("hemlock",idh,idl);
	Call LogWritef(LogLvlInfo,"rpcbind Hemlock=%x%x", idh,idl,0,0);
	
	Syscall RpcCall(idh,idl,100000,2,4,0,0,"rpcbindResult");
	
	Call LogWritef(LogLvlInfo,"rpcbindCall",0,0,0,0);
End;

End.		      

