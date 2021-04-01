
{ -*- mode:fvm -*- }

Program TestProgram(0,0,TestProc,TestRpcCall);

Begin

Include "syscall.pas";
Include "string.pas";
Include "log.pas";
Include "xdr.pas";

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

Procedure sleeptest(private : int)
Begin
	Call LogWritef(LogLvlInfo,"sleeptest private=%u", private, 0,0,0);
End;

{ private rpc completion routine - receives rpccall result }
Procedure rpcbindResult(len : int, buf : opaque, private : int)
Begin
	var b, offset : int;
	var prog, vers, prot, port : int;
	
	Call LogWritef(LogLvlInfo,"rpcbindResult len=%u private=%u",len,private,0,0);

	offset = 0;
	Call XdrDecodeU32(buf,offset,b);
	While b && (offset < len) Do
	Begin
		Call XdrDecodeU32(buf,offset,prog);
		Call XdrDecodeU32(buf,offset,vers);
		Call XdrDecodeU32(buf,offset,prot);
		Call XdrDecodeU32(buf,offset,port);
		Call LogWritef(LogLvlInfo,"rpcbindResult prog=%u vers=%u prot=%u port=%u",
		     prog,vers,prot,port);

		Call XdrDecodeU32(buf,offset,b);
	End;

	Syscall Sleep(5000,&sleeptest,123);

End;

{ Tests calling rpc function on a remote machine }
Procedure TestRpcCall()
Begin
	var idh, idl : int;

	Syscall HostregIdByName("hemlock",idh,idl);
	Call LogWritef(LogLvlInfo,"rpcbind Hemlock=%x%x", idh,idl,0,0);

	{ call the rpcbind.list procedure, which takes no arguments }
	Syscall RpcCall(idh,idl,100000,2,4,0,0,&rpcbindResult,12);
	
	Call LogWritef(LogLvlInfo,"rpcbindCall sent",0,0,0,0);
End;

End.		      

