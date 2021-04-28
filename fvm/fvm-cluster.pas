
{ -*- mode: fvm -*- }

Program FvmCluster(0,0,Init,Exit,FvmClusterCommand,Run);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   Include "log.pas";
   Include "programs.pas";
   
   { constants }
   Const FvmClusterAppId = FvmRpcProg; 
   
   { declarations }
   Declare Procedure FvmClusterCommand(clidH : int, clidL : int, seqH : int, seqL : int, len : int, buf : opaque);
   
   { globals }
   var gclidH : int;
   var gclidL : int;
   
   { procedures }
   Procedure Init()
   Begin
	Syscall RaftAppRegister(FvmClusterAppId, "FvmCluster", &FvmClusterCommand, 0, 0);
	Syscall RaftClidByAppid(FvmClusterAppid, gclidH, gclidL); 
   End;

   Procedure Exit()
   Begin
	Syscall RaftAppUnregister(FvmClusterAppId);
   End;
	
   Procedure FvmClusterCommand(clidH : int, clidL : int, seqH : int, seqL : int, len : int, buf : opaque)
   Begin
	{ Run the specified procedure }
	var offset : int;
	var bufp : opaque[4096];
	var modname, procname : string[64];
	var lenp : int;
	
	offset = 0;
	Call XdrDecodeString(buf,offset,modname);
	Call XdrDecodeString(buf,offset,procname);
	Call XdrDecodeOpaque(buf,offset,lenp,bufp);
	Syscall FvmRun(modname,procname,lenp,bufp,0,0,0);
   End;

   Procedure Run(modname : string, procname : string, len : int, buf : opaque)
   Begin
	var bufp : opaque[4096];
	var offset : int;

	offset = 0;
	Call XdrEncodeString(bufp,offset,modname);
	Call XdrEncodeString(bufp,offset,procname);
	Call XdrEncodeOpaque(bufp,offset,len,buf);
	Syscall RaftCommand(gclidH, gclidL, offset, bufp);
   End;
   
   { constant values }

End.
