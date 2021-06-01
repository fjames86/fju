
{ -*- mode: fvm -*- }

{
 * Attempt at a clustered freg 
 * - all writes are done though raft commands
 * - reads can be serviced locally using standard apis? 
}

Program ClReg(0,0,Init,Exit,Command,Snapsave,Snapload,Put,Subkey,Rem);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   
   { constants }
   Const ClRegAppID = 123212;
   Const CmdPut = 1;
   Const CmdSubkey = 2;
   Const CmdRem = 3;
   
   { declarations }
	  	  
   { globals }
   var regh : int;
   var regclidH, regclidL : int;
   
   { procedures }
   Procedure Command(clidH : int,clidL : int,seqH : int,seqL : int,len : int,buf : opaque)
   Begin
     var cmd : int;
     var offset : int;	
     var path : string[1024];
     var entry : FregEntry;
     var lenp, flags, result : int;
     
     offset = 0;
     Call XdrDecodeU32(buf,offset,cmd);
     Call XdrDecodeString(buf,offset,path);
     If cmd = CmdPut Then
     Begin
	Call XdrDecodeU32(buf,offset,flags);
	Call XdrDecodeU32(buf,offset,lenp);	
	Syscall FregPut(regh,0,0,path,flags,lenp,buf + offset,0,0);
     End
     Else If cmd = CmdSubkey Then
     Begin
	Syscall FregSubkey(regh,0,0,path,0,0);
     End
     Else If cmd = CmdRem Then
     Begin
	Syscall FregEntryByName(regh,0,0,path,entry,result);
	If result Then
	   Syscall FregRem(regh,entry.idH,entry.idL,result);
     End;
     
   End;

   Procedure Snapsave(clidH : int, clidL : int, termH : int, termL : int, seqH : int, seqL : int)
   Begin
	{ TODO }

	{ walk the entire tree, appending a command to the snapshot for each value found }
   End;

   Procedure Snapload(clidH : int, clidL : int, len : int, buf : opaque)
   Begin
	{ TODO }

	{ the snapshot consists of a list of entries, each of which is a standard command buffer }
   End;
   
   Procedure Init()
   Begin
	Syscall FregOpen("clreg",regh);
	Syscall RaftAppRegister(ClRegAppID,"ClReg",&Command, &Snapsave, &SnapLoad);
	Syscall RaftClidByAppid(ClRegAppID,regclidH,regclidL);
   End;

   Procedure Exit()
   Begin
	Syscall FregClose(regh);
	Syscall RaftAppUnregister(ClRegAppID);
   End;

   Procedure Put(path : string, flags : int, len : int, buf : opaque)
   Begin
	var offset : int;
	var bufp : opaque[4096];

	offset = 0;
	Call XdrEncodeU32(bufp,offset,CmdPut);
	Call XdrEncodeString(bufp,offset,path);
	Call XdrEncodeU32(bufp,offset,flags);
	Call XdrEncodeOpaque(bufp,offset,len,buf);
	
	Syscall RaftCommand(regclidH,regclidL,offset,bufp);
   End;

   Procedure Rem(path : string)
   Begin
	var offset : int;
	var bufp : opaque[4096];

	offset = 0;
	Call XdrEncodeU32(bufp,offset,CmdRem);
	Call XdrEncodeString(bufp,offset,path);
	
	Syscall RaftCommand(regclidH,regclidL,offset,bufp);
   End;

   Procedure Subkey(path : string)
   Begin
	var offset : int;
	var bufp : opaque[4096];

	offset = 0;
	Call XdrEncodeU32(bufp,offset,CmdSubkey);
	Call XdrEncodeString(bufp,offset,path);
	
	Syscall RaftCommand(regclidH,regclidL,offset,bufp);
   End;


   { constant values }

End.
