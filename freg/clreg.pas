
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
   Include "log.pas";
   
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

     Call LogWritef(LogLvlTrace,"ClReg Command len=%d",len,0,0,0);
     
     offset = 0;
     Call XdrDecodeU32(buf,offset,cmd);
     Call XdrDecodeString(buf,offset,path);
     If cmd = CmdPut Then
     Begin	
	Call XdrDecodeU32(buf,offset,flags);
	Call XdrDecodeU32(buf,offset,lenp);

	Call LogWritef(LogLvlTrace,"ClReg Command Put %s Flags=%x len=%d", path,flags,lenp,0);
	If (offset + lenp) <= len Then Begin
	   Syscall FregPut(regh,0,0,path,flags,lenp,buf + offset,0,0);
	End Else Begin
	    Call LogWritef(LogLvlError,"ClReg Command Put short buffer",0,0,0,0);
	End;
	
     End
     Else If cmd = CmdSubkey Then
     Begin
	Call LogWritef(LogLvlTrace,"ClReg Command Subkey %s", path,0,0,0);
	
	Syscall FregSubkey(regh,0,0,path,0,0);
     End
     Else If cmd = CmdRem Then
     Begin
	Call LogWritef(LogLvlTrace,"ClReg Command Rem %s", path,0,0,0);
	
	Syscall FregEntryByName(regh,0,0,path,entry,result);
	If result Then Begin
	   Syscall FregRem(regh,entry.idH,entry.idL,result);
	End Else Begin
	   Call LogWritef(LogLvlError,"ClReg Command Rem %s failed",path,0,0,0);
	End;
     End
     Else Begin
     	  Call LogWritef(LogLvlError,"ClReg Command unknown command %d path %s", cmd,path,0,0);
     End;
     
   End;

   Procedure Snapsave(clidH : int, clidL : int, termH : int, termL : int, seqH : int, seqL : int)
   Begin
	{ TODO }

	{ walk the entire tree, appending a command to the snapshot for each value found }
	Call LogWritef(LogLvlTrace,"ClReg Snapsave",0,0,0,0);
   End;

   Procedure Snapload(clidH : int, clidL : int, len : int, buf : opaque)
   Begin
	{ TODO }

	{ the snapshot consists of a list of entries, each of which is a standard command buffer }
	Call LogWritef(LogLvlTrace,"ClReg Snapload",0,0,0,0);
   End;
   
   Procedure Init()
   Begin
	Syscall FregOpen("clreg",regh);
	Syscall RaftAppRegister(ClRegAppID,"ClReg",&Command, &Snapsave, &SnapLoad);
	Syscall RaftClidByAppid(ClRegAppID,regclidH,regclidL);
	If (regclidH = 0) && (regclidL = 0) Then Begin
	   Call LogWritef(LogLvlError,"ClReg no cluster found with appid=%u", ClRegAppID,0,0,0);
	End;
   End;

   Procedure Exit()
   Begin
	Syscall FregClose(regh);
	Syscall RaftAppUnregister(ClRegAppID);
   End;

   Procedure Put(path : string, flags : int, len : int, buf : opaque)
   Begin
	var offset, off : int;
	var u32 : int;
	var u64h, u64l : int;
	var bufp : opaque[4096];

	offset = 0;
	off = 0;
	Call XdrEncodeU32(bufp,offset,CmdPut);
	Call XdrEncodeString(bufp,offset,path);
	Call XdrEncodeU32(bufp,offset,flags);
	If (flags & FregTypeMask) = FregTypeU32 Then Begin
	   Call XdrDecodeU32(buf,off,u32);
	   Call XdrEncodeOpaque(bufp,offset,4,&u32);
	End Else If (flags & FregTypeMask) = FregTypeU64 Then Begin
	    Call XdrDecodeU64(buf,off,u64h,u64l);
	    Call XdrEncodeOpaque(bufp,offset,8,&u64h);
	End Else If (flags & FregTypeMask) = FregTypeString Then Begin
	    Call XdrDecodeU32(buf,off,u32);
	    Call XdrEncodeOpaque(bufp,offset,u32,buf + 4);
	End Else 
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
