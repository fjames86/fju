
{ -*- mode: fvm -*- }

{
 * Attempt at a clustered freg 
 * - all writes are done though raft commands
 * - reads can be serviced locally using standard apis? 
}

Program ClReg(0,0,Init,Exit,Command,Snapsave,Snapload,Put,Subkey,Rem,Get);
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
   Record SnapInfo = 
   	  clidH : int;
	  clidL : int;
	  termH : int;
	  termL : int;
	  seqH : int;
	  seqL : int;
	  snapOffset : int;
   End;
   

   { globals }
   var regh : int;
   var regclidH, regclidL : int;
   
   { procedures }
   Procedure DecodeCommand(buf : opaque, var offset : int, var cmd : int, path : string, var flags : int, var lenp : int, var bufp : opaque)
   Begin
	Call XdrDecodeU32(buf,offset,cmd);
	Call XdrDecodeString(buf,offset,path);
	If cmd = CmdPut Then
	Begin
		Call XdrDecodeU32(buf,offset,flags);
		Call XdrdecodeU32(buf,offset,lenp);
		bufp = buf + offset;
		offset = offset + lenp;
		If lenp % 4 Then offset = offset + 4 - (lenp % 4);
	End;
		
   End;

   Procedure ApplyCommand(cmd : int, path : string, flags : int, lenp : int, bufp : opaque)
   Begin
	var result : int;
        var entry : FregEntry;
	
	If cmd = CmdPut Then
        Begin	
	   Call LogWritef(LogLvlTrace,"ClReg Command Put %s Flags=%x len=%d", path,flags,lenp,0);
	   Syscall FregPut(regh,0,0,path,flags,lenp,bufp,0,0);
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
   
   Procedure Command(clidH : int,clidL : int,seqH : int,seqL : int,len : int,buf : opaque)
   Begin
     var offset, cmd, flags, lenp, result : int;
     var bufp : opaque;
     var path : string[1024];


     Call LogWritef(LogLvlTrace,"ClReg Command len=%d",len,0,0,0);
     
     offset = 0;
     Call DecodeCommand(buf,offset,cmd,path,flags,lenp,bufp);
     
     If cmd = CmdPut && offset > len Then
     Begin
	Call LogWritef(LogLvlError,"Clreg Command short buffer",0,0,0,0);
	Return;
     End;

     Call ApplyCommand(cmd,path,flags,lenp,bufp);
   End;

   Procedure SnapsaveValue(snapInfo : int, entryp : int)
   Begin
	var path : string[1024];
	var buf : opaque[4096];
	var offset, type : int;
	var entry : ^FregEntry;
	var sinfo : ^SnapInfo;
	
	entry = entryp;
	sinfo = snapInfo;
	Syscall FregPath(regh, entry.idH, entry.idL, path, 1024);
	
	offset = 0;
	Call XdrEncodeU32(buf,offset,CmdPut);
	Call XdrEncodeString(buf,offset,path);

	Call XdrEncodeU32(buf,offset,entry.len);
	Syscall FregGet(regh,entry.idH,entry.idL,entry.len,buf + offset, 0,0);

	{ append to snapshot }
	Syscall RaftSnapshotSave(sinfo.clidH,sinfo.clidL,sinfo.termH,sinfo.termL,sinfo.seqH,sinfo.seqL,offset,buf,sinfo.snapOffset);
	sinfo.snapOffset = sinfo.snapOffset + offset;
   End;
   
   Procedure SnapsaveKey(snapInfo : int, parentH : int,parentL : int)
   Begin
	var idH, idL : int;
	var entryp : FregEntry;
	var result : int;
	
	Syscall FregNext(regh, parentH, parentL, 0, 0, entryp, result);
	While result Do
	Begin
		If (entryp.Flags & FregTypeMask) = FregTypeKey Then
		Begin
		   { recurse to save child key }
		   Call SnapsaveKey(snapInfo,entryp.idH,entryp.idL);
		End Else Begin
		   Call SnapsaveValue(snapinfo,entryp);
		End;
		
		Syscall FregNext(regh, parentH, parentL, entryp.idH, entryp.IdL, entryp, result);
	End;
	
   End;
   
   
   Procedure Snapsave(clidH : int, clidL : int, termH : int, termL : int, seqH : int, seqL : int)
   Begin
	var snapinfo : SnapInfo;
	
	{ walk the entire tree, appending a command to the snapshot for each value found }
	Call LogWritef(LogLvlTrace,"ClReg Snapsave",0,0,0,0);

	Syscall RaftSnapshotSave(clidH,clidL,termH,termL,seqH,seqL,0,0,0);

	{ walk the tree and append a command for each new entry }
	snapinfo.clidH = clidH;
	snapinfo.clidL = clidL;
	snapinfo.termH = termH;
	snapinfo.termL = termL;
	snapinfo.seqH = seqH;
	snapinfo.seqL = seqL;
	snapinfo.snapOffset = 0;
	Call SnapsaveKey(snapinfo,0,0);
	
	{ final call sets offset=-1 }
	Syscall RaftSnapshotSave(clidH,clidL,termH,termL,seqH,seqL,0,0,-1);

	Call LogWritef(LogLvlTrace,"ClReg Snapsave Done",0,0,0,0);
   End;

   Procedure Snapload(clidH : int, clidL : int, len : int, buf : opaque)
   Begin
	var offset, cmd, flags, lenp : int;
	var bufp : opaque;
	var path : string[1024];

	{ the snapshot consists of a list of entries, each of which is a standard command buffer }
	Call LogWritef(LogLvlTrace,"ClReg Snapload",0,0,0,0);

	offset = 0;
	While offset < len Do
	Begin
		Call DecodeCommand(buf,offset,cmd,path,flags,lenp,bufp);
		Call ApplyCommand(cmd,path,flags,lenp,bufp);
	End;

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
	var offset, off, u : int;
	var u64h, u64l : int;
	var bufp : opaque[4096];

	offset = 0;
	off = 0;
	Call XdrEncodeU32(bufp, offset, CmdPut);
	Call XdrEncodeString(bufp, offset, path);
	Call XdrEncodeU32(bufp, offset, flags);
	If (flags & FregTypeMask) = FregTypeU32 Then Begin
	   Call XdrDecodeU32(buf, off, u);
	   Call XdrEncodeOpaque(bufp, offset, 4, &u);
	End Else If (flags & FregTypeMask) = FregTypeU64 Then Begin
	    Call XdrDecodeU64(buf, off, u64h, u64l);
	    Call XdrEncodeOpaque(bufp, offset, 8, &u64h);
	End Else If (flags & FregTypeMask) = FregTypeString Then Begin
	    Call XdrDecodeU32(buf, off, u);
	    Call XdrEncodeOpaque(bufp, offset, u, buf + 4);
	End Else 
	    Call XdrEncodeOpaque(bufp, offset, len, buf);
	
	Syscall RaftCommand(regclidH, regclidL, offset, bufp);
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

   Procedure Get(path : string, var result : int, var lenp : int, var bufp : opaque)
   Begin
	var sts, offset, type, parentH, parentL, u64H, u64L : int;
	var entry : FregEntry;
	var buf : opaque[4096];
	var resbuf : opaque[4096];
	
	
	Syscall FregEntryByName(regh,0,0,path,entry,sts);
	If !sts Then
	Begin
		lenp = 0;
		bufp = 0;
		result = 0;
		Return;
	End;

	type = entry.flags & FregTypeMask;
	If type = FregTypeKey Then
	Begin
	    offset = 0;
	    parentH = entry.idH;
	    parentL = entry.idL;
	    Syscall FregNext(regh,parentH,parentL,0,0,entry,result);
	    While result Do
	    Begin
		Call XdrEncodeU32(resbuf,offset,1);
		Call XdrEncodeString(resbuf,offset,entry.name);
		Call XdrEncodeU32(resbuf,offset,entry.flags);
		Call XdrEncodeU32(resbuf,offset,entry.len);
		Syscall FregNext(regh,parentH,parentL,entry.idH,entry.idL,entry,result);
	    End;
	    Call XdrEncodeU32(resbuf,offset,0);
	    bufp = resbuf;
	    lenp = offset;
	End Else Begin
	    Syscall FregGetByName(regh, 0,0, path, entry.flags, 4096, buf, lenp);
	    offset = 0;
	    bufp = resbuf;
	    If type = FregTypeU32 Then Begin
	       Call Memcpy(&u64h, buf, 4);
	       Call XdrEncodeU32(resbuf,offset,u64h);
	       lenp = offset;
	    End Else If type = FregTypeU64 Then Begin
	       Call Memcpy(&u64h, buf, 4);
	       Call Memcpy(&u64l, buf + 4, 4);
	       Call XdrEncodeU64(resbuf,offset,u64h, u64l);
	       lenp = offset;
	    End Else If type = FregTypeString Then Begin
	       Call XdrEncodeString(resbuf,offset,buf);
	       lenp = offset;
	    End Else If type = FregTypeOpaque Then Begin
   	       bufp = buf;
	    End;
	End;

	result = 1;
   End;
   
   { constant values }

End.
