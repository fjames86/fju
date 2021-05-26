
{ -*- mode: fvm -*- }

Program RaftMgt(0,0,Init,Exit,MgtMsgAdd,MgtMsgRem,AddMember,RemoveMember);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   Include "dmb.pas";
   Include "log.pas";
   
   { constants }
   Const RaftMgtMsgAdd = DmbCatRaftMgt + 0;
   Const RaftMgtMsgRem = DmbCatRaftMgt + 1;

   Const RaftMaxMember = 8;
   
   { declarations }
   Declare Procedure MgtMsgAdd(msgid : int, len : int, buf : opaque);
   Declare Procedure MgtMsgRem(msgid : int, len : int, buf : opaque);   
   
   { globals }

   { procedures }
   Procedure Init()
   Begin
	Syscall DmbSubscribe(&MgtMsgAdd,RaftMgtMsgAdd,DmbFlagRaw);
	Syscall DmbSubscribe(&MgtMsgRem,RaftMgtMsgRem,DmbFlagRaw);	
   End;

   Procedure Exit()
   Begin
	Syscall DmbUnsubscribe(&MgtMsgAdd);
	Syscall DmbUnsubscribe(&MgtMsgRem);	
   End;
   
   Procedure MgtMsgAdd(msgid : int, len : int, buf : opaque)
   Begin
	var offset : int;
	var clidH, clidL : int;
	var hostH, hostL : int;
	
	offset = 0;
	Call XdrDecodeU64(buf,offset,clidH,clidL);
	Call XdrDecodeU64(buf,offset,hostH,hostL);
	Syscall RaftClusterAddMember(clidH,clidL,hostH,hostL);
   End;

   Procedure MgtMsgRem(msgid : int, len : int, buf : opaque)
   Begin
	var offset : int;
	var clidH, clidL : int;
	var hostH, hostL : int;
	
	offset = 0;
	Call XdrDecodeU64(buf,offset,clidH,clidL);
	Call XdrDecodeU64(buf,offset,hostH,hostL);
	Syscall RaftClusterRemMember(clidH,clidL,hostH,hostL);
   End;

   Procedure AddMember(clidH : int, clidL : int, hostname : string)
   Begin
	var hostH, hostL : int;
	var offset : int;
	var buf : opaque[64];
	
	Syscall HostregIdByName(hostname,hostH,hostL);
	If hostH && hostL Then
	Begin
		offset = 0;
		Call XdrEncodeU64(buf,offset,clidH,clidL);
		Call XdrEncodeU64(buf,offset,hostH,hostL);
		Syscall DmbPublish(RaftMgtMsgAdd,0,offset,buf,0,0);		
	End Else Begin
	    Call LogWritef(LogLvlError,"AddMember unknown host %s",hostname,0,0,0);
	End;
	
   End;

   Procedure RemoveMember(clidH : int, clidL : int, hostname : string)
   Begin
	var hostH, hostL : int;
	var offset : int;
	var buf : opaque[64];
	
	Syscall HostregIdByName(hostname,hostH,hostL);
	If hostH && hostL Then
	Begin
		offset = 0;
		Call XdrEncodeU64(buf,offset,clidH,clidL);
		Call XdrEncodeU64(buf,offset,hostH,hostL);
		Syscall DmbPublish(RaftMgtMsgRem,0,offset,buf,0,0);
	End;
	
   End;

   { constant values }

End.
