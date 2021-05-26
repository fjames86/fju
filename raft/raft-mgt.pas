
{ -*- mode: fvm -*- }

Program RaftMgt(0,0,Init,Exit,MgtMsgAdd);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   Include "dmb.pas";
   
   { constants }
   Const RaftMgtMsgAdd = DmbCatRaftMgt + 0;
   Const RaftMgtMsgRem = DmbCatRaftMgt + 1;

   Const RaftMaxMember = 8;
   
   { declarations }
   Declare Procedure MgtMsgAdd(msgid : int, len : int, buf : opaque);
   
   { globals }

   { procedures }
   Procedure Init()
   Begin
	Syscall DmbSubscribe(&MgtMsgAdd,RaftMgtMsgAdd,DmbFlagRaw);
   End;

   Procedure Exit()
   Begin
	Syscall DmbUnsubscribe(&MgtMsgAdd);
   End;
   
   Procedure MgtMsgAdd(msgid : int, len : int, buf : opaque)
   Begin
	var offset : int;
	var clidH, clidL : int;
	var nmember : int;
	var members : int[RaftMaxMember*2];
	var i, hh, hl : int;
	
	offset = 0;
	Call XdrDecodeU64(buf,offset,clidH,clidL);
	Call XdrDecodeU32(buf,offset,nmember);
	i = 0;
	While i < nmember Do
	Begin
		Call XdrDecodeU64(buf,offset,hh, hl);
		members[2*i] = hh;
		members[2*i + 1] = hl;
		i = i + 1;
	End;
	Syscall RaftClusterAdd(clidH,clidL,nmember, members);
   End;

   { constant values }

End.
