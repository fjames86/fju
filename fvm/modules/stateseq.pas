
{ -*- mode: fvm -*- }

{
 * This module maintains a sequence number that is incremented when the service starts.
 * It is intended to be loaded at startup and never unloaded. 
 * Subscribers to the StateSeqMsg dmb message are informed when this service has restarted.
}

Program StateSeq(0,0,Init,GetStateSeq);
Begin
   { Includes }
   Include "syscall.pas";
   Include "dmb.pas";
   
   { constants }
   Const StateSeqMsg = DmbCatStateSeq + 0;
   
   { declarations }
   Declare Const var StateSeqPath : string;
   
   { globals }
   var stateseq : int;

   { procedures }
   Procedure Init()
   Begin
	var result : int;
	Syscall FregGetByName(0,0,0,StateSeqPath,FregTypeU32,4,&stateseq,0);
	stateseq = stateseq + 1;
	Syscall FregPut(0,0,0,StateSeqPath,FregTypeU32,4,&stateseq,0,0);

	Syscall DmbPublish(StateSeqMsg,DmbRemote,4,&stateseq,0,0);
   End;

   Procedure GetStateSeq(var seq : int)
   Begin
	seq = stateseq;
   End;
	
   { constant values }
   Const var StateSeqPath = "/fju/stateseq";

End.
