
{ -*- mode: fvm -*- }

Program StateSeq(0,0,Init,GetStateSeq);
Begin
   { Includes }
   Include "syscall.pas";
   
   { constants }
   
   { declarations }
   Declare Const var StateSeqPath : string;
   
   { globals }
   var stateseq : int;

   { procedures }
   Procedure Init()
   Begin
	var result : int;
	Syscall FregReadInt(StateSeqPath, stateseq, result);
	stateseq = stateseq + 1;
	Syscall FregWriteInt(StateSeqPath,stateseq);
   End;

   Procedure GetStateSeq(var seq : int)
   Begin
	seq = stateseq;
   End;
	
   { constant values }
   Const var StateSeqPath = "/fju/stateseq";

End.
