
{ -*- mode: fvm -*- }

Program Dmb(0,0,Publish);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   
   { procedures }
   Procedure Publish(msgid : int, flags : int, len : int, buf : opaque)
   Begin
	var seqH, seqL : int;
	
	Syscall DmbPublish(msgid,flags,len,buf,seqH,seqL);
   End;

End.
