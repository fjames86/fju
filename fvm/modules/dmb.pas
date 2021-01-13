
{ -*- mode: fvm -*- }

Program Dmb(0,0,Publish);
Begin
   { Includes }
   Include "syscall.pas";

   { procedures }
   Procedure Publish(msgid : int, flags : int, len : int, buf : opaque)
   Begin
	Syscall DmbPublish(msgid,flags,len,buf);
   End;

End.
