
{ -*- mode: fvm -*- }

Program TestReg(0,0,Main);
Begin
   { Includes }
   Include "syscall.pas";

   { constants }

   { declarations }
      
   { globals }

   { procedures }
   Procedure Main()
   Begin
	var handle : int;
	var idH, idl : int;
	
	Syscall FregOpen("frank",handle);
	Syscall FregSubkey(handle,0,0,"frank",idH,idL);
	If idH && idL Then
 	   Syscall Puts("Subkey Success")
	Else
	   Syscall Puts("Subkey Failure");

	Syscall FregPut(handle,idH,idL,"myvalue",FregTypeString,6,"hello",idH,idL);
	If idH && idL Then
	   Syscall Puts("Put Success")
	Else
	   Syscall Puts("Put Failure");
		
	Syscall FregClose(handle);
   End;
   
   { constant values }

End.
