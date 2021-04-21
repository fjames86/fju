
{ -*- mode: fvm -*- }

Program TestFile(0,0,TestProc,Init,Exit,ReadValue,WriteValue);
Begin
   { Includes }
   Include "syscall.pas";

   { constants }

   { declarations }
      
   { globals }
   var gfd : int;
   
   { procedures }
   Procedure TestProc(filename : string)
   Begin
	var fd : int;
	var buf : opaque[16];
	Syscall Open(filename,fd);
	If fd = 0 Then Begin
	   Syscall Puts("failed to open file");
	   Return;
	End;
	   
	Syscall Read(fd,16,buf,0);
	Syscall Puts(buf);
	Syscall Close(fd);
   End;

   Procedure Init()
   Begin
	Syscall Open("testfile.dat",gfd);
   End;

   Procedure Exit()
   Begin
	Syscall Close(gfd);
   End;

   Procedure ReadValue(var val : int)
   Begin
	Syscall Read(gfd,4,&val,0);
   End;

   Procedure WriteValue(val : int)
   Begin
	Syscall Write(gfd,4,&val,0);
   End;
   
   { constant values }

End.
