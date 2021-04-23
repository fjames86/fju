
{ -*- mode: fvm -*- }

Program TestRaft(0,0,Init,Exit,Command,Snapsave,Snapload,GetValue);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   
   { constants }
   Const APPID = 123321;
   
   { declarations }
      
   { globals }
   var myglob : int;
   
   { procedures }
   Procedure Command(clidH : int, clidL : int, seqH : int, seqL : int, len : int, buf : opaque)
   Begin
	Call LogWritef(LogLvlInfo, "TestRaft Command",0,0,0,0);
	
	myglob = myglob + 1;
   End;

   Procedure Snapsave(clidH : int, clidL : int, termH : int, termL : int, seqH : int, seqL : int)
   Begin
	var fd : int;

	Call LogWritef(LogLvlInfo,"TestRaft Snapsave",0,0,0,0);

	Syscall Open("TestRaft.dat",fd);
	If fd = 0 Then Return;
	Syscall Write(fd,&myglob,4,0);
	Syscall Close(fd);
   End;

   Procedure Snapload(clidH : int, clidL : int, len : int, buf : opaque)
   Begin
	var fd : int;

	Call LogWritef(LogLvlInfo, "TestRaft Snapload",0,0,0,0);

	Syscall Open("TestRaft.dat",fd);
	If fd = 0 Then Return;
	Syscall Read(fd,&myglob,4,0);
	Syscall Close(fd);
   End;
   
   Procedure Init()
   Begin
	Syscall RaftAppRegister(APPID,&Command,&Snapsave,&Snapload);
   End;

   Procedure Exit()
   Begin
	Syscall RaftAppUnregister(APPID);
   End;
	
   Procedure GetValue(var x : int)
   Begin
	x = myglob;
   End;

   { constant values }

End.
