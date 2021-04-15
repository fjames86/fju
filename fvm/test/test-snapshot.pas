
{ -*- mode: fvm -*- }

{
 * This implements a test module which saves its state to a file and 
 * can later restore it from the file. 
}

Program TestSnapshot(0,0,Init,GetString,SetString,Save,Load);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   
   { constants }
   
   { declarations }
   Declare Const var FILENAME : string;
   
   { globals }
   var gstart : int;
   var mystr : string[64];   
   var gend : int;

   { procedures }
   Procedure Init()
   Begin
	Call Strcpy(mystr,"Init");
   End;

   Procedure SetString(s : string, var prev : string)
   Begin
	var l : int;
	var pstr : string[64];
	Call Strcpy(pstr,mystr);
	
	Call strlen(s, l);
	If (l > 0) && (l < 64) Then
	   Call Strcpy(mystr,s);
	   
	prev = pstr;
   End;

   Procedure GetString(var p : string)
   Begin
	p = mystr;
   End;
   
   Procedure Save()
   Begin
	var fd : int;

	Syscall Open(FILENAME,fd);
	If fd = 0 Then Return;
	Syscall Write(fd, &gend - &gstart, &gstart, 0);
	Syscall Close(fd);
   End;

   Procedure Load()
   Begin
	var fd : int;
	
	Syscall Open(FILENAME,fd);
	If fd = 0 Then
	Begin
		Call LogWritef(LogLvlError,"Failed to open file",0,0,0,0);
		Return;
	End;
	
	Syscall Read(fd, &gend - &gstart, &gstart, 0 );
	Syscall Close(fd);
   End;
   
   { constant values }
   Const var FILENAME = "testsnapshot.dat";
   
End.
