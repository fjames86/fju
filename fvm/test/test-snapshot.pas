
{ -*- mode: fvm -*- }

{
 * This implements a test module which saves its state to a file and 
 * can later restore it from the file. 
}

Program TestSnapshot(0,0,Init,Exit,GetString,SetString,Save,Load,MsgLoad,Publish,SaveData,LoadData);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   Include "dmb.pas";
   
   { constants }
   Const MsgIdLoad = DmbCatTestsnapshot + 0;
   
   { declarations }
   Declare Const var FILENAME : string;
   Declare Procedure MsgLoad(msgid : int, len : int, buf : opaque);
   
   { globals }
   var gstart : int;
   var mystr : string[64];   
   var gend : int;

   { procedures }
   Procedure Init()
   Begin
	Call Strcpy(mystr,"Init");
	Syscall DmbSubscribe(&MsgLoad, MsgIdLoad, DmbFlagRaw);
   End;

   Procedure Exit()
   Begin
	Syscall DmbUnsubscribe(&MsgLoad);
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

   Procedure MsgLoad(msgid : int, len : int, buf : opaque)
   Begin
	If len = (&gend - &gstart) Then
	   Call Memcpy(&gstart, buf, len);
	   
   End;

   Procedure Publish()
   Begin
	var seqh, seql : int;

	Syscall DmbPublish(MsgIdLoad, DmbRemote, &gend - &gstart, &gstart, seqh, seql);
   End;

   Procedure SaveData()
   Begin
	Syscall FvmSaveData(0,0,0);
   End;

   Procedure LoadData()
   Begin
	Syscall FvmLoadData(0,0);
   End;
   
   { constant values }
   Const var FILENAME = "testsnapshot.dat";
   
End.
