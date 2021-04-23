
{ -*- mode: fvm -*- }

{
 * Implements a very naive hash table using log
 * Adding new entries is done by appending to the end
 * deleting entries is done by appending a deletion entry to the end 
 * lookups are done by walking the log, starting from the end.
 * Note that this isn't really a hash table at all - it is really a simple lookup table
}

Program LHT(0,0,Init,Exit,Get,Put,Rem);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   
   { constants }
   Const MaxKey = 32;
   Const EntryDeleted = 0x0001;
   
   { declarations }
   Record Entry =
   	  key : string[MaxKey];
	  flags : int;
   End;
   
   { globals }
   var gfd : int;
   
   { procedures }
   Procedure Init()
   Begin
	Syscall LogOpen("lht",gfd);
   End;

   Procedure Exit()
   Begin
	Syscall LogClose(gfd);
   End;

   Procedure Put(key : string, len : int, buf : opaque)
   Begin
	{ Append entry to log }
	var entry : Entry;

	{ write data first }
	Syscall LogWrite(gfd,LogBinary,len,buf);

	{ then write entry }
	Call strcpy(entry.key,key);
	entry.flags = 0;
	Syscall LogWrite(gfd,LogBinary,SizeOf(entry),entry);
   End;

   Procedure Get(key : string, var found : int, var len : int, var buf : opaque)
   Begin
	{ starting from end of log, read entries until get a match or run out of entries. If the matching entry indicates a deletion then key not found. }
	var entry : Entry;
	var idh, idl, flags, lenp, sts : int;
	var xbuf : opaque[1024];

	found = 0;
	len = 0;
	buf = 0;
	
	{ start from end of log }
	Syscall LogLastid(gfd,idh,idl);
	While idh && idl Do
	Begin
		Syscall LogRead(gfd,idh,idl,SizeOf(entry),entry,flags,lenp);
		Call Strcmp(entry.key,key,sts);
		If sts Then Begin
		   { key match }
		   If entry.flags & EntryDeleted Then Begin
		      Call LogWritef(LogLvlInfo,"LHT key found deleted",0,0,0,0);
		   End Else Begin
		       Syscall LogPrev(gfd,idh,idl,idh,idl);
		       If idh && idl Then Begin
		       	  Syscall LogRead(gfd,idh,idl,SizeOf(xbuf),xbuf,flags,lenp);
			  found = 1;
		       	  len = lenp;
		       	  buf = xbuf;
		       End;
		   End;
		   Return;
		End;

		If !(entry.flags & EntryDeleted) Then
		Begin
			Syscall LogPrev(gfd,idh,idl,idh,idl);
		End;
		If idh && idl Then Syscall LogPrev(gfd,idh,idl,idh,idl);
	End;
	
	found = 0;
	len = 0;
	buf = 0;
   End;

   Procedure Rem(key : string)
   Begin
	{ Append deletion entry to log }
	var entry : Entry;
	Call Strcpy(entry.key, key);
	entry.flags = EntryDeleted;
	Syscall LogWrite(gfd,LogBinary,SizeOf(entry),entry);
   End;
   
   { constant values }

End.
