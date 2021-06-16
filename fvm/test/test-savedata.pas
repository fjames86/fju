
{ -*- mode: fvm -*- }

Program SaveData(0,0,GetString,SetString,Save,Restore,Remove);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   
   { constants }

   { declarations }
      
   { globals }
   var g_str : string[256];
   
   { procedures }
   Procedure GetString(var s : string)
   Begin
	s = g_str;
   End;

   Procedure SetString(s : string)
   Begin
	Call Strcpy(g_str, s);
   End;

   Procedure Save(id : int)
   Begin
	Syscall FvmSaveData(id,0,0);
   End;

   Procedure Restore(id : int)
   Begin
	Syscall FvmLoadData(id,0);
   End;

   Procedure Remove(id : int)
   Begin
	Syscall FvmRemoveData(id,0);
   End;
   
   { constant values }

End.
