
{ -*- mode: fvm -*- }

Program DCHT(0,0,);
Begin
   { Includes }
   Include "syscall.pas";

   { constants }

   { declarations }
      
   { globals }

   { procedures }
   Procedure XorU64(x : int, y : int, z : int)
   Begin
	z[0] = x[0] ^ y[0];
	z[1] = x[1] ^ y[1];
   End;

   Procedure GtU64(x : int, y : int, var result)
   Begin
	result = (x[0] > y[0]) && (x[1] > y[1]) ? 1 : 0;
   End;

   Procedure NearestHost(key : int, var hostH : int, var lostL : int)
   Begin
	{ loop throuh all hosts, find the one with smallest key^hostid }

	{ hosts are stored in freg under /fju/hostreg/hosts/xx/id }
	Syscall FregEntryByName(0,0,0,"/fju/hostreg/hosts",entry,result);
	parentH = entry.idH;
	parentL = entry.idL;
	
	Syscall FregNext(0,parentH,parentL,idH,idL,entryp,result);
	While result Do
	Begin
		Syscall FregGetByName(0,idH,idL,"id",FregTypeU64,&hostid,8,lenp);
		If lenp = 8 Then
		Begin
			Call XorU64(key,&hostid,&gtu64);
			Call GtU64(&gtu64,&besthost,result);
			{ todo }
		End;
	End;
	
   End;
   
   { constant values }

End.
