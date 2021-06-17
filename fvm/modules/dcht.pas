
{ -*- mode: fvm -*- }

Program DCHT(0,0,Init,Exit,Get,Put,_Put,_Get, _getcb);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   Include "programs.pas";
   
   { constants }

   { declarations }
      
   { globals }

   { procedures }
   Procedure XorU64(x : int, y : int, z : int)
   Begin
	z[0] = x[0] ^ y[0];
	z[1] = x[1] ^ y[1];
   End;

   Procedure LtU64(x : int, y : int, var result : int)
   Begin
	result = (x[0] < y[0]) || ((x[0] = y[0]) && (x[1] < y[1]));
   End;

   Procedure NearestHost(key : int, var hostH : int, var hostL : int)
   Begin
	var entry : FregEntry;
	var result, lenp, localH, localL : int;
	var parent, hostid, besthost, bu64, u64 : int[2];
	
	{ loop through all hosts, find the one with smallest key^hostid }
	Syscall HostregLocalId(localH,localL);
	
	{ hosts are stored in freg under /fju/hostreg/hosts/xx/id }
	Syscall FregEntryByName(0,0,0,"/fju/hostreg/hosts",entry,result);
	parent[0] = entry.idH;
	parent[1] = entry.idL;
	
	Syscall FregNext(0,parent[0], parent[1],0,0,entry,result);
	While result Do
	Begin
		Syscall FregGetByName(0,entry.idH,entry.idL,"id",FregTypeU64,hostid,8,lenp);
		If (lenp = 8) && ((hostid[0] <> localH) && (hostid[1] <> localL)) Then
		Begin
			Call XorU64(key,hostid,u64);
			Call LtU64(u64,bu64,result);
			If result Then
			Begin
				besthost[0] = hostid[0];
				besthost[1] = hostid[1];
				bu64[0] = u64[0];
				bu64[1] = u64[1];
			End;
		End;
		Syscall FregNext(0,parent[0], parent[1],0,0,entry,result);		
	End;

	hostH = besthost[0];
	hostL = besthost[1];
   End;

   Procedure Init()
   Begin
   End;

   Procedure Exit()
   Begin
   End;

   { Internal procedure used to distribute values }
   Procedure _Get(klen : int, key : opaque, var kleno : int, var kbuf : opaque, var vlen : int, var vbuf : opaque)
   Begin
	var lenp : int;
	var buf : opaque[4096];
	
	kleno = ChtKeySize;
	kbuf = key;
	Syscall ChtRead(key,SizeOf(buf),buf,lenp);
	vlen = lenp;
	vbuf = buf;
   End;
   
   Procedure _getcb(len : int, buf : opaque, private : int)
   Begin
	var offset, sts : int;
	var vblen, vlen, klen : int;
	var vbuf, key : opaque;
	
	{ 
	results: success (boolean) result buffer (opaque)
	result buffer: klen,key,vlen,vbuf 
	}
	
	offset = 0;
	Call XdrDecodeU32(buf,offset,sts);
	If !sts Then Return;

	Call XdrDecodeU32(buf,offset,vblen); { result buffer len }
	Call XdrDecodeOpaque(buf,offset,klen,key); { klen/key }	
	Call XdrDecodeOpaque(buf,offset,vlen,vbuf); { value len }

	If vlen > 0 Then
	   Syscall ChtWrite(key,vlen,vbuf);
   End;
   
   Procedure CallGet(hostH : int, hostL : int, key : opaque)
   Begin
	var cinfo : RpcCallInfo;
	var offset : int;
	var buf : opaque[4096];
	
	cinfo.HostH = hostH;
	cinfo.HostL = hostL;
	cinfo.Prog = FvmRpcProg;
	cinfo.Vers = FvmRpcVers;
	cinfo.Proc = 4;
	cinfo.Timeout = 500;

	offset = 0;
	Call XdrEncodeString(buf,offset,"DCHT");
	Call XdrEncodeString(buf,offset,"_Get");
	Call XdrEncodeU32(buf,offset,4+ChtKeySize);
	Call XdrEncodeOpaque(buf,offset,ChtKeySize,key);
	Syscall RpcCall(cinfo,offset,buf,&_getcb,0);
   End;

   Procedure Get(klen : int, key : opaque, var vlen : int, var val : opaque)
   Begin
	var lenp : int;
	var hostH, hostL : int;
	var databuf : opaque[4096];

	If klen <> ChtKeySize Then Return;
	
	Syscall ChtRead(key,databuf,4096,lenp);
	If lenp Then
	Begin
		vlen = lenp;
		val = databuf;
		Return;
	End;

	Call NearestHost(key,hostH,hostL);
	
	{ ask nearest host for the value }
	Call CallGet(hostH,hostL,key);
   End;

   Procedure CallPut(hostH : int, hostL : int, key : opaque, vlen : int, val : opaque)
   Begin
	var cinfo : RpcCallInfo;
	var offset : int;
	var buf : opaque[4096];
	
	cinfo.HostH = hostH;
	cinfo.HostL = hostL;
	cinfo.Prog = FvmRpcProg;
	cinfo.Vers = FvmRpcVers;
	cinfo.Proc = 4;
	cinfo.Timeout = 500;

	offset = 0;
	Call XdrEncodeString(buf,offset,"DCHT");
	Call XdrEncodeString(buf,offset,"_Put");
	Call XdrEncodeU32(buf,offset,4+ChtKeySize+4+vlen);
	Call XdrEncodeOpaque(buf,offset,ChtKeySize,key);
	Call XdrEncodeOpaque(buf,offset,vlen,val);
	Syscall RpcCall(cinfo,offset,buf,0,0);
   End;

   { Internal routine used to distribute values }
   Procedure _Put(klen : int, key : opaque, vlen : int, val : opaque)
   Begin
   	If klen <> ChtKeySize Then Return;
   	Syscall ChtWrite(key,vlen,val);
   End;
   
   Procedure Put(klen : int, key : opaque, vlen : int, val : opaque)
   Begin
	var hostH, hostL : int;
	var cinfo : RpcCallInfo;

	If klen <> ChtKeySize Then Return;
	
	{ store local }
	Syscall ChtWrite(key,vlen,val);

	{ get to nearest host }
	Call NearestHost(key,hostH,hostL);
	
	{ Send to nearest host }
	Call CallPut(hostH,hostL,key,vlen,val);
   End;
   
   { constant values }

End.
