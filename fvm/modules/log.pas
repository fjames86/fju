
{ -*- mode:fvm -*- }


Program Log(0x2fff777a,1,ProcNull,ProcRead,ProcWrite,ProcLastId,ProcReadEnd,LogRead,LogWrite);
Begin

Include "syscall.pas";
Include "string.pas";
Include "xdr.pas";

Const LogEntryHeader = 24; { reserve space for id,flags,timestamp,msglen }

Procedure ProcNull()
Begin
End;

{ helper function to read entries }
Procedure ReadLogEntries(logname : string, idhigh : int, idlow : int, n : int, var nentries : int, var elen : int, var ebuf : opaque, fromend : int)
Begin
	var flags, len, offset, ne : int;
	var timeH, timeL, l, f : int;
	var p : opaque;
	var xdr : opaque[2048];

	offset = 0;
	ne = 0;
	If fromend Then Syscall LogPrev(logname,idhigh,idlow,idhigh,idlow)
	Else Syscall LogNext(logname,idhigh,idlow,idhigh,idlow);
	
	While (idhigh|idlow) Do
	Begin
		If (offset + LogEntryHeader) >= 2048 Then Break;
		
		flags = 0;
		len = 0;
		p = xdr + offset + LogEntryHeader;
		Syscall LogRead(logname,idhigh,idlow,2048 - (offset + LogEntryHeader),p,flags,len);
		If len > 2048 - (offset + LogEntryHeader) Then Begin
		   { the entry was larger than space left in buffer so break here }
		   Break;
		End;
		
		Call XdrEncodeU64(xdr,offset,idhigh,idlow);		
		Call XdrEncodeU32(xdr,offset,flags);

		Syscall LogReadInfo(logname,idhigh,idlow,l,f,timeH,timeL);
		Call XdrEncodeU64(xdr,offset,timeH,timeL);
		
		Call XdrEncodeU32(xdr,offset,len);
		If len % 4 Then len = len + 4 - (len % 4);
		offset = offset + len;
		ne = ne + 1;
		If ne >= n Then Break;

		If fromend Then Syscall LogPrev(logname,idhigh,idlow,idhigh,idlow)
		Else Syscall LogNext(logname,idhigh,idlow,idhigh,idlow);
	End;
	
	nentries = ne;
	elen = offset;
	ebuf = xdr;
End;

{ read up to n entries starting from id }
Procedure ProcRead(logname : string, idhigh : int, idlow : int, n : int, var nentries : int, var elen : int, var ebuf : opaque)
Begin
	Call ReadLogEntries(logname,idhigh,idlow,n,nentries,elen,ebuf,0);
End;

{ procedure to write into log }
Procedure ProcWrite(logname : string, flags : int, len : int, buf : opaque, var idhigh : int, var idlow : int)
Begin
	Syscall LogWrite(logname,flags,len,buf);
	Syscall LogLastId(logname,idhigh,idlow);
End;

{ procedure to get last id }
Procedure ProcLastId(logname : string, var idhigh : int, var idlow : int)
Begin
	Syscall LogLastId(logname,idhigh,idlow);
End;

{ read entries from end }
Procedure ProcReadEnd(logname : string, idhigh : int, idlow : int, n : int, var nentries : int, var elen : int, var ebuf : opaque)
Begin
	Call ReadLogEntries(logname,idhigh,idlow,n,nentries,elen,ebuf,1);
End;

{ ------- non-rpc procedures below ------ }

Procedure LogRead(logname : string, idHigh : int, idLow : int, var flags : int, var lenp : int, var bufp : opaque)
Begin
	var len : int;
	var buf : opaque[2048];
	
	Syscall LogRead(logname,idhigh,idlow,2048,buf,flags,len);
	bufp = buf;
	lenp = len;
End;

Procedure LogWrite(logname : string, flags : int, len : int, buf : opaque)
Begin
	Syscall LogWrite(logname,flags,len,buf);
End;

End.

{ Local Variables: }
{ fvm-output-path: "/opt/fju/fvm/log.fvm" }
{ End: }
