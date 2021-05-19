
{ -*- mode:fvm -*- }


Program Log(0x2fff777a,1,ProcNull,ProcRead,ProcWrite,ProcLastId,ProcReadEnd,LogRead,LogWrite);
Begin

Include "syscall.pas";
Include "string.pas";
Include "xdr.pas";
Include "log.pas";

Const LogEntryHeader = 24; { reserve space for id,flags,timestamp,msglen }

Procedure ProcNull(alen : int, abuf : opaque, var rlen : int, var rbuf : opaque)
Begin
End;

{ helper function to read entries }
Procedure ReadLogEntries(logname : string, idhigh : int, idlow : int, n : int, var nentries : int, var elen : int, xdr : opaque, fromend : int)
Begin
	var flags, len, offset, ne : int;
	var timeH, timeL : int;
	var p : opaque;
	var logh : int;
	var loginfo : LogInfo;

	Syscall LogOpen(logname,logh);

	offset = 0;
	ne = 0;
	If fromend Then Syscall LogPrev(logh,idhigh,idlow,idhigh,idlow)
	Else Syscall LogNext(logh,idhigh,idlow,idhigh,idlow);

	While (idhigh|idlow) Do
	Begin
		If (offset + LogEntryHeader) >= 2048 Then Break;
		
		flags = 0;
		len = 0;
		p = xdr + offset + LogEntryHeader;
		Syscall LogRead(logh,idhigh,idlow,2048 - (offset + LogEntryHeader),p,flags,len);
		If len > 2048 - (offset + LogEntryHeader) Then Begin
		   { the entry was larger than space left in buffer so break here }
		   Break;
		End;
		
		Call XdrEncodeU64(xdr,offset,idhigh,idlow);		
		Call XdrEncodeU32(xdr,offset,flags);

		Syscall LogReadInfo(logh,idhigh,idlow,loginfo);
		timeH = loginfo.TimestampH;
		timeL = loginfo.TimestampL;
		
		Call XdrEncodeU64(xdr,offset,timeH,timeL);
		
		Call XdrEncodeU32(xdr,offset,len);
		If len % 4 Then len = len + 4 - (len % 4);
		offset = offset + len;
		ne = ne + 1;
		If ne >= n Then Break;

		If fromend Then Syscall LogPrev(logh,idhigh,idlow,idhigh,idlow)
		Else Syscall LogNext(logh,idhigh,idlow,idhigh,idlow);
	End;

	Syscall LogClose(logh);
	
	nentries = ne;
	elen = offset;
End;

{ read up to n entries starting from id }
Procedure ProcRead(alen : int, abuf : opaque, var rlen : int, var rbuf : opaque)
Begin
	var logname : string[64];
	var idhigh, idlow, n : int;
	var offset : int;
	var elen, nentries : int;
	var buf : opaque[4096];

	offset = 0;
	Call XdrDecodeString(abuf,offset,logname);
	Call XdrDecodeU32(abuf,offset,idhigh);
	Call XdrDecodeU32(abuf,offset,idlow);
	Call XdrDecodeU32(abuf,offset,n);

	Call ReadLogEntries(logname,idhigh,idlow,n,nentries,elen,buf + 4,0);

	offset = 0;
	Call XdrEncodeU32(buf,offset,nentries);
	offset = offset + elen;
	
	rlen = offset;
	rbuf = buf;
End;

{ procedure to write into log }
Procedure ProcWrite(alen : int, abuf : opaque, var rlen : int, var rbuf : opaque)
Begin
	var logname : string[64];
	var len, idhigh, idlow, offset : int;
	var buf : opaque;
	var logh, flags : int;
	var bufp : opaque[8];
	
	offset = 0;
	Call XdrDecodeString(abuf,offset,logname);
	Call XdrDecodeOpaqueRef(abuf,offset,len,buf);

	Syscall LogOpen(logname,logh);
	Syscall LogWrite(logname,flags,len,buf);
	Syscall LogLastId(logname,idhigh,idlow);
	Syscall LogClose(logh);

	offset = 0;
	Call XdrEncodeU32(bufp,offset,idhigh);
	Call XdrEncodeU32(bufp,offset,idlow);
	rlen = offset;
	rbuf = bufp;
End;

{ procedure to get last id }
Procedure ProcLastId(alen : int, abuf : opaque, var rlen : int, var rbuf : opaque) 
Begin
	var logname : string[64];
	var logh, offset : int;
	var bufp : opaque[8];
	var idhigh, idlow : int;
	
	offset = 0;
	Call XdrDecodeString(abuf,offset,logname);
	
	Syscall LogOpen(logname,logh);
	Syscall LogLastId(logname,idhigh,idlow);
	Syscall LogClose(logh);

	offset = 0;
	Call XdrEncodeU32(bufp, offset, idhigh);
	Call XdrencodeU32(bufp, offset, idlow);
	rlen = offset;
	rbuf = bufp;
End;

{ read entries from end }
Procedure ProcReadEnd(alen : int, abuf : opaque, var rlen : int, var rbuf : opaque)
Begin
	var logname : string[64];
	var idhigh, idlow, n : int;
	var offset : int;
	var elen, nentries : int;
	var buf : opaque[4096];

	offset = 0;
	Call XdrDecodeString(abuf,offset,logname);
	Call XdrDecodeU32(abuf,offset,idhigh);
	Call XdrDecodeU32(abuf,offset,idlow);
	Call XdrDecodeU32(abuf,offset,n);

	Call ReadLogEntries(logname,idhigh,idlow,n,nentries,elen,buf + 4,1);

	offset = 0;
	Call XdrEncodeU32(buf,offset,nentries);
	offset = offset + elen;
	
	rlen = offset;
	rbuf = buf;
End;

{ ------- non-rpc procedures below ------ }

Procedure LogRead(logname : string, idHigh : int, idLow : int, var flags : int, var lenp : int, var bufp : opaque)
Begin
	var len : int;
	var logh : int;
	var buf : opaque[2048];

	Syscall LogOpen(logname,logh);
	Syscall LogRead(logname,idhigh,idlow,2048,buf,flags,len);
	Syscall LogClose(logh);
	bufp = buf;
	lenp = len;
End;

Procedure LogWrite(logname : string, flags : int, len : int, buf : opaque)
Begin
	var logh : int;
	Syscall LogOpen(logname,logh);
	Syscall LogWrite(logname,flags,len,buf);
	Syscall LogClose(logh);
End;

End.

{ Local Variables: }
{ fvm-output-path: "/opt/fju/fvm/log.fvm" }
{ End: }
