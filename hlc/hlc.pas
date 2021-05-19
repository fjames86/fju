
{ -*- mode: fvm -*- }

{
 * Naive hash chain implemented using an fju log 
}

Program HLC(0,0,Open,Close,Read,Write,Init,Exit,MsgAppend);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   Include "log.pas";
   Include "dmb.pas";
   
   { constants }
   Const HdrSize = 32;
   Const HlcMsgAppend = DmbCatHlc + 0;
   
   { declarations }
   Record Header =
   	  hash : opaque[32];
   End;

   { globals }
   var gfd : int;
   
   { procedures }
   Procedure Open(name : string)
   Begin
	If gfd Then Syscall LogClose(gfd);
	Syscall LogOpen(name,gfd);
   End;

   Procedure Close()
   Begin
	If gfd Then Syscall LogClose(gfd);
	gfd = 0;
   End;
   
   Procedure Read(idhigh : int, idlow : int, var lenp : int, var bufp : opaque)
   Begin
	var buf : opaque[2048];
	var flagsp, lenpp : int;

	If gfd = 0 Then Return;
	
	Syscall LogRead(gfd,idhigh,idlow,SizeOf(buf),buf,flagsp,lenpp);
	If lenpp >= HdrSize Then
	Begin
		bufp = buf + HdrSize;
		lenp = lenpp - HdrSize;
	End
	Else	
		Call LogWritef(LogLvlError,"HLC previous entry too small %u", lenpp, 0,0,0);

   End;

   Procedure GetHash(seqH : int, seqL : int, tsH : int, tsL : int, len : int, buf : opaque, hash : opaque)
   Begin
	var iov : SecIov[5];
	var iovp : ^SecIov;

	iovp = iov[0];
	iovp.len = 4;
	iovp.buf = &seqH;
	iovp = iov[1];
	iovp.len = 4;
	iovp.buf = &seqL;
	iovp = iov[2];
	iovp.len = 4;
	iovp.buf = &tsH;
	iovp = iov[3];
	iovp.len = 4;
	iovp.buf = &tsH;
	iovp = iov[4];
	iovp.len = len;
	iovp.buf = buf;
	Syscall Sha1(5,iov,hash);
   End;

   Procedure WriteGenesisBlock()
   Begin
	var bufp : opaque[HdrSize];

	Call GetHash(-1,-1,-1,-1,0,0,bufp);
	Syscall LogWrite(gfd,LogBinary,HdrSize,bufp);
   End;
   
   Procedure Write(len : int, buf : opaque, var idHigh : int, var idLow : int)
   Begin
	var bufp : opaque[2048];
	var idh, idl : int;
	var loginfo : LogInfo;
	
	If gfd = 0 Then Return;

	Syscall LogLastId(gfd,idh,idl);
	If (idh = 0) && (idl = 0) Then
	Begin
	   Call WriteGenesisBlock();
	   Syscall LogLastId(gfd,idh,idl);
	End;

	{ Compute header derived from previous entry }
	Syscall LogReadInfo(gfd,idh,idl,loginfo);
	
	Call GetHash(loginfo.SeqH, loginfo.Seql, loginfo.TimestampH, loginfo.TimestampL,len,buf,bufp);
	Call Memcpy(bufp + HdrSize,buf,len);
	Syscall LogWrite(gfd,LogBinary,len + HdrSize, bufp);
	Syscall LogLastId(gfd,idh,idl);
	idHigh = idh;
	idLow = idl;

	{ publish dmb message }	
	Syscall DmbPublish(HlcMsgAppend, DmbRemote, bufp, len + HdrSize, 0, 0);
   End;

   Procedure Append(hdrlen : int, hdr : opaque, len : int, buf : opaque)
   Begin
	var idh, idl : int;
	var tmphdr : opaque[HdrSize];
	var result : int;
	var loginfo : LogInfo;
	
	Syscall LogLastId(gfd,idh,idl);
	If (idh = 0) && (idl = 0) Then
	Begin
		Call WriteGenesisBlock();
		Syscall LogLastId(gfd,idh,idl);
	End;

	{ Compute the hash that we would generate and compare }
	Syscall LogReadInfo(gfd,idh,idl,loginfo);
	Call GetHash(loginfo.SeqH, loginfo.SeqL, loginfo.TimestampH, loginfo.TimestampL,len,buf,tmphdr);
	Call Memcmp(tmphdr,hdr,Hdrsize,result);
	If result = 0 Then
	Begin
		Call LogWritef(LogLvlError,"HLC Append mismatch",0,0,0,0);
		Return;
	End;

	Call Write(len,buf,0,0);
	
   End;

   Procedure MsgAppend(msgid : int, len : int, buf : opaque)
   Begin
	{ decode message and append }
	If msgid <> HlcMsgAppend Then Return;

	{ msg buffer should be <hdr><buffer> }
	If len < HdrSize Then Return;

	Call Append(Hdrsize, buf, len - HdrSize, buf + HdrSize);	
   End;
   
   Procedure Init()
   Begin
	Call Open("hlc");
	Syscall DmbSubscribe(&MsgAppend, HlcMsgAppend, DmbFlagRaw);
   End;

   Procedure Exit()
   Begin
	Call Close();
   End;
   
   { constant values }

End.
