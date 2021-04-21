
{ -*- mode: fvm -*- }

{
 * Attempt at rewriting hlc.c into fvm pascal.
}

Program HLC(0,0,Open,Close,Read,Write,Init,Exit);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "xdr.pas";
   Include "log.pas";
   
   { constants }
   Const HdrSize = 32;
   
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

   Procedure GetHash(idH : int, idL : int, tsH : int, tsL : int, len : int, buf : opaque, hash : opaque)
   Begin
	var iov : SecIov[5];
	var iovp : ^SecIov;

	iovp = iov[0];
	iovp.len = 4;
	iovp.buf = &idH;
	iovp = iov[1];
	iovp.len = 4;
	iovp.buf = &idL;
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
   
   Procedure Write(len : int, buf : opaque)
   Begin
	var bufp : opaque[2048];
	var idh, idl, tsh, tsl, flags, lenp : int;
	
	If gfd = 0 Then Return;

	Syscall LogLastId(gfd,idh,idl);
	If (idh = 0) && (idl = 0) Then
	Begin
		{ No previous entry - add genesis entry }
		Call GetHash(-1,-1,-1,-1,0,0,bufp);
		Syscall LogWrite(gfd,LogBinary,HdrSize,bufp);
		Syscall LogLastId(gfd,idh,idl);
	End;

	{ Compute header derived from previous entry }
	Syscall LogReadInfo(gfd,idh,idl,lenp,flags,tsh,tsl);
	Call GetHash(idh,idl,tsh,tsl,len,buf,bufp);
	Call Memcpy(bufp + HdrSize,buf,len);
	Syscall LogWrite(gfd,LogBinary,len + HdrSize, bufp);
   End;
   
   Procedure Init()
   Begin
	Call Open("hlc");
   End;

   Procedure Exit()
   Begin
	Call Close();
   End;
   
   { constant values }

End.
