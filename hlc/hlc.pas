
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
	If lenpp > 0 Then
	Begin
		bufp = buf + HdrSize;
		lenp = lenpp - HdrSize;
	End;
   End;

   Procedure Write(len : int, buf : opaque)
   Begin
	var bufp : opaque[2048];

	If gfd = 0 Then Return;

	{ TODO: compute sha1(id,seq,buf) instead of sha1(buf) }
	Syscall Sha1(len,buf,bufp);	
	Call Memcpy(bufp + HdrSize,buf,len);
	Syscall LogWrite(gfd,LogBinary,len + HdrSize, bufp);
   End;
   
   Procedure Init()
   Begin
	Call Open("hlc.log");
   End;

   Procedure Exit()
   Begin
	Call Close();
   End;
   
   { constant values }

End.
