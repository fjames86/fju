
{ -*- mode: fvm -*- }

Program TestSec(0,0,TestHash,TestEncrypt,TestDecrypt);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   
   { constants }

   { declarations }
      
   { globals }

   { procedures }
   Procedure TestHash(len : int, buf : opaque, var hashlen : int, var hashbuf : opaque)
   Begin
	var hash : opaque[SHA1HashSize];
	
	Syscall SHA1(len,buf,hash);
	hashlen = SHA1HashSize;
	hashbuf = hash;
   End;

   Procedure TestEncrypt(klen : int, key : opaque, len : int, buf : opaque, var elen : int, var ebuf : opaque)
   Begin
	var ebufp : opaque[256];
	var str : string[256];

	If len > 256 Then len = 256;
	If len % AesKeySize Then len = len - (len % AesKeySize);
	If len < 0 Then len = 0;
	Call Memcpy(ebufp, buf, len);

	If klen < AesKeySize Then Begin
	   Syscall Puts("bad klen");
	   Return;
	End;

	Syscall Sprintf(str,"len=%u klen=%u", len,klen,0,0);
	Syscall Puts(str);

	Syscall AesEncrypt(len,ebufp,key);
	elen = len;
	ebuf = ebufp;
   End;

   Procedure TestDecrypt(klen : int, key : opaque, len : int, buf : opaque, var elen : int, var ebuf : opaque)
   Begin
	var ebufp : opaque[256];

	If len > 256 Then len = 256;
	Call Memcpy(ebufp, buf, len);
	Syscall AesDecrypt(len,ebufp,key);
	elen = len;
	ebuf = ebufp;
   End;
   
   { constant values }

End.
