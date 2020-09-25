{ -*- text -*- }

Program TestReg(111111,1,Main);
Begin

Procedure Memcpy(dest : opaque, source : opaque, size : integer)
Begin
	var i : integer;
	Do
	Begin
		^dest := ^source;
		dest := dest + 1;
		source := source + 1;
		i := i + 1;
	End
	While i < size;

End;

Procedure Memset(dest : opaque, val : integer, size : integer)
Begin
	var i : integer;
	val := (val * 256) or val;
	val := (val * 256) or val;
	val := (val * 256) or val;

	i := 0;
	Do
	Begin
		^dest := val;
		dest := dest + 4;
		i := i + 4;
	End
	While i < size;
End;

Procedure Main(	   argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque)
Begin
   var u : integer;
   var str : string[64];
   var size : integer;
   var key : opaque[16];
   var buf : opaque[256];
   
   
   Syscall ReadRegInt("/testint", u);
   u := u + 1;
   Syscall WriteRegInt("/testint", u);

   Syscall ReadRegString("/teststr", str, 64 );
   Syscall Puts(str);
   Syscall WriteRegString("/teststr", "my new value" );

   { ------------------------------ }
   
   Call Memset(key, 0xaa, 16 );
   size := 256;
   Syscall ReadCht(key,buf,size);
   
   If size > 0 Then
   Begin
      Syscall Puts("Successfully read from CHT\n");
      Call Memset(key, 0xbb, 16);
      Syscall WriteCht(key,buf,size);
   End
   Else
   Begin
	Syscall Puts("Failed to read from CHT\n")
   End;
	
End;
   
End.
