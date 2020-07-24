{ -*- mode:text -*- }

Program bob(123123,1, ProcNull, ProcHello);
Begin


Const hellostring := "Hello";

var myglobalint : integer;
var myglobalstring : string[64];

Procedure ProcNull(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	rescount := 0;
	resbuf := 0;
End;

Declare Procedure strcpy( dest : string, source : string);

Procedure ProcHello(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var resb : string[64];

	Call strcpy( resb, hellostring );
	rescount := 10;
	resbuf := resb;
	
End;

Procedure strlen(source : string, var len : integer)
Begin
	ASM "LDSP R0 -8";
	ASM "LD R0 R0";
	ASM "STSP R0 -12";
End;

Procedure strcpy(dest : string, source : string)
Begin
	var p : string;
	var q : string;
	var len : integer;

	Call strlen(source, len);
	len := len + 4;
	q := source;
	p := dest;	
	While len > 0 Do
	Begin
		q := p;
		q := q + 4;
		p := p + 4;
	End;

End;


End.
