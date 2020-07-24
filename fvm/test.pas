{ -*- mode:text -*- }

Program bob(123123,1, ProcNull, ProcHello);
Begin


Const hellostring := "Hello";
Const MYCONSTANT := 123;

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
	len := len + MYCONSTANT;
	q := source;
	p := dest;	
	While len > 0 Do
	Begin
		q := p;
		q := q + 4;
		p := p + 4;
	End;

End;

Procedure Syscall(id : integer, var result : integer)
Begin
	ASM "LDSP	R0	-12";
	ASM "POP	R7";
	ASM "CALLNAT 	R0	R1";
	ASM "PUSH	R7";
	ASM "STSP	R0	-8"
End;

Const NativeYield := 12;

Procedure Yield(timeout : integer, flags : integer)
Begin
	var result : integer;
	var argtimeout : integer;
	var argflags : integer;

	argtimeout := timeout;
	argflags := flags;
	
	Call Syscall(NativeYield, result);
End;

End.
