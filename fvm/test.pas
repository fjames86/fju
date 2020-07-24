{ -*- mode:text -*- }

Program bob(123123,1, ProcNull, ProcHello);
Begin


Const hellostring := "Hello";

Procedure ProcNull(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	rescount := 0;
	resbuf := 0;
End;

Declare Procedure strcpy( var dest : string, source : string);

Procedure ProcHello(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var resb : string[64];

	Call strcpy( resb, hellostring );
	rescount := 10;
	resbuf := resb;
	
End;




End.
