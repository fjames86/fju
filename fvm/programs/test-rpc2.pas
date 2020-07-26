{ -*- text -*- }

Program testRpc(444444,1,ProcNull,ProcHello,ProcCounter);
Begin

Procedure ProcNull(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	rescount := 0;
	resbuf := 0;
End;

Const HelloWorld := "Hello, World!";

Procedure ProcHello(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var p : opaque;
	p := HelloWorld;
	
	rescount := ^p;
	resbuf := HelloWorld;
End;

var counter : integer;

Procedure ProcCounter(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var p : opaque[4];
	var pp : opaque;

	pp := p;
	^pp := counter;

	counter := counter + 1;

	resbuf := p;
	rescount := 4;
End;




End.
