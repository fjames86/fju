{ -*- text -*- }

Program testRpc(2444444,1,ProcNull,ProcHello,ProcEcho,ProcCounter);
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
	
	rescount := ^p; { get string length }
	rescount := rescount + 4;
	resbuf := HelloWorld;
End;


Procedure ProcEcho(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	rescount := argcount;
	resbuf := argbuf;
End;

var counter : integer;

Procedure ProcCounter(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	counter := counter + 1;

	resbuf := address counter;
	rescount := 4;
End;




End.
