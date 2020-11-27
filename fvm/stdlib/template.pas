
{ -*- text -*- }

{ *
  * This file provides a template for program skeletons.
  * Compile with: fvmc -o template.fvm native.pas template.pas 
  * 
}

Program Template(10000,1,ProcNull,ProcTemplate);
Begin

Procedure localProc()
Begin
	{ Example private function that writes a log message }
	LogStr( "Hello, World" );
End;

{ Example null procedure that does nothing } 
Procedure ProcNull(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	rescount := 0;
	resbuf := 0;
End;

const HelloWorld := "Hello, World!";

{ example procedure that echo the string "hello world" }
Procedure ProcTemplate(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var p : opaque;
	
	p := HelloWorld;	
	rescount := ^p; { get string length }
	rescount := rescount + 4;
	resbuf := HelloWorld;
End;



End.


