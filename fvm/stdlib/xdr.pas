
{ -*- text -*- }

Declare
Begin

Procedure EncodeInteger(var buf : opaque, val : integer)
Begin
	  ^buf := val;
	  buf := buf + 4;
End;

Procedure DecodeInteger(var buf : opaque, var val : integer)
Begin
	val := ^buf;
	buf := buf + 4;
End;

Procedure EncodeString(var buf : opaque, val : string)
Begin
	var p : opaque;
	var len : integer;

	p := val;
	len := ^p;
	
	^buf := len;
	buf := buf + 4;

	While len > 0 Do
	Begin
		^p := ^buf;
		p := p + 4;
		buf := buf + 4;
		len := len - 4;
	End;
End;

Procedure DecodeString(var buf : opaque, val : string, var valsize : integer)
Begin
	var len : integer;
	var ln : integer;
	var p : opaque;

	len := ^buf;
	If len % 4 Then len := len + 4 - (len % 4);
	
	If len > valsize Then
	   len := valsize;
	ln := len - valsize;

	p := val;
	^p := len;
	p := p + 4;
	While len > 0 Do
	Begin
		^p := ^buf;
		p := p + 4;
		buf := buf + 4;
		len := len - 4;
	End;
	buf := buf + ln;
	
	valsize := len;
End;

End.


