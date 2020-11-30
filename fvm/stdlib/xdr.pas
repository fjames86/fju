
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
	var strlen : integer;
	var ln : integer;
	var p : opaque;
	var tmpstr : string[256];

	strlen := ^buf;
	If strlen % 4 Then strlen := strlen + 4 - (strlen % 4);
	
	p := val;
	^p := strlen;
	p := p + 4;
	While strlen > 0 Do
	Begin
		If valsize >= 0 Then
		Begin
			^p := ^buf;
		End;
		
		p := p + 4;
		strlen := strlen - 4;
		valsize := valsize - 4;
	End;
	buf := buf + strlen;	
	valsize := strlen;

	Syscall Sprintf(tmpstr,256,"val.len: %d\n", AddressOf strlen);
	Syscall Puts(tmpstr);
End;

End.


