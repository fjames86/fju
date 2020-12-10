
{ -*- mode:fvm -*- }

Procedure Memcpy(dlen : u32, dest : opaque, slen : u32, src : opaque)
Begin
	var i : u32;
	var len : u32;

	len = (slen < dlen) ? slen : dlen;
	
	i = 0;
	While i < len Do Begin
	      dest[i] = src[i];
	      i = i + 4;
	End;
End;

Procedure Strcpy(dest : string, src : string)
Begin
	var c, cc, i, j : int;
	var p, q : opaque;

	i = 0;
	While 1 Do Begin
	      c = src[i];
	      dest[i] = src[i];
	      j = 0;
	      While j < 4 Do Begin
	      	      cc = c >> (8 * j);
		      if !cc Then Goto Done;
	      End;
	      i = i + 4;
	End;

Done:

End;

Procedure Strlen(str : string, var len : int)
Begin
	var i : int;
	var c : int;
	
	i = 0;
	While 1 Do Begin
	      c = str[i] & 0xff;
	      If c Then
	      	 i = i + 1
	      Else
		Goto Done;
	End;
Done:
	len = i;
End;

Procedure Strcat(dest : string, src : string)
Begin
	var i : int;
	
	Call Strlen(dest, i);
	Call Strcpy(dest + i, src);
End;

