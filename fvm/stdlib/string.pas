
{ -*- mode:fvm -*- }

Procedure Memcpy(dlen : u32, dest : opaque, slen : u32, src : opaque)
Begin
	var i : u32;
	var len : u32;

	len = (slen < dlen) ? slen : dlen;
	
	i = 0;
	While i < len Do Begin
	      dest[i] = src[i];
	      i = i + 1;
	End;
End;

Procedure Strcpy(dest : string, src : string)
Begin
	var i : int;

	i = 0;
	While src[i] Do Begin
	      dest[i] = src[i];
	      i = i + 1;	      
	End;
	dest[i] = 0;
End;

Procedure Strlen(str : string, var len : int)
Begin
	var i : int;

	i = 0;
	While str[i] Do Begin
	      i = i + 1;
	End;

	len = i;
End;

Procedure Strcat(dest : string, src : string)
Begin
	var i : int;
	
	Call Strlen(dest, i);
	Call Strcpy(dest + i, src);
End;

