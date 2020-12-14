
{ -*- mode:fvm -*- }

Procedure Memcpy(dest : opaque, src : opaque, len : int)
Begin
	var i : u32;

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

Procedure Strcmp(str1 : string, str2 : string, var result : int)
Begin
	var i : int;
	i = 0;
	While ((str1[i] = str2[i]) && str1[i] && str2[i]) Do
	Begin
		i = i + 1;
	End;
	If str1[i] || str2[i] Then result = 0 Else result = 1;
End;

