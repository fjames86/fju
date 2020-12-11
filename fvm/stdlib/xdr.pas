
{ -*- mode:fvm- -*- }

Procedure XdrEncodeInt(var offset : int, var buf : opaque, val : int)
Begin
	buf[0] = val >> 24;
	buf[1] = val >> 16;
	buf[2] = val >> 8;
	buf[3] = val & 0xff;
	offset = offset + 4;
	buf = buf + 4;
End;

Procedure XdrDecodeInt(var offset : int, var buf : opaque, var val : int)
Begin
	var v : int;
	v = buf[0];
	v = (v << 8) | buf[1];
	v = (v << 8) | buf[2];
	v = (v << 8) | buf[3];
	offset = offset + 4;
	buf = buf + 4;
	val = v;
End;

Procedure XdrEncodeString(var offset : int, var buf : opaque, val : string)
Begin
	var len : int;

	Call Strlen(val,len);
	
	Call XdrEncodeInt(offset,buf,len);
	if len % 4 then len = len + 4 - (len % 4);
	Call Strcpy(buf,str);
	buf = buf + len;
	offset = offset + 4;
End;

Procedure XdrDecodeString(var offset : int, var buf : opaque, val : string)
Begin
	var len : int;

	Call XdrDecodeInt(offset,buf,len);
	Call Strcpy(val,buf);
	if len % 4 then len = len + 4 - (len % 4);
	buf = buf + len;
	offset = offset + 4;
End;

