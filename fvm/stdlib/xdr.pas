
{ -*- mode:fvm -*- }

Procedure XdrEncodeU32(buf : opaque, var offset : int, val : int)
Begin
	buf[offset + 0] = val >> 24;
	buf[offset + 1] = val >> 16;
	buf[offset + 2] = val >> 8;
	buf[offset + 3] = val & 0xff;
	offset = offset + 4;
End;

Procedure XdrDecodeU32(buf : opaque, var offset : int, var val : int)
Begin
	var v : int;
	v = buf[offset];
	v = (v << 8) | buf[offset + 1];
	v = (v << 8) | buf[offset + 2];
	v = (v << 8) | buf[offset + 3];
	offset = offset + 4;
	val = v;
End;

Procedure XdrEncodeString(buf : opaque, var offset : int, str : string)
Begin
	var len : int;

	Call Strlen(str,len);
	
	Call XdrEncodeU32(buf,offset,len);
	if len % 4 then len = len + 4 - (len % 4);
	Call Strcpy(buf + offset,str);
	offset = offset + len;
End;

Procedure XdrDecodeString(buf : opaque, var offset : int, str : string)
Begin
	var len : int;

	Call XdrDecodeU32(offset,buf,len);
	Call Strcpy(str,buf + offset);
	if len % 4 then len = len + 4 - (len % 4);
	offset = offset + len;
End;

Procedure XdrEncodeU64(buf : opaque, var offset : int, h : int, l : int)
Begin
	Call XdrEncodeU32(buf,offset,h);
	Call XdrEncodeU32(buf,offset,l);
End;

Procedure XdrDecodeU64(buf : opaque, var offset : int, var h : int, var l : int)
Begin
	Call XdrDecodeU32(buf,offset,h);
	Call XdrDecodeU32(buf,offset,l);
End;

Procedure XdrEncodeOpaque(buf : opaque, var offset : int, len : int, bufp : opaque)
Begin
	Call XdrEncodeU32(buf,offset,len);
	Call Memcpy(buf + offset,bufp,len);
	if len % 4 then len = len + 4 - (len % 4);	
	offset = offset + len;
End;

Procedure XdrDecodeOpaque(buf : opaque, var offset : int, var lenp : int, bufp : opaque)
Begin
	var len : int;

	Call XdrDecodeU32(offset,buf,len);
	Call Memcpy(bufp,buf + offset,len);
	lenp = len;
	if len % 4 then len = len + 4 - (len % 4);
	offset = offset + len;
End;
