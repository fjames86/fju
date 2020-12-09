
{ -*- text -*- }

Const LogLvlTrace = 0x00;
Const LogLvlDebug = 0x01;
Const LogLvlInfo = 0x02;
Const LogLvlWarn = 0x03;
Const LogLvlError = 0x04;
Const LogLvlFatal = 0x05;
Const LogBinary = 0x10;
Declare Syscall LogWrite(flags : u32, len : u32, buf : opaque) : 1;

Declare Syscall LogReadNexteidHigh : u32, idLow : u32, var len : u32, var buf : opaque, var eidHigh : u32, var eIdLow : u32) : 2;
