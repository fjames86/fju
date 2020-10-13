{ -*- text -*- }

Declare
Begin

   Const NativeNop := 0;
   Const NativePuts := 1;
   Const NativeRand := 2;
   Const NativeNow := 3;
   Const NativeLogStr := 4;
   Const NativeProgidByName := 5;      
   Const NativeProcidByName := 6;
   Const NativeWriteLog := 7;
   Const NativeReadLog := 8; 
   Const NativeYield := 10;
   Const NativeInvoke := 11;
   Const NativeReadRegInt := 12;
   Const NativeReadRegString := 13;
   Const NativeWriteRegInt := 14;
   Const NativeWriteRegString := 15;
   
   Const YieldFork := 1;
      
   Declare Syscall Puts(str : string) : 1;
   Declare Syscall Rand(var r : integer) : 2;
   Declare Syscall Now(var nowhigh : integer, var nowlow : integer) : 3;
   Declare Syscall LogStr(str : string) : 4;
   Declare Syscall ProgidByName(str : string, var progid : integer) : 5;
   Declare Syscall ProcidByName(progid : integer, str : string, var procid : integer) : 6;
   Declare Syscall WriteLog(logname : string, buf : opaque, count : integer) : 7;
   Declare Syscall ReadLog(logname : string, idhigh : integer, idlow : integer, buf : opaque, var count	: integer) : 8;
   Declare Syscall Sprintf(dest : string, destsize : integer, fmt : string, args : opaque) : 9;
   Declare Syscall Yield(timeout : integer, flags : integer, var result : integer) : 10;
   Declare Syscall Invoke(progid : integer, procid : integer, args : opaque, argcount : integer, res : opaque, var rescount : integer) : 11;   
   Declare Syscall ReadRegInt(path : string, var int : integer) : 12;
   Declare Syscall ReadRegString(path : string, str : string, size : integer ) : 13;
   Declare Syscall WriteRegInt(path : string, int : integer ) : 14;
   Declare Syscall WriteRegString(path : string, str : string) : 15;
   Declare Syscall ReadCht(key : opaque, buf : opaque, var size : integer) : 16;
   Declare Syscall WriteCht(key : opaque, buf : opaque, size : integer) : 17;
   Declare Syscall NextLogEntry(logname : string, var idhigh : integer, var idlow : integer, var result : integer) : 18;
   Declare Syscall LogDebug(str : string) : 19;
   Declare Syscall LogWarn(str : string) : 20;
   Declare Syscall LogError(str : string) : 21;   
   Declare Syscall NextRegEntry(path : string, name : string, var type : integer) : 22;
   
End.
