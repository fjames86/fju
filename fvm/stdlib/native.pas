
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
   
   Const YieldFork := 1;
      
   Declare Syscall Puts(str : string) : 1;
   Declare Syscall Rand(var r : integer) : 2;
   Declare Syscall Now(var nowhigh : integer, var nowlow : integer) : 3;
   Declare Syscall LogStr(str : string) : 4;
   Declare Syscall ProgidByName(str : string, var progid : integer) : 5;
   Declare Syscall ProcidByName(progid : integer, str : string, var procid : integer) : 6;
   Declare Syscall WriteLog(logname : string, buf : opaque, count : integer) : 7;
   Declare Syscall ReadLog(logname : string, idhigh : integer, idlow : integer, buf : opaque, var count : integer) : 8;
   Declare Syscall Yield(timeout : integer, flags : integer) : 10;
   
End.
