
Program EventLog(2300000,1,EventCallback);
Begin

Procedure EventCallback(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
   var str : string[64];
      { xxxx todo }
      Syscall Sprintf(str,64,"Event cat=%u", cat);
   Syscall LogStr(str);
End;
   
End.
