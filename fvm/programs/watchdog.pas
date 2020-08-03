
Program Watchdog(PROG_WATCHDOG,1,Service);
Begin

   var LastTick : integer;

   Procedure Service()
   Begin
      var high, low : integer;
      var str : string[256];
	 
      Syscall Now(high, low);

      If (low - LastTick) > 2000 Then
      Begin
	 low := low - LastTick - 1000;
	 Syscall Sprintf(str,256,"Watchdog detected latency of %ums", AddressOf low);
	 Syscall LogStr(str);
      End;

      LastTick := low;
   End;
   
End.
