{ -*- text -*- }

Program testService(23333332,1,Service,Init);
Begin

Procedure Service()
Begin
	  var result : integer;
	  
	  Syscall LogStr("Example service routine");
	  Syscall Yield(0, YieldFork, result);
	  If result Then
	  Begin
	     Syscall LogStr("Child from yield");
	  End
	  Else
	  Begin
	     Syscall LogStr("Parent from yield");
	  End;
End;

Procedure Init()
Begin
	Syscall LogStr("testService initializing");
End;
   
End.
