{ -*- text -*- }

Program testService(23333332,1,Service,Init);
Begin

Procedure Service()
Begin
{
	  Syscall LogStr("Example service routine");
}
End;

Procedure Init()
Begin
	  var result : integer;

	  Syscall LogStr("testService initializing");

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
   
End.
