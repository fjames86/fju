
{ -*- text -*- }

Program Test5(5,1,Main);
Begin

Procedure Main()
Begin
	var result : integer;
	
	Syscall Yield(0,YieldFork,result);
	If result Then
	Begin
		result := 0;
		Syscall LogStr("Invoking");
		Syscall Invoke(5, 2, 0, 0, 0, result);
	End
	Else
	Begin
		Syscall LogStr("Continuing");
	End;
End;

End.
