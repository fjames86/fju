{ -*- mode: fvm -*- }

{
 * This shows how to do some simple operations on pairs of ints as 
 * if they were uint64s.
}
Program Test2(0,0,Main);
Begin

Include "syscall.pas";

{ Take an address which is a pointer to a pair of 2 ints to simulate passing a uint64 }
Procedure ProcWithU64(u64addr : int)
Begin
	u64addr[0] = 0x12344321;
	u64addr[1] = 0xabcddbca;
End;

{ Simple u64 addition function }
Procedure AddU64(u1 : int, u2 : int, result : int)
Begin
	var h, l : int;
	
	l = u1[1] + u2[1];
 	h = u1[0] + u2[0];
	If (l < u1[1]) || (l < u2[1]) Then
	   h = h + 1;
	result[0] = h;
	result[1] = l;
End;

Procedure Main()
Begin
	var high, low : int;
	var u64 : int[2];
	var str : string[64];

	{ pass address of local variable }
	Call ProcWithU64(&high);
	Syscall Sprintf(str,"Test string: %x %x", high,low,0,0);
	Syscall Puts(str);

	{ pass (implicit address of) array of 2 ints }
	Call ProcWithU64(u64);
	Syscall Sprintf(str,"Test string: %x %x", u64[0],u64[1],0,0);
	Syscall Puts(str);

	high = 0;
	low = 0x8f000001;
	Call AddU64(&high, &high, u64);
	Syscall Sprintf(str,"Test Add: %x %x", u64[0],u64[1],0,0);
	Syscall Puts(str);

End;

End.