
{ -*- mode: fvm -*- }

{
 * This module defines a set of tests to check the fvm runtime and compiler are
 * working as expected.
}

Program Test(0,0,Main,TestXCallCB); 
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";

   Declare Procedure Test/TestXCallCB(var s : string);

   Const testexpr1 = 0x11110000;
   Const testexpr = (testexpr1 & 0xffff0000) | 0x0001;

   { procedures }
   Procedure TestAdd()
   Begin
	var a, b : int;
	var str : string[64];
	
	a = 1;
	b = 2;
	if (a + b) <> 3 then
	Begin
	   Syscall Puts("1 + 2 <> 3");
	End;

	a = 3;
	b = 5;
	if (a - b) <> -2 Then Begin
	   Syscall Puts("3 - 5 <> -2");
	End;

	a = 3;
	b = 5;
	if (a * b) <> 15 Then Begin
	   Syscall Puts("3 * 5 <> 15");
	End;

	a = 15;
	b = 5;
	if (a / b) <> 3 Then Begin
	   Syscall Puts("15 / 5 <> 3");
	End;

	a = 15;
	b = 6;
	if (a % b) <> 3 Then Begin
	   Syscall Sprintf(str,"15 %% 6 = %d", a % b, 0,0,0);
	   Syscall Puts(str);
	   Syscall Puts("15 % 6 <> 3");
	End;
	
   End;

   Procedure TestStr()
   Begin
	var str : string[64];
	
	Syscall Puts("Puts(TestAdd)");
	Syscall Sprintf(str,"Test format u32=%u 0x%x str=%s",
		123, 0xdeadbeef, "Hello", 0);
	Syscall Puts(str);

   End;

   Procedure TestXCallCB(var s : string)
   Begin
	s = "Hello from xcall";
   End;
	
   Procedure TestXCall()
   Begin
	var s : string;
	Call Test/TestXCallCB(s);
	Syscall Puts(s);
   End;


   Procedure TestLoop()
   Begin
	var i : int;
	var str : string[64];

	Syscall Puts("TestLoop Start");
	i = 0;
	while i < 10 Do
	Begin
	      If i % 2 Then Begin
	      	 i = i + 1;
		 Continue;
	      End;
	      
	      If i = 8 Then Break;
	      i = i + 1;
	End;

	Syscall Puts("TestLoop Done");
   End;

   Procedure TestStrcpy()
   Begin
	var s : string[64];
	var result : int;
	
	Call Strcpy(s,"Hello");
	Call Strcmp(s,"Hello",result);
	If result Then
	   Syscall Puts("String compare success")
	Else
	   Syscall Puts("String compare failure");
	   
   End;

    Procedure TestMemcmp()
    Begin
        var p1 : opaque[32];
        var p2 : opaque[32];
   	var r : int;
	
   	Call Memset(p1,0,32);
   	Call Memset(p2,1,32);
   	Call Memcmp(p1,p2,32,r);
   	If r Then Syscall Puts("Memcmp shows same (incorrect!)") Else Syscall Puts("Memcmp shows different (correct)");

   	Call Memset(p1,1,32);
   	Call Memcmp(p1,p2,32,r);
   	If r Then Syscall Puts("Memcmp shows same (correct)") Else Syscall Puts("Memcmp shows different (incorrect!)");
    End;

    Const var MyLogName = "fred";
    
   Procedure TestLogProp()
   Begin
	var idh, idl : int;
	var idh2, idl2 : int;
	var flags, lenp : int;
	var logh : int;
	var buf : opaque[1024];

	Syscall LogOpen(MyLogName,logh);
	
	Syscall LogLastId(logh,idh,idl);
	Call Printf("%s LastId %x%x", MyLogName,idh,idl,0);
	
	idh = 0;
	idl = 0;
	Do
	Begin
		Syscall LogNext(logh,idh,idl,idh2,idl2);
		If (idh2 | idl2) Then
		Begin

			Syscall LogRead(logh,idh2,idl2,1024,buf,flags,lenp);
			Call Printf("%x%x %s", idh2,idl2,buf,0);		
		End Else Begin
		    Call Printf("Failed to get next log entry for %x%x", idh,idl,0,0);
		End;
		    
		
		idh = idh2;
		idl = idl2;
	End While (idh | idl);

	Syscall LogClose(logh);
   End;
   
   Procedure Main()
   Begin
	var i : int;
	
	Call TestStr();
        Call TestAdd();
	Call TestXCall();
	Call TestLoop();
	Call TestStrcpy();
	Call TestMemcmp();
	Call TestLogProp(); 
	
	Syscall Puts("Done");
   End;


End.
