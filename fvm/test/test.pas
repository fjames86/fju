
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

   const var TestFregPath = "/test";
   
   Procedure TestFreg()
   Begin
	var val, result, entrytype, cont : int;
	var path, name, entryname : string[64];

	val = 0;
	result = 0;
	
	Syscall FregSubkey(TestFregPath);

	Syscall Sprintf(path,"%s/testu32", TestFregPath, 0, 0, 0);
	Syscall FregReadInt(path,val,result);
	if result Then Begin
	   Syscall Sprintf(path,"FregReadInt Success %u", val,0,0,0);
	   Syscall Puts(path);
	End Else Begin
	    Syscall Puts("FregReadInt Failure");
	End;

	Syscall Sprintf(path,"%s/testu32", TestFregPath, 0, 0, 0);
	Syscall FregWriteInt(path,123);

	Syscall Sprintf(path,"%s/teststr", TestFregPath,0,0,0);
	Syscall FregWriteString(path,"testval");

	Call Strcpy(name,"");
	cont = 1;
	Do Begin
	
	   Syscall FregNext(TestFregPath,name,entryname,entrytype,result);
	   If result Then Begin
	       Syscall Sprintf(path,"%s/%s type=%u (%s)",
	       	       TestFregPath,
		       entryname,
		       entrytype,
		       (entrytype = FregTypeU32) ? "U32" :
		        ((entrytype = FregTypeString) ? "String" :
		         ((entrytype = FregTypeOpaque) ? "Opaque" :
		          ((entrytype = FregTypeKey) ? "Key" :
		           "Other"))));
	       Syscall Puts(path);
	       Call Strcpy(name,entryname);
	   End Else Begin
	       cont = 0;
	   End;
		
	End While cont;

	Syscall FregNext("/fju/nls/logs","",entryname,entrytype,result);
	Call Printf("/fju/nls/logs/%s", entryname,0,0,0);
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

   Procedure TestFvmClRun()
   Begin
	Syscall FvmClRun(0,0,"Test","TestXCallCB",0,0);
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
	var buf : opaque[1024];

	Syscall LogLastId(MyLogName,idh,idl);
	Call Printf("%s LastId %x%x", MyLogName,idh,idl,0);
	
	idh = 0;
	idl = 0;
	Do
	Begin
		Syscall LogNext(MyLogName,idh,idl,idh2,idl2);
		If (idh2 | idl2) Then
		Begin

			Syscall LogRead(MyLogName,idh2,idl2,1024,buf,flags,lenp);
			Call Printf("%x%x %s", idh2,idl2,buf,0);		
		End Else Begin
		    Call Printf("Failed to get next log entry for %x%x", idh,idl,0,0);
		End;
		    
		
		idh = idh2;
		idl = idl2;
	End While (idh | idl);

   End;
   
   Procedure Main()
   Begin
	var i : int;
	
	Call TestStr();
        Call TestAdd();
	Call TestFreg();
	Call TestXCall();
	{ Call TestFvmClRun(); }
	Call TestLoop();
	Call TestStrcpy();
	Call TestMemcmp();
        { Call TestLogProp(); } 
	
	Syscall Puts("Done");
   End;


End.
