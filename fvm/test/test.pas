
{ -*- mode: fvm -*- }

{
 * This module defines a set of tests to check the fvm runtime and compiler are
 * working as expected.
}

Program Test(0,0,Main,TestPars); 
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   
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
   End;
	
   Procedure Main()
   Begin
	var i : int;
	
	Call TestStr();
        Call TestAdd();
	Call TestFreg();

	Syscall Puts("Done");
   End;

Procedure TestPars(p1 : int, p2 : int, p3 : int, p4 : int, p5 : int, p6 : int, p7 : int, p8 : int, p9 : int)
Begin
End;

End.
