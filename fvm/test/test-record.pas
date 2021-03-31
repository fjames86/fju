
{ -*- mode: fvm -*- }

Program TestRecords(0,0,TestProc);
Begin
   { Includes }
   Include "syscall.pas";

   { constants }

   { declarations }
   Const clen = 12;
   
   Record fred =
   	  a : int;
	  b : string;
	  c : int[clen];
	  d : opaque[3];
   End;
	  
   { globals }

   { procedures }
   Procedure TestProc(var result : int)
   Begin
	var b : fred[2];
	var bp : ^fred;

	bp = b[1];
	bp.a = 123;
	result = bp.a;
	
   {
	var b : fred;
	var bp : opaque;
	var a : int;

	bp = b;
	*(bp + OffsetOf(fred.a)) = *(bp + OffsetOf(fred.a));
}	
	{ 
          * Assign to a specific offset into an opaque array. 
  	  * This always assigns a full word i.e. a u32 assignment.
	}
	{ *(b + OffsetOf(fred.d)) = 123; }

	{ Assign to a record field }
	{ b.d[2] = 123;} 
	{ b.c[2] = 321;}
	
	{ Assign to a record field from a record field }
	{ 	b.a = b.a; }

{
	b.a = 123;
	b.b = "fred";
	b.c[0] = b.a;
	b.d[1] = 321;
	result = b.a;
}
{
	Syscall RpcCall(12,12,10000,2,1,0,0,"resultproc");
}
   End;


   { constant values }

End.
