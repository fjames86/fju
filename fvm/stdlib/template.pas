
Program Template(0,0,TestProc);
Begin
   { Includes }
   Include "syscall.pas";

   { constants }
   const EXAMPLE = 132;

   { declarations }
   Declare Procedure ForwardRef(a : u32);
   Declare Const var conststring : string;
      
   { globals }
   var globalvar : u32;

   { procedures }
   Procedure TestProc(input : u32, var output : string)
   Begin
      globalvar = input;
      output = conststring;
   End;

   { constant values }
   Const var conststring = "Hello world";

End.
