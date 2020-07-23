{ -*- mode:text -*- }

program bob(123123,1);
begin

   const fred := 123;
   const conststr := "fred";
   var globu32 : integer := 123;
   var globalstr : string[64];
   
procedure procname(x : integer, var y : integer) { this is a comment }
begin
   var local : integer;
      
   myvar := 123; 
   if myvar = 123 Then
      myvar  := 321
   else if myvar > 321 Then
      myvar  := 111
   else if myvar <> 321 Then
      goto fred2
   else
      myvar := 222;

   while myvar = 123 do
   begin
     myvar := 12 xor 123
   end;
   
 fred: 
     Call bob(myvar);
    goto fred1
end



end.
