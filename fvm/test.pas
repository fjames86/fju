
program bob(123123,1);
begin

   const fred := 123;
   const conststr := "fred";
   var globu32 : integer := 123;
   var globalstr : string[64];
   
procedure fred(x : integer) { this is a comment }
begin
   var local : integer;
      
   myvar := 123; 
   if myvar = 123 Then
      myvar  := 321
   else if myvar = 321 Then
      myvar  := 111
   else if myvar <> 321 Then
      goto fred
   else
      myvar := 222;

 fred: 
     Call bob(myvar);
    goto fred 
end

end
.
