
program bob;
begin

   const fred := 123;
label bob;
   
procedure fred(x : integer) { this is a comment }
begin
   myvar := 123;
   if myvar = 123 Then
      myvar := 321
   else if myvar = 321 Then
      myvar := 111
   else
      myvar := 222;
   
   Call bob(myvar)   
end

end
.
