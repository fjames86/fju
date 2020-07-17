
program bob;
begin
   
procedure fred(x : integer) { this is a comment }
begin
   myvar := 123;
   if myvar = 123 Then
      myvar := 321
   elseif myvar = 321 Then
      myvar := 111
   else
      myvar := 222
   EndIf;
   
   Call bob(myvar)   
end

end
.
