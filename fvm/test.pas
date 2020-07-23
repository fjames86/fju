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
   var mystr : string[64];
   
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
end;

procedure myfunc(x : integer, var y : integer)
begin
	var fred : integer;	
	localv1 := x;
	y := localv1;
	fred := 0;

	while fred < 10 do
	begin
		fred := fred + 1
	end;

	fred := 0;
	do
	fred := fred + 1
	while fred < 12

end





end.
