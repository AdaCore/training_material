procedure Main is
   --$ begin cut
   type Arr is array (1 .. 2) of Integer;
   A : Arr := [3, 4];
   B : Arr := [A with delta 1 => 0];

   type Rec is record
      I1, I2 : Integer;
   end record;
   C : Rec := (I1 => 3, I2 => 4);
   D : Rec := (C with delta I1 => 0);
   --$ end cut
begin
   null;
end Main;
