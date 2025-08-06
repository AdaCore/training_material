with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   --$ begin cut
   type A_F is access function (I : Integer) return Integer
      with Post => A_F'Result > I;
   --$ end cut

   function Plus_One (I : Integer) return Integer is (I + 1)
      with Inline, Static;

   function Minus_One (I : Integer) return Integer is (I - 1)
      with Inline, Static;

   Acc : array (Positive range <>) of A_F := (Plus_One'Access, Minus_One'Access);
begin
   for A of Acc loop
      Put_Line (Integer'Image (A (1)));
   end loop;
end Main;
