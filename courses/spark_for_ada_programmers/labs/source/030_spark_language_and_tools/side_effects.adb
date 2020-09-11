with Simple_IO;

package body Side_Effects
  with SPARK_Mode => On
is
   X, Y, Z, R : Integer;

   function F (X : Integer) return Integer is
   begin
      Z := 0; -- Side effect
      return  X + 1;
   end F;

   procedure Test is
   begin
      X := 10;
      Y := 20;
      Z := 10;

      R := Y / Z + F (X); -- possible order dependency here.

      Simple_IO.Put_Line (R);  -- R = 13 if L->R evaluation,
			       -- constraint error if R->L
   end Test;

end Side_Effects;
