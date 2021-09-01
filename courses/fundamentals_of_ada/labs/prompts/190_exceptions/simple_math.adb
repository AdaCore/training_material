with Ada.Numerics.Generic_Elementary_Functions;
package body Simple_Math is
  package Math is new Ada.Numerics.Generic_Elementary_Functions (Float_T);

   -- Sqrt should raise our exception when X < 0 and
   -- the square root of X otherwise
   function Sqrt (X : Float_T) return Float_T is
   begin
      return Math.Sqrt (X); -- not fully implemented
   end Sqrt;

   -- Square should raise our exception when X*X is too large and
   -- the X*X otherwise
  function Square (X : Float_T) return Float_T is
  begin
      return X * X;
  end Square;

   function Multiply (L, R : Float_T) return Float_T is
   begin
      return L * R;
   end Multiply;

   function Divide (N, D : Float_T) return Float_T is
   begin
      return N / D;
   end Divide;

end Simple_Math;
