with Ada.Numerics.Generic_Elementary_Functions;
package body Simple_Math is

  package Math is new Ada.Numerics.Generic_Elementary_Functions (Float_T);

  function Sqrt
   (X : Float_T)
    return Float_T is
   (if X >= 0.0 then Math.Sqrt (X)
    else raise Illegal_Operation with "negative number");
  function Square
   (X : Float_T)
    return Float_T is
  begin
    if abs (X) < 1.0 then
      return X * X;
    elsif (Float_T'Last / X) < X then
      raise Illegal_Operation with "number too large";
    else
      return X * X;
    end if;
  end Square;

  function Multiply
   (L, R : Float_T)
    return Float_T is (L * R);
  function Divide
   (N, D : Float_T)
    return Float_T is (N / D);

end Simple_Math;
