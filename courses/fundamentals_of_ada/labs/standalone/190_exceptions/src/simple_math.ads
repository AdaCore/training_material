package Simple_Math is

  type Float_T is digits 6 range -1_000.0 .. 1_000.0;

  Illegal_Operation : exception;

  function Sqrt
   (X : Float_T)
    return Float_T;
  function Square
   (X : Float_T)
    return Float_T;

  function Multiply
   (L, R : Float_T)
    return Float_T;
  function Divide
   (N, D : Float_T)
    return Float_T;

end Simple_Math;
