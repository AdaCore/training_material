package Ops is
   type Op_Kind is (Increment, Decrement, Double, Half);

   procedure Apply
     (Op :        Op_Kind;
      X  : in out Integer);
end Ops;
