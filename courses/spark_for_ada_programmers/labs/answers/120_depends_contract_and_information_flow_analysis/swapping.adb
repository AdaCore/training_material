package body Swapping
is

   procedure Swap (X, Y: in out Positive)
   is
      Tmp: Positive;
   begin
      Tmp := X;
      X   := Y;
      Y   := Tmp;
   end Swap;

   procedure Identity (X, Y: in out Positive)
   is
   begin
      Swap (X, Y);
      Swap (Y, X);
   end Identity;

end Swapping;
