package body Basics is

   procedure Bump_Pair (P : in out Pair) is
   begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
   end Bump_Pair;

   procedure Swap_Pair (P : in out Pair) is
      Tmp : Integer := P.X;
   begin
      P.X := P.Y;
      P.Y := Tmp;
   end Swap_Pair;

   procedure Bump_Triplet (T : in out Triplet) is
   begin
      T.A := T.A + 1;
      T.B := T.B + 1;
   end Bump_Triplet;

   procedure Swap_Triplet (T : in out Triplet) is
   begin
      T.A := T.B;
      T.B := T.C;
      T.C := T.A;
   end Swap_Triplet;

   procedure Bump_And_Swap_Pair (P : in out Pair) is
   begin
      P.Bump_Pair;
      P.Swap_Pair;
   end Bump_And_Swap_Pair;

   procedure Bump_And_Swap_Triplet (T : in out Triplet) is
   begin
      T.Bump_Triplet;
      T.Swap_Triplet;
   end Bump_And_Swap_Triplet;

end Basics;
