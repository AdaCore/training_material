package body Basics is

   procedure Swap (X, Y : in out Integer) is
      Tmp : Integer := X;
   begin
      X := Y;
      Y := Tmp;
   end Swap;

   procedure Bump_Rec (R : in out Rec) is
   begin
      case R.Disc is
         when True =>
            R.A := R.A + 1;
         when False =>
            R.B := R.B + 1;
      end case;
   end Bump_Rec;

   procedure Swap_Table (T : in out Table; I, J : Index) is
   begin
      if I /= J then
         Swap (T (I), T (J));
         pragma Annotate (GNATprove, False_Positive,
                          "formal parameters * might be aliased",
                          "I /= J so T(I) and T(J) cannot alias");
      end if;
   end Swap_Table;

   procedure Init_Rec (R : out Rec) is
   begin
      case R.Disc is
         when True =>
            R := (Disc => True, A => 1);
         when False =>
            R := (Disc => False, B => 1);
      end case;
   end Init_Rec;

   procedure Init_Table (T : out Table) is
   begin
      T := (others => 0);
      T (T'First) := 1;
      T (T'Last) := 2;
   end Init_Table;

end Basics;
