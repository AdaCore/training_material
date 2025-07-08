package body Basics is

   procedure Swap (X, Y : in out Integer) is
      Tmp : Integer := X;
   begin
      X := Y;
      Y := Tmp;
   end Swap;

   procedure Swap_Rec (R : in out Rec) is
   begin
      Swap (R.A, R.B);
   end Swap_Rec;

   procedure Swap_Table (T : in out Table; I, J : Index) is
   begin
      Swap (T (I), T (J));
   end Swap_Table;

   procedure Swap_The_Rec is
   begin
      Swap_Rec (The_Rec);
   end Swap_The_Rec;

   procedure Swap_The_Table (I, J : Index) is
   begin
      Swap_Table (The_Table, I, J);
   end Swap_The_Table;

   procedure Init_Rec (R : out Rec) is
   begin
      R.A := 1;
      R.B := 2;
   end Init_Rec;

   procedure Init_Table (T : out Table) is
   begin
      T (T'First) := 1;
      T (T'Last) := 2;
      T (T'First + 1 .. T'Last - 1) := (others => 0);
   end Init_Table;

   procedure Init_The_Rec is
   begin
      Init_Rec (The_Rec);
   end Init_The_Rec;

   procedure Init_The_Table is
   begin
      Init_Table (The_Table);
   end Init_The_Table;

end Basics;
