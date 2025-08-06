package body Basics
  with Refined_State => (State => (The_Rec, The_Table))
is

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
      if I /= J then
         Swap (T (I), T (J));
         pragma Annotate (GNATprove, False_Positive,
                          "formal parameters * might be aliased",
                          "I /= J so T(I) and T(J) cannot alias");
      end if;
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
      T := (others => 0);
      T (T'First) := 1;
      T (T'Last) := 2;
   end Init_Table;

   procedure Init_The_Rec is
   begin
      Init_Rec (The_Rec);
   end Init_The_Rec;

   procedure Init_The_Table is
   begin
      Init_Table (The_Table);
   end Init_The_Table;

   procedure Init_The_State is
   begin
      Init_The_Rec;
      Init_The_Table;
   end Init_The_State;

   procedure Strange_Init_Rec (R : out Rec; Cond : Boolean) is
   begin
      if Cond then
         Init_Rec (R);
      else
         Init_Rec (R);
      end if;
   end Strange_Init_Rec;

   procedure Strange_Init_Table (T : out Table; Val : Integer) is
   begin
      T := (others => 0);
      T (T'First) := Val;
      T (T'First) := 0;
   end Strange_Init_Table;

end Basics;
