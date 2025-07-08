package Basics is

   type Rec is record
      A, B : Integer;
   end record;

   type Index is range 1 .. 10;
   type Table is array (Index range <>) of Integer;

   procedure Swap (X, Y : in out Integer)
     with Global => null;

   The_Rec : Rec;
   The_Table : Table (1 .. 10);

   procedure Swap_Rec (R : in out Rec)
     with Global => null;

   procedure Swap_Table (T : in out Table; I, J : Index)
     with Global => null;

   procedure Swap_The_Rec
     with Global => (In_Out => Basics.The_Rec);

   procedure Swap_The_Table (I, J : Index)
     with Global => (In_Out => Basics.The_Table);

   procedure Init_Rec (R : out Rec)
     with Global => null;

   procedure Init_Table (T : out Table)
     with Global => null;

   procedure Init_The_Rec
     with Global => (Output => Basics.The_Rec);

   procedure Init_The_Table
     with Global => (Output => Basics.The_Table);

end Basics;
