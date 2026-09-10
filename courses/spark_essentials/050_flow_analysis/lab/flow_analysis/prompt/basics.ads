package Basics is

   type Rec is record
      A, B : Integer;
   end record;

   type Index is range 1 .. 10;
   type Table is array (Index range <>) of Integer;

   procedure Swap (X, Y : in out Integer);

   The_Rec : Rec;
   The_Table : Table (1 .. 10);

   procedure Swap_Rec_Parameter (R : in out Rec);

   procedure Swap_Table_Parameter (T : in out Table; I, J : Index);

   procedure Swap_Global_Rec;

   procedure Swap_Global_Table (I, J : Index);

   procedure Init_Rec (R : out Rec);

   procedure Init_Table (T : out Table);

   procedure Init_The_Rec;

   procedure Init_The_Table;

end Basics;
