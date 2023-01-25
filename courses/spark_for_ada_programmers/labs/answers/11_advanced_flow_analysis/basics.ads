package Basics is

   type Rec is record
      A, B : Integer;
   end record;

   type Index is range 1 .. 10;
   type Table is array (Index range <>) of Integer;

   procedure Swap (X, Y : in out Integer)
   with
     Global  => null,
     Depends => (X => Y, Y => X);

   The_Rec : Rec;
   The_Table : Table (1 .. 10);

   procedure Swap_Rec (R : in out Rec)
   with
     Global  => null,
     Depends => (R => +null);

   procedure Swap_Table (T : in out Table; I, J : Index)
   with
     Global  => null,
     Depends => (T => +(I, J));

   procedure Swap_The_Rec
   with
     Global  => (In_Out => The_Rec),
     Depends => (The_Rec => +null);

   procedure Swap_The_Table (I, J : Index)
   with
     Global  => (In_Out => The_Table),
     Depends => (The_Table => +(I, J));

   procedure Init_Rec (R : out Rec)
   with
     Global  => null,
     Depends => (R => null);

   procedure Init_Table (T : out Table)
   with
     Global  => null,
     Depends => (T => +null);

   procedure Init_The_Rec
   with
     Global  => (Output => The_Rec),
     Depends => (The_Rec => null);

   procedure Init_The_Table
   with
     Global  => (Output => The_Table),
     Depends => (The_Table => null);

   procedure Strange_Init_Rec (R : out Rec; Cond : Boolean)
   with
     Global  => null,
     Depends => (R => Cond);

   procedure Strange_Init_Table (T : out Table; Val : Integer)
   with
     Global => null,
     Depends => (T => +Val);

end Basics;
