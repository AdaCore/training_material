pragma Unevaluated_Use_Of_Old (Allow);

package Basics is

   type Rec (Disc : Boolean := False) is record
      case Disc is
         when True =>
            A : Integer;
         when False =>
            B : Integer;
      end case;
   end record;

   type Index is range 1 .. 10;
   type Table is array (Index range <>) of Integer;

   procedure Swap (X, Y : in out Integer);

   The_Rec : Rec;
   The_Table : Table (1 .. 10);

   function Value_Rec (R : Rec) return Integer is
     (if R.Disc then R.A else R.B);

   procedure Bump_Rec (R : in out Rec);

   procedure Swap_Table (T : in out Table; I, J : Index);

   procedure Bump_The_Rec;

   procedure Swap_The_Table (I, J : Index);

   procedure Init_Rec (R : out Rec);

   procedure Init_Table (T : out Table);

   procedure Init_The_Rec;

   procedure Init_The_Table;

end Basics;
