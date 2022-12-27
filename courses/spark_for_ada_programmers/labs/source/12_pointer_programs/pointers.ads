pragma Unevaluated_Use_Of_Old (Allow);

with SPARK.Big_Integers; use SPARK.Big_Integers;

package Pointers is

   type Int_Acc is access Integer;

   procedure Swap (X, Y : not null Int_Acc);

   procedure Swap_Ptr (X, Y : in out not null Int_Acc);

   function Alloc (Value : Integer) return Int_Acc is (new Integer'(Value));

   procedure Dealloc (X : in out Int_Acc)
   with
     Depends => (X => null, null => X);

   procedure Realloc (X : in out Int_Acc)
   with
     Pre  => X /= null,
     Post => X.all = X.all'Old;

   type List_Cell;
   type List_Acc is access List_Cell;
   type List_Cell is record
      Value : Integer;
      Next  : List_Acc;
   end record;

   function At_End
     (L : access constant List_Cell) return access constant List_Cell
   is (L)
   with
     Ghost,
     Annotate => (GNATprove, At_End_Borrow);

   function All_List_Zero (L : access constant List_Cell) return Boolean
   is
     (L = null or else (L.Value = 0 and then All_List_Zero (L.Next)))
   with
     Annotate => (GNATprove, Always_Return),
     Subprogram_Variant => (Structural => L);

   procedure Init_List_Zero (L : access List_Cell)
     with Post => All_List_Zero (L);

end Pointers;
