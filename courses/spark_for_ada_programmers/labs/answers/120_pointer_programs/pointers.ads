pragma Unevaluated_Use_Of_Old (Allow);

with SPARK.Big_Integers; use SPARK.Big_Integers;

package Pointers is

   type Int_Acc is access Integer;

   procedure Swap (X, Y : not null Int_Acc)
     with Post => X.all = Y.all'Old and then Y.all = X.all'Old;

   procedure Swap_Ptr (X, Y : in out not null Int_Acc)
     with Post => X.all = Y.all'Old and then Y.all = X.all'Old;

   function Alloc (Value : Integer) return Int_Acc is (new Integer'(Value));

   procedure Dealloc (X : in out Int_Acc)
   with
     Depends => (X => null, null => X),
     Post => X = null;

   procedure Realloc (X : in out Int_Acc)
   with
     Pre  => X /= null,
     Post => X.all = X.all'Old;

   type List_Cell;
   --  Forward declaration for record used in list

   type List_Acc is access List_Cell;
   --  Pointer used for linked list of List_Cell

   type List_Cell is record
      Value : Integer;  --  value for cell
      Next  : List_Acc; --  pointer to next item in list
   end record;

   function All_List_Zero (L : access constant List_Cell) return Boolean
   is
     (L = null or else (L.Value = 0 and then All_List_Zero (L.Next)))
   with
     Subprogram_Variant => (Structural => L);
   --  Return True if every item in list L has a value of 0.
   --  Uses recursion to traverse the list, so we need a subprogram_variant
   --  to indicate what object we are recursing on.

   procedure Init_List_Zero (L : access List_Cell)
     with Post => All_List_Zero (L);
   --  Initialize value of every element in list L to 0.
   --  Use All_List_Zero in a postcondtion to state the behavior;

end Pointers;
