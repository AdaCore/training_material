with Ada.Unchecked_Deallocation;

package body Pointers is

   procedure Swap (X, Y : not null Int_Acc) is
      Tmp : Integer := X.all;
   begin
      X.all := Y.all;
      Y.all := Tmp;
   end Swap;

   procedure Swap_Ptr (X, Y : in out not null Int_Acc) is
      Tmp : Int_Acc := X;
   begin
      X := Y;
      Y := X;
   end Swap_Ptr;

   procedure Dealloc (X : in out Int_Acc) is
      procedure Free is new Ada.Unchecked_Deallocation (Integer, Int_Acc);
   begin
      Free (X);
   end Dealloc;

   procedure Realloc (X : in out Int_Acc) is
      Value : constant Integer := X.all;
   begin
      Dealloc (X);
      X := Alloc (Value);
   end Realloc;

   procedure Init_List_Zero (L : access List_Cell) is
      B : access List_Cell := L;
   begin
      while B /= null loop
         B.Value := 0;
         B := B.Next;
      end loop;
   end Init_List_Zero;

end Pointers;
