package body Swap_Generics is

   procedure Swap_Generic (Value_1 : in out Data_Type; Value_2 : in out Data_Type) is
    Tmp : Data_Type;
   begin
      Tmp := Value_1;
      Value_1 := Value_2;
      Value_2 := Tmp;
   end Swap_Generic;

end Swap_Generics;
