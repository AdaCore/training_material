package body Swaps is

   procedure Swap (Value_1 : in out Integer; Value_2 : in out Integer) is
      Tmp : Integer;
   begin
      Tmp := Value_1;
      Value_1 := Value_2;
      Value_2 := Tmp;
   end Swap;

end Swaps;
