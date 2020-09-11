package body Max is

   function Arrays_Max (A : in Our_Array) return Index_Range is
      X : Index_Range := Index_Range'First;
      Y : Index_Range := Index_Range'Last;
   begin
      while X /= Y loop
         --  Write a suitable Loop_Invariant to help prove the postcondition
         --  Write a suitable Loop_Variant to help prove loop termination

         if A (X) <= A (Y) then
            X := X + 1;
         else
            Y := Y - 1;
         end if;
      end loop;
      return X;
   end Arrays_Max;

end Max;
