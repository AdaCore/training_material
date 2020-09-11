package body Max is

   function Arrays_Max (A : in Our_Array) return Index_Range is
      X : Index_Range := Index_Range'First;
      Y : Index_Range := Index_Range'Last;

      function max (L, R : Content_Range) return Content_Range is
        (if L > R then L else R) with
         Ghost;
   begin
      while X /= Y loop
         --  Write a suitable Loop_Invariant to help prove the postcondition
         --  Write a suitable Loop_Variant to help prove loop termination
         pragma Loop_Invariant
           (
            -- for all values up to X, the value is <= the larger of the value at
            -- x and the value at y
            (for all k in Index_Range'first .. X => max (a (x), a (y)) >= a(k) ) and
            -- for all values down to Y, the value is <= the larger of the value
            -- at x and the value at y
            (for all k in Y .. Index_Range'last =>  max (a (x), a (y)) >= a(k) ) and
            ( X < Y ) );
         pragma Loop_Variant (decreases => Y - X);

         if A (X) <= A (Y) then
            X := X + 1;
         else
            Y := Y - 1;
         end if;

      end loop;
      return X;
   end Arrays_Max;

end Max;
