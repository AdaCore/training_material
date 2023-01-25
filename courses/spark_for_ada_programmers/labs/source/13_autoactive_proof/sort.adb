package body Sort is

   -- Swap the values of Values (X) and Values (Y)
   procedure Swap (Values : in out Nat_Array; X, Y : Index) is
      Temp        : Integer;
      Temp_Index  : Index with Ghost;
   begin
      Temp       := Values (X);
      Values (X) := Values (Y);
      Values (Y) := Temp;
   end Swap;

   -- Finds the index of the smallest element in the slice Values (From .. To)
   function Index_Of_Minimum (Values : Nat_Array; From, To : Index) return Index is
      Min : Index := From;
   begin
      for Index in From .. To loop
         if Values (Index) < Values (Min) then
            Min := Index;
         end if;
      end loop;

      return Min;
   end Index_Of_Minimum;

   procedure Selection_Sort (Values : in out Nat_Array) is
      Smallest : Index;  -- Index of the smallest value in the unsorted part
   begin
      for Current in 1 .. Values'Last - 1 loop
         Smallest := Index_Of_Minimum (Values, Current, Values'Last);

         if Smallest /= Current then
            Swap (Values => Values,
                  X      => Current,
                  Y      => Smallest);
         end if;
      end loop;
   end Selection_Sort;

end Sort;
