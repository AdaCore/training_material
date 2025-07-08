package body Sort is

   procedure Swap (Values : in out Nat_Array; X, Y : Index)
   with
     Pre  => X /= Y,
     Post => Values = (Values'Old with delta
                         X => Values'Old (Y),
                         Y => Values'Old (X))
       and then Permutation = (Permutation'Old with delta
                                 X => Permutation'Old (Y),
                                 Y => Permutation'Old (X));
   --  Swap the values of Values (X) and Values (Y).
   --  Also update the ghost object.

   function Index_Of_Minimum (Values : Nat_Array; From, To : Index) return Index
   with
     Pre  => To in From .. Values'Last,
     Post => Index_Of_Minimum'Result in From .. To and then
       (for all I in From .. To =>
          Values (Index_Of_Minimum'Result) <= Values (I));
   -- Finds the index of the smallest component in the slice Values (From .. To)

   procedure Swap (Values : in out Nat_Array; X, Y : Index)
   is
      Temp        : Integer;
      Temp_Index  : Index with Ghost;
   begin
      Temp       := Values (X);
      Values (X) := Values (Y);
      Values (Y) := Temp;

      Temp_Index := Permutation (X);
      Permutation (X) := Permutation (Y);
      Permutation (Y) := Temp_Index;
   end Swap;

   function Index_Of_Minimum (Values : Nat_Array; From, To : Index) return Index
   is
      Min : Index := From;
   begin
      for Index in From .. To loop
         if Values (Index) < Values (Min) then
            Min := Index;
         end if;
         pragma Loop_Invariant
           (Min in From .. To and then
              (for all I in From .. Index => Values (Min) <= Values (I)));
      end loop;

      return Min;
   end Index_Of_Minimum;

   procedure Selection_Sort (Values : in out Nat_Array) is
      Smallest : Index;  -- Index of the smallest value in the unsorted part
   begin
      --  Initialize ghost object with all indexes
      Permutation := (for J in Index => J);

      --  Selection sort: find the index of the smallest remaining
      --  values and swap the value there with the current value
      for Current in 1 .. Values'Last - 1 loop
         Smallest := Index_Of_Minimum (Values, Current, Values'Last);

         if Smallest /= Current then
            Swap (Values => Values,
                  X      => Current,
                  Y      => Smallest);
         end if;

         pragma Loop_Invariant (Is_Sorted (Values, 1, Current));
         pragma Loop_Invariant
           (for all J in Current + 1 .. Values'Last =>
              Values (Current) <= Values (J));
         pragma Loop_Invariant (Is_Perm (Values'Loop_Entry, Values));
      end loop;
   end Selection_Sort;

end Sort;
