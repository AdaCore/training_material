with Nat_Multisets;          use Nat_Multisets;
with Perm.Lemma_Subprograms; use Perm.Lemma_Subprograms;

package body Sort is

   -- Swap the values of Values (X) and Values (Y)
   procedure Swap (Values : in out Nat_Array; X, Y : Index)
   with
     Pre  => X /= Y,
     Post => Is_Perm (Values'Old, Values)
       and then Values = (Values'Old with delta X => Values'Old (Y), Y => Values'Old (X))
   is
      Temp : Integer;

      --  Ghost variables
      Init   : constant Nat_Array := Values with Ghost;
      Interm : Nat_Array with Ghost;

      --  Ghost procedure
      procedure Prove_Perm with Ghost,
        Pre  => Is_Set (Init, X, Init (Y), Interm)
          and then Is_Set (Interm, Y, Init (X), Values),
        Post => Is_Perm (Init, Values)
      is
      begin
         Occ_Set (Init, X, Init (Y), Interm);
         Occ_Set (Interm, Y, Init (X), Values);
         pragma Assert
           (for all F of Union (Occurrences (Init), Occurrences (Values)) =>
                Occ (Values, F) = Occ (Init, F));
      end Prove_Perm;

   begin
      Temp       := Values (X);
      Values (X) := Values (Y);

      --  Ghost code
      pragma Assert (Is_Set (Init, X, Init (Y), Values));
      Interm := Values;

      Values (Y) := Temp;

      --  Ghost code
      pragma Assert (Is_Set (Interm, Y, Init (X), Values));
      Prove_Perm;
   end Swap;

   -- Finds the index of the smallest element in the array
   function Index_Of_Minimum (Values : Nat_Array; From, To : Index) return Index
   with
     Pre  => To in From .. Values'Last,
     Post => Index_Of_Minimum'Result in From .. To and then
       (for all I in From .. To =>
          Values (Index_Of_Minimum'Result) <= Values (I))
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
