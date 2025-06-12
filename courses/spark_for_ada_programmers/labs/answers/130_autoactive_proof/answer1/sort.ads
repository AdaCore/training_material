with Sort_Types; use Sort_Types;

package Sort is

   function Is_Sorted (Values : Nat_Array; From, To : Index) return Boolean is
     (for all I in From .. To - 1 => Values (I) <= Values (I + 1))
   with
     Ghost;
   --  Values is considered sorted if, for every index between From and To-1,
   --  the value is less than or equal to the next value in the array.

   function Is_Sorted (Values : Nat_Array) return Boolean is
     (Is_Sorted (Values, Values'First, Values'Last))
   with
     Ghost;
   --  Values is considered sorted if, for every element in the array,
   --  the value is less than or equal to the next value in the array.

   Permutation : Permut_Array with Ghost;
   --  Permutation array only used for proof

   function Is_Perm (Left, Right : Nat_Array) return Boolean is
     (Is_Permutation_Array (Permutation)
       and then
     (for all J in Index => Right (J) = Left (Permutation (J))))
   with
     Ghost;
   --  Ghost object Permutation is actually a map of indexes
   --  between Left and Right. I.e., if Permutation(3) is 5, then
   --  the value at Right(3) is the same value as Left(5).
   --  This function verifies that, for all indexes in Right,
   --  the value at the index in Right is the same as the value
   --  at the mapped index.

   procedure Selection_Sort (Values : in out Nat_Array)
   with
     Post => Is_Sorted (Values)
       and then Is_Perm (Values'Old, Values);
   --  Sort Values such that the output is sorted, and the
   --  output is a permutation of the input (e.g. no missing
   --  or extra values).

end Sort;
