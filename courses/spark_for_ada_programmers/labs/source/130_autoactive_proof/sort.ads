with Sort_Types; use Sort_Types;

package Sort is

   function Is_Sorted (Values : Nat_Array; From, To : Index) return Boolean is
     (for all I in From .. To - 1 => Values (I) <= Values (I + 1))
   with
     Ghost;

   function Is_Sorted (Values : Nat_Array) return Boolean is
     (Is_Sorted (Values, Values'First, Values'Last))
   with
     Ghost;

   Permutation : Permut_Array with Ghost;

   function Is_Perm (Left, Right : Nat_Array) return Boolean is
     (Is_Permutation_Array (Permutation)
       and then
     (for all J in Index => Right (J) = Left (Permutation (J))))
   with
     Ghost;

   procedure Selection_Sort (Values : in out Nat_Array)
   with
     Post => Is_Sorted (Values)
       and then Is_Perm (Values'Old, Values);

end Sort;
