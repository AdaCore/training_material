with SPARK.Big_Integers;   use SPARK.Big_Integers;
with Sort_Types;           use Sort_Types;
with Perm;                 use Perm;

package Sort is

   function Is_Sorted (Values : Nat_Array; From, To : Index) return Boolean is
     (for all I in From .. To - 1 => Values (I) <= Values (I + 1))
   with
     Ghost;

   function Is_Sorted (Values : Nat_Array) return Boolean is
     (Is_Sorted (Values, Values'First, Values'Last))
   with
     Ghost;

   procedure Selection_Sort (Values : in out Nat_Array)
   with
     Post => Is_Sorted (Values)
       and then Is_Perm (Values'Old, Values);

end Sort;
