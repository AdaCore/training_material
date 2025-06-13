package Sort_Types is

   subtype Index is Integer range 1 .. 100;

   type Nat_Array is array (Index) of Natural;
   --  Array of natural integers

   type Permut_Array is array (Index) of Index;
   --  A permutation of an array A can be considered an
   --  array P of all indexes into A such that P has
   --  no duplicate values and no missing values.
   --  E.g. if A is indexed by 1..3, then P has any combination
   --  of 1, 2, and 3 (the indexes) where all 3 values
   -- appear and none are duplicated.

   function Is_Permutation_Array (Permut : Permut_Array) return Boolean is
     (for all J in Index =>
        (for all K in Index =>
           (if J /= K then Permut (J) /= Permut (K))));
   --  An array is a permutation if, for all indexes J and K, if J and K
   --  are not equal, then the contents at J and K are not equal.

end Sort_Types;
