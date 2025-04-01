package Sort_Types is

   subtype Index is Integer range 1 .. 100;

   type Nat_Array is array (Index) of Natural;

   type Permut_Array is array (Index) of Index;

   function Is_Permutation_Array (Permut : Permut_Array) return Boolean is
     (for all J in Index =>
        (for all K in Index =>
           (if J /= K then Permut (J) /= Permut (K))));

end Sort_Types;
