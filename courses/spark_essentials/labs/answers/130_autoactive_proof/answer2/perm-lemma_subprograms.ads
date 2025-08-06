package Perm.Lemma_Subprograms with
  Ghost,
  Annotate => (GNATprove, Always_Return)
is

   function Is_Set
     (A : Nat_Array; I : Index; V : Natural; R : Nat_Array) return Boolean
   is
     (R (I) = V
       and then (for all J in A'Range =>
                   (if I /= J then R (J) = A (J))));

   procedure Occ_Set (A : Nat_Array; I : Index; V : Natural; R : Nat_Array)
   with
     Global => null,
     Pre    => Is_Set (A, I, V, R),
     Post   =>
       (if V = A (I) then Occurrences (R) = Occurrences (A)
        else Occ (R, V) = Occ (A, V) + 1
          and then Occ (R, A (I)) = Occ (A, A (I)) - 1
          and then
            (for all E of Union (Occurrences (R), Occurrences (A)) =>
               (if E not in V | A (I) then Occ (R, E) = Occ (A, E))));

end Perm.Lemma_Subprograms;
