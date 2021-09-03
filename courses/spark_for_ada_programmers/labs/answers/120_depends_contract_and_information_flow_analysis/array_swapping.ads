package Array_Swapping
is

   subtype ElementType is Natural range 0..1000;
   subtype IndexType is Positive range 1..100;
   type ArrayType is array (IndexType) of ElementType;

   procedure Rotate3(A: in out ArrayType; X, Y, Z: in IndexType)
     with
       Depends => (A => (A, X, Y, Z)),
       Pre     => X /= Y and
                  Y /= Z and
                  X /= Z,
       Post    => A = (A'Old with delta X => A'Old(Y),
                                        Y => A'Old(Z),
                                        Z => A'Old(X));

end Array_Swapping;
