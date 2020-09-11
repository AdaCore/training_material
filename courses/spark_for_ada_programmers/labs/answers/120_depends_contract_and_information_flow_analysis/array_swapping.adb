package body Array_Swapping
is

  procedure Swap (A: in out ArrayType; I, J: in IndexType)
  with Depends => (A => (A, I, J)),
       Pre     => I /= J,
       Post    => A = A'Old'Update (I => A'Old(J),
                                    J => A'Old(I))
  is
    T: ElementType;
  begin
    T    := A(I);
    A(I) := A(J);
    A(J) := T;
  end Swap;

  procedure Rotate3(A: in out ArrayType; X, Y, Z: in IndexType)
  is
  begin
    Swap(A, X, Y);
    Swap(A, Y, Z);
  end Rotate3;

end Array_Swapping;
