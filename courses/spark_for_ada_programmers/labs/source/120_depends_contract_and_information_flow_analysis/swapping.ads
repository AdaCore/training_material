package Swapping
is

   procedure Swap (X, Y: in out Positive);

   procedure Identity (X, Y: in out Positive)
     with Depends => (X => X,
                      Y => Y);

end Swapping;
