package Longest_Common_Prefix
  with SPARK_Mode => On,
  Initializes => A
is
   type Text is array (Positive range <>) of Integer;
   A : Text (1 .. 1000) := (others => 0);

   function LCP (X, Y : Positive) return Natural
     with
       Pre          => X in A'Range and Y in A'Range,
     Contract_Cases => (A (X) /= A (Y) => LCP'Result = 0,
                        X = Y          => LCP'Result = A'Last - X + 1,
                        others         => 0 < LCP'Result),
     Post           => (for all J in 0 .. LCP'Result - 1 => A (X + J) = A (Y + J)
                        and then
                          (if X + LCP'Result in A'Range and Y + LCP'Result in A'Range then
                           A (X + LCP'Result) /= A (Y + LCP'Result)));

end Longest_Common_Prefix;
