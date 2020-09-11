with Simple_IO;

package body Aliasing
  with SPARK_Mode => On
is

   procedure Multiply (X, Y : in     Rec;
		       Z    :    out Rec)
   is
   begin
      Z := (0, 0);
      Z.F := X.F * Y.F;
      Z.G := X.G * Y.G;
   end Multiply;

   procedure Test
   is
     R      : Rec;
     Result : Integer;
   begin
      R := (10, 10);
      Result := 100;

      -- If R is passed by reference, then
      -- parameters might be aliased here. This is Unspecified
      -- in Ada 2012, so must be eliminated in SPARK
      Multiply (R, R, R);

      Result := Result / R.F; -- So R.F might be 0 here

      Simple_IO.Put_Line (Result);
   end Test;


end Aliasing;
