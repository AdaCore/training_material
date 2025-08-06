procedure Main is
   A : access Integer;
begin
   declare
      type R is record
         A : access Integer;
      end record;

      D : R := (A => new Integer);
   begin
      -- Invalid, and no conversion possible
      A := D.A;
   end;
end Main;
