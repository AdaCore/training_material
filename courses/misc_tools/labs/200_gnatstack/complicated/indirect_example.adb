package body Indirect_Example is

   type Subprogram_Access_T is access procedure
       (A, B :     Integer;
        C    : out Boolean);
   procedure Procedure_One
     (A, B :     Integer;
      C    : out Boolean) is
   begin
      C := A > B;
   end Procedure_One;

   procedure Procedure_Two
     (A, B :     Integer;
      C    : out Boolean) is
   begin
      C := A < B;
   end Procedure_Two;

   Calls : array (Boolean) of Subprogram_Access_T :=
     (Procedure_One'Access,
      Procedure_Two'Access);

   procedure Test (Flag : in out Boolean) is
   begin
      Calls (Flag).all (1, 2, Flag);
   end Test;

end Indirect_Example;
