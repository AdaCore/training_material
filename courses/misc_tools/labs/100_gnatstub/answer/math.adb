------------------------------------------------------------
--                                                        --
-- MATH                                                   --
--                                                        --
--                                                        --
-- Simplistic math package to add or subtract two numbers --
--                                                        --
------------------------------------------------------------

pragma Ada_2012;
package body Math is

   ---------------------
   -- Add_Two_Numbers --
   ---------------------

   procedure Add_Two_Numbers
     (Result : out Integer; Param_A : Integer; Param_B : Integer)
   is
   begin
      Result := Param_A + Param_B;
   end Add_Two_Numbers;

   --------------------------
   -- Subtract_Two_Numbers --
   --------------------------

   function Subtract_Two_Numbers
     (Param_A : Integer; Param_B : Integer) return Integer
   is
   begin
      return Param_A - Param_B;
   end Subtract_Two_Numbers;

end Math;
