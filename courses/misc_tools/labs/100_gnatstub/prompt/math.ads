------------------------------------------------------------
--                                                        --
-- MATH                                                   --
--                                                        --
--                                                        --
-- Simplistic math package to add or subtract two numbers --
--                                                        --
------------------------------------------------------------
package Math is

   procedure Add_Two_Numbers
     (Result  : out Integer;
      Param_A :     Integer;
      Param_B :     Integer);

   function Subtract_Two_Numbers
     (Param_A : Integer;
      Param_B : Integer)
      return Integer;

end Math;
