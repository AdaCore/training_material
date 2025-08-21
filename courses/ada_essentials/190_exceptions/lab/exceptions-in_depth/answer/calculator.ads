--Calculator_Body
--Calculator_Spec
package Calculator is
   Formatting_Error : exception;
   Divide_By_Zero   : exception;
   type Integer_T is range -1_000 .. 1_000;
   function Add
     (Left, Right : String)
      return Integer_T;
   function Subtract
     (Left, Right : String)
      return Integer_T;
   function Multiply
     (Left, Right : String)
      return Integer_T;
   function Divide
     (Top, Bottom : String)
      return Integer_T;
end Calculator;
