--Calculator_Body
package body Calculator is
   function Value
     (Str : String)
      return Integer_T is
   begin
      return Integer_T'Value (Str);
   exception
      when Constraint_Error =>
         raise Formatting_Error;
   end Value;
   function Add
     (Left, Right : String)
      return Integer_T is
   begin
      return Value (Left) + Value (Right);
   end Add;
   function Subtract
     (Left, Right : String)
      return Integer_T is
   begin
      return Value (Left) - Value (Right);
   end Subtract;
   function Multiply
     (Left, Right : String)
      return Integer_T is
   begin
      return Value (Left) * Value (Right);
   end Multiply;
   function Divide
     (Top, Bottom : String)
      return Integer_T is
   begin
      if Value (Bottom) = 0 then
         raise Divide_By_Zero;
      else
         return Value (Top) / Value (Bottom);
      end if;
   end Divide;
end Calculator;
