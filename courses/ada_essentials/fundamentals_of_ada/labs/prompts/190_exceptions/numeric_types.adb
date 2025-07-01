package body Numeric_Types is

   function Value
     (Str : String)
      return Integer_T is
   begin
      return Integer_T'First;
   end Value;

end Numeric_Types;
