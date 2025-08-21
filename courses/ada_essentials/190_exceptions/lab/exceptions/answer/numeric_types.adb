
package body Numeric_Types is

   function Legal (C : Character) return Boolean is
   begin
      return
        C in '0' .. '9' or C = '+' or C = '-' or C = '_' or C = 'e' or C = 'E';
   end Legal;

   function Value (Str : String) return Integer_T is
   begin
      for I in Str'Range loop
         if not Legal (Str (I)) then
            raise Illegal_String;
         end if;
      end loop;
      return Integer_T'Value (Str);
   exception
      when Constraint_Error =>
         raise Out_Of_Range;
   end Value;

end Numeric_Types;
