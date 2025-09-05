package body Converter is

   function Legal (C : Character) return Boolean is
   begin
      return
        C in '0' .. '9'
        or C = '+'
        or C = '-'
        or C = '+'
        or C = '_'
        or C = 'e'
        or C = 'E';
   end Legal;

   function Convert (Str : String) return Types.Integer_T is
   begin
      for I in Str'Range loop
         if not Legal (Str (I)) then
            raise Illegal_String;
         end if;
      end loop;
      return Types.Integer_T'Value (Str);
   exception
      when Constraint_Error =>
         raise Out_Of_Range;
   end Convert;

end Converter;
