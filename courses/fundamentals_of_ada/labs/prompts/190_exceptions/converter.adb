package body Converter is

   function Convert
     (Str : String)
      return Types.Integer_T is
   begin
      return Types.Integer_T'first;
   end Convert;

end Converter;
