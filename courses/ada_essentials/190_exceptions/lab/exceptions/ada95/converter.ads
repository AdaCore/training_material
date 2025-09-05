with Types;

package Converter is
   Illegal_String : exception;
   Out_Of_Range : exception;
   function Convert (Str : String) return Types.Integer_T;
end Converter;
