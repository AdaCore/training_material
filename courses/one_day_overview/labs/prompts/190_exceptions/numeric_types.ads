package Numeric_Types is
   Illegal_String : exception;
   Out_Of_Range   : exception;

   Max_Int : constant := 2**15;
   type Integer_T is range -(Max_Int) .. Max_Int - 1;

   function Value
     (Str : String)
      return Integer_T;
end Numeric_Types;
