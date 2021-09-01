package Types.Strings is

   Exponent_Digits      : Natural := 2;
   Digits_After_Decimal : Natural := 3;

   function To_String (Value : Miles_T) return String;
   function To_String (Value : Hours_T) return String;
   -- Create functions to convert your types to strings

end Types.Strings;
