
package Types.Strings is

   Exponent_Digits      : Natural := 2;
   Digits_After_Decimal : Natural := 3;

   function To_String
     (Value : Mph_T)
      return String;

   function To_String
     (Value : Feet_T)
      return String;
   function To_String
     (Value : Miles_T)
      return String;
   function To_String
     (Value : Kilometers_T)
      return String;

   function To_String
     (Value : Seconds_T)
      return String;
   function To_String
     (Value : Minutes_T)
      return String;
   function To_String
     (Value : Hours_T)
      return String;

end Types.Strings;
