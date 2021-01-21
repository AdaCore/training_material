
with Ada.Text_IO; use Ada.Text_IO;

package body Types.Strings is

   package Io is new Ada.Text_IO.Float_IO (Float);
   function To_String
     (Value : Float)
      return String is
      Ret_Val : String (1 .. 30);
   begin
      Io.Put
        (To   => Ret_Val,
         Item => Value,
         Aft  => Digits_After_Decimal,
         Exp  => Exponent_Digits);
      for I in reverse Ret_Val'Range
      loop
         if Ret_Val (I) = ' '
         then
            return Ret_Val
                (I + 1 .. Ret_Val'Last);
         end if;
      end loop;
      return Ret_Val;
   end To_String;

   function To_String
     (Value : Mph_T)
      return String is (To_String (Float (Value)));

   function To_String
     (Value : Feet_T)
      return String is (To_String (Float (Value)));
   function To_String
     (Value : Miles_T)
      return String is (To_String (Float (Value)));
   function To_String
     (Value : Kilometers_T)
      return String is (To_String (Float (Value)));

   function To_String
     (Value : Seconds_T)
      return String is (To_String (Float (Value)));
   function To_String
     (Value : Minutes_T)
      return String is (To_String (Float (Value)));
   function To_String
     (Value : Hours_T)
      return String is (To_String (Float (Value)));

end Types.Strings;
