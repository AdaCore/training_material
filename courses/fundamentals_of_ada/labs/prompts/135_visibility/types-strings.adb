with Ada.Text_IO; use Ada.Text_IO;
package body Types.Strings is

   package Io is new Ada.Text_IO.Float_IO (Float);
   function To_String (Value : Float) return String is
   begin
      -- Use IO package to convert number to a string
      -- Use global objects from spec to control formatting
      return "";
   end To_String;

   -- Implement
   function To_String (Value : Miles_T) return String is
   begin
      return To_String ( Float(Value) );
   end To_String;

   function To_String (Value : Hours_T) return String is
   begin
      return To_String ( Float(Value) );
   end To_String;

   -- Implement functions to convert your types to strings

end Types.Strings;
