with Ada.Text_IO; use Ada.Text_IO;

package body Database is

   function Value (Attribute : Attribute_T) return Value_T is
   begin
      -- Implement this
      return Value_T'First;
   end Value;

   function Reference (Value : Value_T) return Reference_T is
      Retval : Reference_T;
   begin
      -- Implement this
      return Retval;
   end Reference;

   procedure Increment (Attribute : Attribute_T) is
   begin
      null;  -- Implement this
   end Increment;

   procedure Print (Message : String) is
   begin
      null;  -- Implement this
   end Print;

end Database;
