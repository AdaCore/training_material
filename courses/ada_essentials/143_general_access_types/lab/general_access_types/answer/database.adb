with Ada.Text_IO; use Ada.Text_IO;

package body Database is

   Table : array (Value_T'Range) of aliased Natural := (others => 0);

   function Value (Attribute : Attribute_T) return Value_T is
      Retval : Natural := 0;
   begin
      for Dice of Attribute loop
         Retval := Retval + Natural (Dice);
      end loop;
      return Value_T (Retval);
   end Value;

   function Reference (Value : Value_T) return Reference_T is
   begin
      return Table (Value)'Access;
   end Reference;

   procedure Increment (Attribute : Attribute_T) is
   begin
      Table (Value (Attribute)) := Table (Value (Attribute)) + 1;
   end Increment;

   procedure Print (Message : String) is
   begin
      Put_Line (Message);
      Put ("  Value: ");
      for Index in Table'Range loop
         Set_Col (8 + Positive_Count (Index * 4));
         Put (Index'Image);
      end loop;
      New_Line;

      Put ("  Count: ");
      for Index in Table'Range loop
         Set_Col (8 + Positive_Count (Index * 4));
         Put (Table (Index)'Image);
      end loop;
      New_Line;
   end Print;

end Database;
