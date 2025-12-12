with Ada.Exceptions;
with Ada.Text_IO;
with Numeric_Types;

procedure Main is

   procedure Print_Value (Str : String) is
      Value : Numeric_Types.Integer_T;
   begin
      Ada.Text_IO.Put (Str & " => ");
      Value := Numeric_Types.Value (Str);
      Ada.Text_IO.Put_Line (Numeric_Types.Integer_T'Image (Value));
   exception
      when The_Err : Numeric_Types.Out_Of_Range =>
         Ada.Text_IO.Put_Line
           ("Out of range: " & Ada.Exceptions.Exception_Message (The_Err));
      when The_Err : Numeric_Types.Illegal_String =>
         Ada.Text_IO.Put_Line
           ("Illegal entry: "
            & Ada.Exceptions.Exception_Information (The_Err));
      when The_Err : Numeric_Types.Bad_Format =>
         Ada.Text_IO.Put_Line
           ("Bad format: " & Ada.Exceptions.Exception_Name (The_Err));
   end Print_Value;

begin
   Print_Value ("123");
   Print_Value ("2_3_4");
   Print_Value ("-345");
   Print_Value ("+456");
   Print_Value ("1234567890");
   Print_Value ("12e3");
   Print_Value ("12-");
   Print_Value ("123abc");
end Main;
