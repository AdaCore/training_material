with Ada.Text_IO;
with Converter;
with Types;

procedure Main is

   procedure Print_Value (Str : String) is
      Value : Types.Integer_T;
   begin
      Ada.Text_IO.Put (Str & " => ");
      Value := Converter.Convert (Str);
      Ada.Text_IO.Put_Line (Types.Integer_T'Image (Value));
   exception
      when Converter.Out_Of_Range =>
         Ada.Text_IO.Put_Line ("Out of range");
      when Converter.Illegal_String =>
         Ada.Text_IO.Put_Line ("Illegal entry");
   end Print_Value;

begin
   Print_Value ("123");
   Print_Value ("2_3_4");
   Print_Value ("-345");
   Print_Value ("+456");
   Print_Value ("1234567890");
   Print_Value ("123abc");
   Print_Value ("12e3");
end Main;
