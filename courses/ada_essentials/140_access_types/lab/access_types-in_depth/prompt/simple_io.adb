--Simple_IO_Spec

--Simple_IO_Body
with Ada.Text_IO;
package body Simple_Io is
   function Get_String
     (Prompt : String)
      return String is
      Str  : String (1 .. 1_000);
      Last : Integer;
   begin
      Ada.Text_IO.Put (Prompt & "> ");
      Ada.Text_IO.Get_Line (Str, Last);
      return Str (1 .. Last);
   end Get_String;

   function Get_Number
     (Prompt : String)
      return Integer is
      Str : constant String := Get_String (Prompt);
   begin
      return Integer'Value (Str);
   end Get_Number;

   function Get_Character
     (Prompt : String)
      return Character is
      Str : constant String := Get_String (Prompt);
   begin
      return Str (Str'First);
   end Get_Character;

   procedure Print_String (Str : String) is
   begin
      Ada.Text_IO.Put_Line (Str);
   end Print_String;
   procedure Print_Number (Num : Integer) is
   begin
      Ada.Text_IO.Put_Line (Integer'Image (Num));
   end Print_Number;
   procedure Print_Character (Char : Character) is
   begin
      Ada.Text_IO.Put_Line (Character'Image (Char));
   end Print_Character;

   function Get_String
     (Prompt : String)
      return Unbounded_String is
   begin
      return To_Unbounded_String (Get_String (Prompt));
   end Get_String;
   procedure Print_String (Str : Unbounded_String) is
   begin
      Print_String (To_String (Str));
   end Print_String;
end Simple_Io;
