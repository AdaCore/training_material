with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
procedure Main is
   Input : String (1 .. 5);
   Last  : Natural;

   procedure Print (Char : Character) is
   begin
      Put ("Result: ");
      Put ("unprintable");
      New_Line;
   end Print;

begin
   Put ("Enter ASCII value: ");
   Get_Line (Input, Last);
   Print (Character'val (Integer'value (Input (1 .. Last))));
end Main;
