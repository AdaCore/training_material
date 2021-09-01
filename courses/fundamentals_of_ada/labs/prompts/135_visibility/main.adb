with Ada.Text_IO; use Ada.Text_IO;
with Types;
with Types.Strings;
procedure Main is

   -- Use 'renames' to simplfy access to global formatting objects

   Hours      : Types.Hours_T;
   Miles      : Types.Miles_T;

   function Get (Prompt : String) return String is
   begin
      Put (Prompt & "> ");
      return Get_Line;
   end Get;

begin

   -- Query user for distance and time values
   -- Calculate and print MPH for all distance/time combinations
   Miles := Types.Miles_T'Value (Get ("Miles"));
   Hours := Types.Hours_T'Value (Get ("Hours"));
   Put_Line (Types.Strings.To_String (Miles) & " miles / " & Types.Strings.To_String (Hours) &
             " hour = ? mph");

end Main;
