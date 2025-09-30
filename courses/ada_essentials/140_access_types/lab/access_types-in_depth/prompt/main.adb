with Ada.Text_IO; use Ada.Text_IO;
with Database;
with Database_List;
procedure Main is
   List      : Database_List.List_T;
   Component : Database.Database_T;

   procedure Print is
   begin
      Database_List.First (List);
      Put_Line ("List");
      --  Print the list in order
   end Print;

begin

   --  Add four elements in random order
   --  Print the list
   Print;
   --  Remove an item from the middle of the list
   --  Add another element
   --  Print the list
   Print;
end Main;
