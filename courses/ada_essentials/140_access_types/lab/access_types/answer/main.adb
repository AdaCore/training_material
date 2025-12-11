with Ada.Text_IO; use Ada.Text_IO;
with Database;
with Database_List;
procedure Main is
   List      : Database_List.List_T;
   Component : Database.Database_T;

   procedure Add (Number : Positive;
                  Symbol : Character) is
   begin
      Database_List.Insert (List, Database.Create (Number, Symbol));
   end Add;

   procedure Delete (Number : Positive;
                     Symbol : Character) is
   begin
      Database_List.Delete (List, Database.Create (Number, Symbol));
   end Delete;

   procedure Print is
   begin
      Database_List.First (List);
      Put_Line ("List");
      while not Database_List.End_Of_List (List) loop
         Component := Database_List.Current (List);
         Put_Line ("  " & Database.Image (Component));
         Database_List.Next (List);
      end loop;
   end Print;

begin

   Add (1, 'Z');
   Add (2, 'A');
   Add (3, 'Y');
   Add (4, 'B');
   Print;
   Delete (2, 'A');
   Add (5, 'M');
   Print;

end Main;
