with Ada.Text_IO; use Ada.Text_IO;
with Numbers;     use Numbers;
with Types;       use Types;
with Strings;     use Strings;

package body Parser is

   type Component_T is record
      Line_Number : Integer;
      Category    : Category_T;
      Description : String_T;
      Quantity    : Quantity_T;
      Cost        : Cost_T;
   end record;

   Database       : array (1 .. 10) of Component_T;
   Database_Count : Integer := 0;

   procedure Load (Filename : String) is
      File : File_Type;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File)
      loop
         declare
            Pieces    : Strings_T := Split (Get_Line (File));
            Component : Component_T;
         begin
            Component.Line_Number       := Integer (Line (File) - 1);
            Component.Category          := Convert (Pieces (1).all);
            Component.Description       := Pieces (2);
            Component.Quantity          := Convert (Pieces (3).all);
            Component.Cost              := Convert (Pieces (4).all);
            Database_Count              := Database_Count + 1;
            Database (Database_Count)   := Component;
         end;
      end loop;
   end Load;

   procedure Print is
   begin
      for Component of Database (1 .. Database_Count)
      loop
         Put (Component.Line_Number'Image & ": ");
         Put (Component.Description.all & " (");
         Put (Convert (Component.Category) & ") ");
         Put (Convert (Component.Quantity) & " at $");
         Put_Line (Convert (Component.Cost));
      end loop;
   end Print;

end Parser;
