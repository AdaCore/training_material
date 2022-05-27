with Ada.Text_IO; use Ada.Text_IO;
with Numbers;     use Numbers;
with Types;       use Types;
with Strings;     use Strings;

package body Parser is

   type Element_T is record
      Line_Number : Integer;
      Category    : Category_T;
      Description : String_T;
      Quantity    : Quantity_T;
      Cost        : Cost_T;
   end record;

   Database       : array (1 .. 10) of Element_T;
   Database_Count : Integer := 0;

   procedure Load (Filename : String) is
      File : File_Type;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File)
      loop
         declare
            Pieces  : Strings_T := Split (Get_Line (File));
            Element : Element_T;
         begin
            Element.Line_Number       := Integer (Line (File) - 1);
            Element.Category          := Convert (Pieces (1).all);
            Element.Description       := Pieces (2);
            Element.Quantity          := Convert (Pieces (3).all);
            Element.Cost              := Convert (Pieces (4).all);
            Database_Count            := Database_Count + 1;
            Database (Database_Count) := Element;
         end;
      end loop;
   end Load;

   procedure Print is
   begin
      for Element of Database (1 .. Database_Count)
      loop
         Put (Element.Line_Number'image & ": ");
         Put (Element.Description.all & " (");
         Put (Convert (Element.Category) & ") ");
         Put (Convert (Element.Quantity) & " at $");
         Put_Line (Convert (Element.Cost));
      end loop;
   end Print;

end Parser;
