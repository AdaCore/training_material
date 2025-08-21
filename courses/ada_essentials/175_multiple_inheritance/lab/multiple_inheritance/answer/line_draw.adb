--Drawing_Spec

--Drawing_Body
with Ada.Text_IO;
package body Line_Draw is

   procedure Print (Object : Object_T'Class) is
      Lines : constant Base_Types.Lines_T := Object.Convert;
   begin
      for Index in Lines'Range loop
         Ada.Text_IO.Put_Line ("Line" & Index'Image);
         Ada.Text_IO.Put_Line
           ("  From: " & Base_Types.Image (Lines (Index) (1)));
         Ada.Text_IO.Put_Line
           ("    To: " & Base_Types.Image (Lines (Index) (2)));
      end loop;
   end Print;

end Line_Draw;
