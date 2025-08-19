with Ada.Text_IO;
package body Line_Draw is

   procedure Print (Object : Object_T'Class) is
   -- Get lines from object
   begin
      Ada.Text_IO.Put_Line ("Object");
      -- Print the lines making up the object
   end Print;

end Line_Draw;
