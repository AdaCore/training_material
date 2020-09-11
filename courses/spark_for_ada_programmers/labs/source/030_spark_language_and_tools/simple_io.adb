with Ada.Text_IO;

package body Simple_IO
  with SPARK_Mode => Off
is

   procedure Put_Line (S : in String)
   is
   begin
      Ada.Text_IO.Put_Line (S);
   end Put_Line;

   procedure Put_Line (S : in Integer)
   is
   begin
      Ada.Text_IO.Put_Line (Integer'Image (S));
   end Put_Line;

end Simple_IO;
