with Ada.Text_IO;
package body Console is

   procedure Print
     (S        : String  := "";
      New_Line : Boolean := True) is
   begin
      if S'Length > 0
      then
         Ada.Text_IO.Put (S);
      end if;
      if New_Line
      then
         Ada.Text_IO.New_Line;
      end if;
   end Print;

end Console;
