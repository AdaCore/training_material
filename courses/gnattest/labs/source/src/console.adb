with Ada.Text_IO;
package body Console is

   procedure Print
     (S        : String  := "";
      New_Line : Boolean := True) is
   begin
      if S'length > 0
      then
         Ada.Text_Io.Put (S);
      end if;
      if New_Line
      then
         Ada.Text_Io.New_Line;
      end if;
   end Print;

end Console;
