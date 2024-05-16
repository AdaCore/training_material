with Ada.Text_IO; use Ada.Text_IO;
with Crc;
with Messages;
procedure Main is
   Message : Messages.Message_T;
   Valid   : Boolean;
begin
   loop
      Put ("Create Write Read Print: ");
      declare
         Command : constant String := Get_Line;
      begin
         exit when Command'Length = 0;
         case Command (Command'First) is
            when 'c' | 'C' =>
               Message := Messages.Create ("Hello", True, '1');
            when 'w' | 'W' =>
               Messages.Write (Message);
            when 'r' | 'R' =>
               Messages.Read (Message, Valid);
            when 'p' | 'P' =>
               Messages.Print (Message);
            when others =>
               null;
         end case;
      end;
   end loop;
end Main;
