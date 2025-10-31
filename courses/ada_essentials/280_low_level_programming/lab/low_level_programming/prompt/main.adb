with Ada.Text_IO; use Ada.Text_IO;
with Crc;
with Messages;
procedure Main is
   Message : Messages.Message_T;
   Is_Valid : Boolean;
begin
   -- Message := Messages.Create (?);
   Messages.Print ("Sending message", Message);
   Messages.Write (Message);
   Messages.Read (Message, Is_Valid);
   delay 5.0;
   Messages.Read (Message, Is_Valid);
end Main;
