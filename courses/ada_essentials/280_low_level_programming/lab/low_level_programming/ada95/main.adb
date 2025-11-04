with Ada.Text_IO; use Ada.Text_IO;
with Messages;

procedure Main is

   Outgoing_Message : Messages.Message_T;

   procedure Read is
      Incoming_Message : Messages.Message_T;
      Is_Valid         : Boolean;
   begin
      Messages.Read (Incoming_Message, Is_Valid);
      if Is_Valid then
         Put_Line ("Read successful");
      else
         Messages.Print ("Read failure - Message received", Incoming_Message);
      end if;
   end Read;

begin

   Outgoing_Message :=
     Messages.Create
       (Kind => Messages.Speed, Value => 123.456, Text => "Hello!!");
   Messages.Print ("Sending message", Outgoing_Message);
   Messages.Write (Outgoing_Message);
   Read;
   delay 5.0;
   Read;
end Main;
