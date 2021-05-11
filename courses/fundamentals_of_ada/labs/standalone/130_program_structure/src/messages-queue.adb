
with Messages.Crc;
package body Messages.Queue is

   procedure Push (Message : Message_T) is
   begin
      Top             := Top + 1;
      The_Queue (Top) := Message;
   end Push;

   procedure Pop
     (Message : out Message_T;
      Valid   : out Boolean) is
   begin
      Message := The_Queue (Top);
      Top     := Top - 1;
      Valid   := Message.Crc = Crc (To_String (Message.Content));
   end Pop;

end Messages.Queue;
