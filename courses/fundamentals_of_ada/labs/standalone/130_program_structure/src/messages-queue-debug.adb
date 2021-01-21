
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body Messages.Queue.Debug is

   function Queue_Length return Integer is (Top);

   procedure Inject_Crc_Fault (Position : Integer) is
   begin
      Top                 := Position;
      The_Queue (Top).Crc := The_Queue (Top).Crc + 1;
   end Inject_Crc_Fault;

   function Text
     (Message : Message_T)
      return String is
     (Kind_T'Image (Message.Kind) & " => " & To_String (Message.Content) &
      " (" & Crc_T'Image (Message.Crc) & " )");

end Messages.Queue.Debug;
