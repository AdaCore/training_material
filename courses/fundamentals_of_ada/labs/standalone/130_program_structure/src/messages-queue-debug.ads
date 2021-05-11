
package Messages.Queue.Debug is

   function Queue_Length return Integer;
   procedure Inject_Crc_Fault (Position : Integer);
   function Text
     (Message : Message_T)
      return String;

end Messages.Queue.Debug;
