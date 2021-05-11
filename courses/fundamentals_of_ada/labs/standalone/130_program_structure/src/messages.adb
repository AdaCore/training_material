
with Messages.Crc;
package body Messages is

   function Create
     (Kind    : Kind_T;
      Content : Content_T)
      return Message_T is
   begin
      return (Kind => Kind, Content => To_Unbounded_String (Content),
         Crc       => Crc (Content));
   end Create;

   function Kind
     (Message : Message_T)
      return Kind_T is (Message.Kind);
   function Content
     (Message : Message_T)
      return Content_T is (To_String (Message.Content));

end Messages;
