
package Messages is

   type Message_T is private;

   procedure Set_Content
     (Message : in out Message_T;
      Value   :        Integer);
   function Content
     (Message : Message_T)
      return Integer;
   function Image
     (Message : Message_T)
      return String;

private
   type Message_Content_T;
   type Message_T is access Message_Content_T;

end Messages;
