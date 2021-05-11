
package body Messages is

   type Message_Content_T is new Integer;

   procedure Set_Content
     (Message : in out Message_T;
      Value   :        Integer) is
      New_Value : constant Message_Content_T := Message_Content_T (Value);
   begin
      if Message = null
      then
         Message := new Message_Content_T'(New_Value);
      else
         Message.all := New_Value;
      end if;
   end Set_Content;

   function Content
     (Message : Message_T)
      return Integer is (Integer (Message.all));

   function Image
     (Message : Message_T)
      return String is ("**" & Message_Content_T'Image (Message.all));

end Messages;
