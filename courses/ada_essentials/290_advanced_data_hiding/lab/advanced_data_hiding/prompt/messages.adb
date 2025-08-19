package body Messages is
   procedure Set_Content
     (Message : in out Message_T;
      Value   :        Integer) is
   begin
      null;
   end Set_Content;

   function Content
     (Message : Message_T)
      return Integer is
   begin
      return 0;
   end Content;

   function Image
     (Message : Message_T)
      return String is
   begin
      return "";
   end Image;

end Messages;
