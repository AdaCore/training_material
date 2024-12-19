package body Messages.List_Types.Operations is

   procedure Append
     (List : in out List_T;
      Item :        Message_T) is
   begin
      null;
   end Append;

   function Next
     (List : List_T)
      return List_T is
   begin
      return List;
   end Next;

   function Is_Null
     (List : List_T)
      return Boolean is
   begin
      return True;
   end Is_Null;

   function Image
     (Message : List_T)
      return String is
   begin
      return "";
   end Image;
end Messages.List_Types.Operations;
