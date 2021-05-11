
package body Messages.List_Types.Operations is

   Id : Id_Type := Id_Type'First;

   procedure Append
     (List : in out List_T;
      Item :        Message_T) is
   begin
      if List = null
      then
         List := new List_Content_T'(Id => Id, Content => Item, Next => null);
      else
         List.Next :=
           new List_Content_T'(Id => Id, Content => Item, Next => null);
      end if;
      Id := Id_Type'Succ (Id);
   end Append;

   function Next
     (List : List_T)
      return List_T is (List.Next);

   function Is_Null
     (List : List_T)
      return Boolean is (List = null);

   function Image
     (Message : List_T)
      return String is
   begin
      if Is_Null (Message)
      then
         return "" & ASCII.LF;
      else
         return "id: " & Id_Type'Image (Message.Id) & " => " &
           Image (Message.Content) & ASCII.LF & Image (Message.Next);
      end if;
   end Image;

end Messages.List_Types.Operations;
