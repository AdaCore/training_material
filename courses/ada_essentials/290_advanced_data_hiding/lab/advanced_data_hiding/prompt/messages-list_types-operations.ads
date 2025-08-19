with Messages.List_Types;
package Messages.List_Types.Operations is
   procedure Append
     (List : in out List_T;
      Item :        Message_T);
   function Next
     (List : List_T)
      return List_T;
   function Is_Null
     (List : List_T)
      return Boolean;
   function Image
     (Message : List_T)
      return String;
end Messages.List_Types.Operations;
