package body Messages.Modify is

   procedure Kind (Message : in out Message_T; New_Value : Kind_T) is
   begin
      Message.Kind := New_Value;
   end Kind;

   procedure Request (Message : in out Message_T; New_Value : Request_T) is
   begin
      Message.Request := New_Value;
   end Request;

   procedure Status (Message : in out Message_T; New_Value : Status_T) is
   begin
      Message.Status := New_Value;
   end Status;

end Messages.Modify;
