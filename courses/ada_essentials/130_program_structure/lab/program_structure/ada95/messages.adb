package body Messages is

   function Create
     (Kind : Kind_T; Request : Request_T; Status : Status_T) return Message_T
   is
   begin
      return (Kind => Kind, Request => Request, Status => Status);
   end Create;

   function Kind (Message : Message_T) return Kind_T is
   begin
      return Message.Kind;
   end Kind;

   function Request (Message : Message_T) return Request_T is
   begin
      return Message.Request;
   end Request;

   function Status (Message : Message_T) return Status_T is
   begin
      return Message.Status;
   end Status;

end Messages;
