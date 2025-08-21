
package body Messages is

   function Create (Kind    : Kind_T;
                    Request : Request_T;
                    Status  : Status_T)
                    return Message_T is
      (Kind => Kind, Request => Request, Status  => Status);

   function Kind (Message : Message_T) return Kind_T is
      (Message.Kind);
   function Request (Message : Message_T) return Request_T is
      (Message.Request);
   function Status (Message : Message_T) return Status_T is
      (Message.Status);

end Messages;
