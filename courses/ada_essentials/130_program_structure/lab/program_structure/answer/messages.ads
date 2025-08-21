--Messages
package Messages is
   type Message_T is private;
   type Kind_T is (Command, Query);
   type Request_T is digits 6;
   type Status_T is mod 255;

   function Create (Kind    : Kind_T;
                    Request : Request_T;
                    Status  : Status_T)
                    return Message_T;

   function Kind (Message : Message_T) return Kind_T;
   function Request (Message : Message_T) return Request_T;
   function Status (Message : Message_T) return Status_T;

private
   type Message_T is record
      Kind    : Kind_T;
      Request : Request_T;
      Status  : Status_T;
   end record;
end Messages;
