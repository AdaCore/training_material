package Messages.Modify is

   procedure Kind (Message   : in out Message_T;
                   New_Value :        Kind_T);
   procedure Request (Message   : in out Message_T;
                      New_Value :        Request_T);
   procedure Status (Message   : in out Message_T;
                     New_Value :        Status_T);

end Messages.Modify;
