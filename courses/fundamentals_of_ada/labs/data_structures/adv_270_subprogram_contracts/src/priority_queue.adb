package body Priority_Queue is

   procedure Push (Queue    : in out Queue_T;
                   Priority :        Priority_T;
                   Value    :        String) is
   begin
         --  Implement priorities
         Queue.Size := Queue.Size + 1;
         Queue.Entries (Queue.Size) := (Value => To_Unbounded_String (Value));
   end Push;

   procedure Pop (Queue : in out Queue_T;
                  Value :    out Unbounded_String) is
   begin
      Value      := Queue.Entries (Queue.Size).Value;
      Queue.Size := Queue.Size - 1;
   end Pop;

end Priority_Queue;
