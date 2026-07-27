package body Priority_Queue is

   procedure Push (Queue : in out Queue_T;
                   Item  :        Item_T) is
   begin
      --  Item does not go in the front (or end) of queue.
      --  It goes at the front (or end) of the values with the same priority
      null;
   end Push;

   procedure Pop (Queue : in out Queue_T;
                  Value :    out Value_T) is
   begin
      --  Value should be the first (or last) item in the entire queue
      Value := 0;
   end Pop;

end Priority_Queue;
