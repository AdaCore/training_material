package Priority_Queue is
   type Priority_T is (Low, Medium, High);
   type Queue_T is tagged private;

   procedure Push
     (Queue    : in out Queue_T;
      Priority :        Priority_T;
      Value    :        String);
      --  Add pre/post conditions as necessary

   procedure Pop
     (Queue : in out Queue_T;
      Value :    out String);
      --  Add pre/post conditions as necessary
private
   type Queue_T is tagged null record;
   -- Implement the queue

end Priority_Queue;
