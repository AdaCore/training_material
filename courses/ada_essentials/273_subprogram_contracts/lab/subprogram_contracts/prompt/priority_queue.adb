package body Priority_Queue is

   procedure Push
     (Queue    : in out Queue_T;
      Priority :        Priority_T;
      Value    :        String) is null;

   procedure Pop
     (Queue : in out Queue_T;
      Value :    out String) is null;

end Priority_Queue;
