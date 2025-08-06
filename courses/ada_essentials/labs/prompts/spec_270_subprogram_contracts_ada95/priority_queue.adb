package body Priority_Queue is

   procedure Push
     (Queue    : in out Queue_T;
      Priority :        Priority_T;
      Value    :        String) is
   begin
      null;

   end Push;

   procedure Pop
     (Queue : in out Queue_T;
      Value :    out Unbounded_String) is
   begin
      Value      := Null_Unbounded_String;
   end Pop;

   function Full
     (Queue : Queue_T)
      return Boolean is
   begin
      return false;
   end Full;

   function Empty
     (Queue : Queue_T)
      return Boolean is
   begin
      return true;
   end Empty;

   function Valid
     (Queue : Queue_T)
      return Boolean is
   begin
      return false;
   end Valid;

end Priority_Queue;
