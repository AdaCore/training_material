with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Priority_Queue is
   type Priority_T is (Low, Medium, High);
   type Queue_T is private;

   procedure Push
     (Queue    : in out Queue_T;
      Priority :        Priority_T;
      Value    :        String);
   procedure Pop
     (Queue : in out Queue_T;
      Value :    out Unbounded_String);

   function Full
     (Queue : Queue_T)
      return Boolean;
   function Empty
     (Queue : Queue_T)
      return Boolean;
   function Valid
     (Queue : Queue_T)
      return Boolean;
private
   type Queue_T is null record;

end Priority_Queue;
