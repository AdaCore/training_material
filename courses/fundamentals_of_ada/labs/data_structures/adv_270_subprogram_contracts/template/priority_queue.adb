package body Priority_Queue is

   procedure Push (Queue    : in out Queue_T;
                   Priority :        Priority_T;
                   Value    :        String) is
      --$ begin answer
      Last      : Size_T renames Queue.Size;
      New_Entry : Entries_T := (Priority, To_Unbounded_String (Value));
      --$ end answer
   begin
      --$ begin question
         --  Implement priorities
         Queue.Size := Queue.Size + 1;
         Queue.Entries (Queue.Size) := (Value => To_Unbounded_String (Value));
      --$ end question
      --$ begin answer
      if Queue.Size = 0 then
         Queue.Entries (Last + 1) := New_Entry;
      elsif Priority < Queue.Entries (1).Priority then
         Queue.Entries (2 .. Last + 1) := Queue.Entries (1 .. Last);
         Queue.Entries (1) := New_Entry;
      elsif Priority > Queue.Entries (Last).Priority then
         Queue.Entries (Last + 1) := New_Entry;
      else

         for Index in 1 .. Last loop
            if Priority <= Queue.Entries (Index).Priority then
               Queue.Entries (Index + 1 .. Last + 1) := Queue.Entries (Index .. Last);
               Queue.Entries (Index) := New_Entry;
               exit;
            end if;
         end loop;

      end if;
      Last := Last + 1;
      --$ end answer
   end Push;

   procedure Pop (Queue : in out Queue_T;
                  Value :    out Unbounded_String) is
   begin
      Value      := Queue.Entries (Queue.Size).Value;
      Queue.Size := Queue.Size - 1;
   end Pop;

end Priority_Queue;
