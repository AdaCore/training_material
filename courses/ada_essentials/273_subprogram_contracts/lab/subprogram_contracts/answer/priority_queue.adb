package body Priority_Queue is

   procedure Push (Queue : in out Queue_T;
                   Item  :        Item_T) is
      Last : Size_T renames Queue.Size;
   begin
      if Queue.Size = 0 then
         Queue.Entries (Last + 1) := Item;
      elsif Item.Priority < Queue.Entries (1).Priority then
         Queue.Entries (2 .. Last + 1) := Queue.Entries (1 .. Last);
         Queue.Entries (1)             := Item;
      elsif Item.Priority > Queue.Entries (Last).Priority then
         Queue.Entries (Last + 1) := Item;
      else
         for Index in 1 .. Last loop
            if Item.Priority <= Queue.Entries (Index).Priority
            then
               Queue.Entries (Index + 1 .. Last + 1) :=
                 Queue.Entries (Index .. Last);
               Queue.Entries (Index)                 := Item;
               exit;
            end if;
         end loop;
      end if;
      Last := Last + 1;
   end Push;

   procedure Pop (Queue : in out Queue_T;
                  Value :    out Value_T) is
   begin
      Value      := Queue.Entries (Queue.Size).Value;
      Queue.Size := Queue.Size - 1;
   end Pop;

end Priority_Queue;
