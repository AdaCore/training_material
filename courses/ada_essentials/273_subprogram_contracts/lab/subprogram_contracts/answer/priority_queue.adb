package body Priority_Queue is

   function Pad (Str : String) return String_T is
      Retval : String_T := (others => ' ');
   begin
      if Str'Length > Retval'Length then
         Retval := Str (Str'First .. Str'First + Retval'Length - 1);
      else
         Retval (1 .. Str'Length) := Str;
      end if;
      return Retval;
   end Pad;

   procedure Push (Queue    : in out Queue_T;
                   Priority :        Priority_T;
                   Value    :        String) is
      Last      : Size_T renames Queue.Size;
      New_Entry : constant Entries_T := (Priority, Pad (Value));
   begin
      if Queue.Size = 0 then
         Queue.Entries (Last + 1) := New_Entry;
      elsif Priority < Queue.Entries (1).Priority then
         Queue.Entries (2 .. Last + 1) := Queue.Entries (1 .. Last);
         Queue.Entries (1)             := New_Entry;
      elsif Priority > Queue.Entries (Last).Priority then
         Queue.Entries (Last + 1) := New_Entry;
      else
         for Index in 1 .. Last loop
            if Priority <= Queue.Entries (Index).Priority then
               Queue.Entries (Index + 1 .. Last + 1) :=
                 Queue.Entries (Index .. Last);
               Queue.Entries (Index) := New_Entry;
               exit;
            end if;
         end loop;
      end if;
      Last := Last + 1;
   end Push;

   procedure Pop (Queue : in out Queue_T;
                  Value :    out String_T) is
   begin
      Value      := Queue.Entries (Queue.Size).Value;
      Queue.Size := Queue.Size - 1;
   end Pop;

end Priority_Queue;
