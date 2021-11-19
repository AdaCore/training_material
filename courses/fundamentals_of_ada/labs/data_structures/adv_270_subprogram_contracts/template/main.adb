with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Priority_Queue;

procedure Main is
   Queue : Priority_Queue.Queue_T;
   Value : Unbounded_String;
begin

   for Count in 1 .. 3 loop
      for Priority in Priority_Queue.Priority_T'Range
      loop
         Queue.Push (Priority, Priority'Image & Count'Image);
      end loop;
   end loop;

   while not Queue.Empty loop
      Queue.Pop (Value);
      Put_Line (To_String (Value));
   end loop;

   for Count in 1 .. 4 loop
      for Priority in Priority_Queue.Priority_T'Range
      loop
         Queue.Push (Priority, Priority'Image & Count'Image);
      end loop;
   end loop;

end Main;
