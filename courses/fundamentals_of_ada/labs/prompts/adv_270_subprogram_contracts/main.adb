with Ada.Text_IO; use Ada.Text_IO;
with Priority_Queue;
procedure Main is
   Queue : Priority_Queue.Queue_T;
begin

   Ada.Text_IO.Put_Line ("Normal processing");
   --  Add some number of items to the queue
   --  Queue.Push (Priority, Priority'Image & Count'Image);

   -- Pop everything off the queue and print the item

   -- Try to overflow the queue

end Main;
