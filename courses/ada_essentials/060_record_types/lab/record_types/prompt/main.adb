with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   type Name_T is array (1 .. 6) of Character;
   type Index_T is range 0 .. 1_000;
   type Queue_T is array (Index_T range 1 .. 1_000) of Name_T;

   -- type Fifo_Queue_T is ?
   -- Queue : Fifo_Queue_T;

   Choice : Integer;

begin

   loop
      Put ("1 = add to queue | 2 = remove from queue | others => done: ");
      Choice := Integer'Value (Get_Line);
      if Choice = 1 then
         Put ("Enter name: ");
         -- Use Ada.Text_IO.Get_Line to help put a name on the queue
      elsif Choice = 2 then
         -- Remove the appropriate item from the queue and print it
         Put_Line ("TBD");
      else
         exit;
      end if;
      New_Line;
   end loop;

   Put_Line ("Remaining in line: ");
   -- Print the remaining items in the queue

end Main;
