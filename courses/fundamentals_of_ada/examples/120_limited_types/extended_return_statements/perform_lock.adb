with Ada.Text_IO;          use Ada.Text_IO;
with Multiprocessor_Mutex; use Multiprocessor_Mutex;

procedure Perform_Lock is
   Lock1 : constant Also_Limited_T := Create ("One ");
   Lock2 : constant Also_Limited_T := Create ("Two ");
begin
   Put_Line (Lock1.Id & Lock1.Lock.Flag'Image);
   Put_Line (Lock2.Id & Lock2.Lock.Flag'Image);
end Perform_Lock;
