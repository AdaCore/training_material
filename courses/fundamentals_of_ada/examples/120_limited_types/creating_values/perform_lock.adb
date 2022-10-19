with Ada.Text_IO;          use Ada.Text_IO;
with Multiprocessor_Mutex; use Multiprocessor_Mutex;

procedure Perform_Lock is
   Lock1 : Also_Limited_T := (Lock => (Flag => 2), Id => "LOCK");
   Lock2 : Also_Limited_T;
begin
   -- Lock2 := Create ( 3, "CREA" ); -- illegal
   Put_Line (Lock1.Id & Lock1.Lock.Flag'Image);
end Perform_Lock;
