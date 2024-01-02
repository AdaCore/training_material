--::::::::::
--diners.adb
--::::::::::
with Text_IO;
with Room;
procedure Diners is

  -- Dining Philosophers - Ada 95 edition

  -- This is the main program, responsible only for telling the
  --   Maitre_D to get busy.

  -- Michael B. Feldman, The George Washington University,
  -- July, 1995.
 
begin
  --Text_IO.New_Line;     -- artifice to flush output buffer
  Room.Maitre_D.Start_Serving;
end Diners;
