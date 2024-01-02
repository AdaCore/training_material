--::::::::::
--room.ads
--::::::::::
with Chop;
with Phil;
with Society;
package Room is
 
  -- Dining Philosophers - Ada 95 edition

  -- Room.Maitre_D is responsible for assigning seats at the
  --   table, "left" and "right" chopsticks, and for reporting
  --   interesting events to the outside world.

  -- Michael B. Feldman, The George Washington University,
  -- July, 1995.

  Table_Size : constant := 5;
  subtype Table_Type is Positive range 1 .. Table_Size;
 
  Sticks : array (Table_Type) of Chop.Stick;
 
  task Maitre_D is
    entry Start_Serving;
    entry Report_State (Which_Phil : in Society.Unique_DNA_Codes;
                        State      : in Phil.States;
                        How_Long   : in Natural := 0;
                        Which_Meal : in Natural := 0);
  end Maitre_D;
 
end Room;
