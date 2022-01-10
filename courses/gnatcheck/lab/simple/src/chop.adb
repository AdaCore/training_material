------
--::::::::::
--chop.adb
--::::::::::
package body Chop is

  -- Dining Philosophers - Ada 95 edition
  -- Chopstick is an Ada 95 protected type
  -- Michael B. Feldman, The George Washington University,
  -- July, 1995.
 
  protected body Stick is

    entry PIck_Up when not In_Use is
    begin
      In_Use := True;
    end PIck_Up;

    procedure Put_Down is
    begin
      In_Use := False;
    end Put_Down;

  end Stick;
 
end Chop;
