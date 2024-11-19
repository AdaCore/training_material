--::::::::::
--chop.ads
--::::::::::
package Chop is

  -- Dining Philosophers - Ada 95 edition
  -- Chopstick is an Ada 95 protected type
  -- Michael B. Feldman, The George Washington University,
  -- July, 1995.
 
  protected type Stick is
    entry Pick_Up;
    procedure Put_Down;
  private
    In_Use: Boolean := False;
  end Stick;
 
end Chop;
