--::::::::::
--phil.adb
--::::::::::
with Society;
with Room;
with Random_Generic;
package body Phil is
 
  -- Dining Philosophers - Ada 95 edition
  -- Philosopher is an Ada 95 task type with discriminant.

  -- Chopsticks are assigned by a higher authority, which
  --   can vary the assignments to show different algorithms.
  -- Philosopher always grabs First_Grab, then Second_Grab.
  -- Philosopher is oblivious to outside world, but needs to
  --   communicate is life-cycle events the Maitre_D.

  -- Michael B. Feldman, The George Washington University,
  -- July, 1995.

  subtype Think_Times is Positive range 1..8;
  package Think_Length is 
    new Random_Generic (Result_Subtype => Think_Times);

  subtype Meal_Times is Positive range 1..10;
  package Meal_Length is
    new Random_Generic (Result_Subtype => Meal_Times);

  task body Philosopher is  -- My_ID is discriminant
 
    subtype Life_Time is Positive range 1..5;
 
    Who_Am_I    : Society.Unique_DNA_Codes := My_ID; -- discrim
    First_Grab  : Positive;
    Second_Grab : Positive;
    Meal_Time   : Meal_Times; 
    Think_Time  : Think_Times;
 
  begin

     -- get assigned the first and second chopsticks here

    accept Start_Eating (Chopstick1 : in Positive;
                         Chopstick2 : in Positive) do
      First_Grab  := Chopstick1;
      Second_Grab := Chopstick2;
    end Start_Eating;
 
    Room.Maitre_D.Report_State (Who_Am_I, Breathing);
 
    for Meal in Life_Time loop
 
      Room.Sticks (First_Grab).Pick_Up;
      Room.Maitre_D.Report_State (Who_Am_I, Got_One_Stick, First_Grab);
 
      Room.Sticks (Second_Grab).Pick_Up;
      Room.Maitre_D.Report_State (Who_Am_I, Got_Other_Stick, Second_Grab);
 
      Meal_Time := Meal_Length.Random_Value;
      Room.Maitre_D.Report_State (Who_Am_I, Eating, Meal_Time, Meal);
 
      delay Duration (Meal_Time);
 
      Room.Maitre_D.Report_State (Who_Am_I, Done_Eating);

      Room.Sticks (First_Grab).Put_Down;
      Room.Sticks (Second_Grab).Put_Down;
 
      Think_Time := Think_Length.Random_Value; 
      Room.Maitre_D.Report_State (Who_Am_I, Thinking, Think_Time);
      delay Duration (Think_Time);
 
    end loop;
 
    Room.Maitre_D.Report_State (Who_Am_I, Dying);

  end Philosopher;
 
end Phil;
