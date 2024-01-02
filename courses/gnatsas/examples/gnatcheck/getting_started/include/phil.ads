--::::::::::
--phil.ads
--::::::::::
with Society;
package Phil is
 
  -- Dining Philosophers - Ada 95 edition
  -- Philosopher is an Ada 95 task type with discriminant
  -- Michael B. Feldman, The George Washington University,
  -- July, 1995.

  task type Philosopher (My_ID : Society.Unique_DNA_Codes) is
 
    entry Start_Eating (Chopstick1 : in Positive;
                        Chopstick2 : in Positive);
 
  end Philosopher;
 
  type States is (Breathing, Thinking, Eating, Done_Eating, 
                  Got_One_Stick, Got_Other_Stick, Dying);

end Phil;
