--::::::::::
--society.ads
--::::::::::
package Society is

  -- Dining Philosophers - Ada 95 edition
  -- Society gives unique ID's to people, and registers their names
  -- Michael B. Feldman, The George Washington University,
  -- July, 1995.

  subtype Unique_DNA_Codes is Positive range 1..5;

  Name_Register : array(Unique_DNA_Codes) of String(1..18) :=

     ("Edsger Dijkstra   ",
      "Bjarne Stroustrup ",
      "Chris Anderson    ",
      "Tucker Taft       ",
      "Jean Ichbiah      ");

end Society;
