--::::::::::
--random_generic.ads
--::::::::::
generic
  type Result_Subtype is (<>);
package Random_Generic is
 
  -- Simple integer pseudo-random number generator package.
  -- Michael B. Feldman, The George Washington University, 
  -- June 1995.
 
  function Random_Value return Result_Subtype;  
 
end Random_Generic;
