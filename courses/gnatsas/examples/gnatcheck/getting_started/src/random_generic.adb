--::::::::::
--random_generic.adb
--::::::::::
with Ada.Numerics.Discrete_Random;
package body Random_Generic is
 
  -- Body of random number generator package.
  -- Uses Ada 95 random number generator; hides generator parameters
  -- Michael B. Feldman, The George Washington University, 
  -- June 1995.
 
  package Ada95_Random is new Ada.Numerics.Discrete_Random
    (Result_Subtype => Result_Subtype);

  G: Ada95_Random.Generator;

  function Random_Value return Result_Subtype is 
  begin
    return Ada95_Random.Random(Gen => G);
  end Random_Value;

begin

  Ada95_Random.Reset(Gen => G);  -- time-dependent initialization

end Random_Generic;
