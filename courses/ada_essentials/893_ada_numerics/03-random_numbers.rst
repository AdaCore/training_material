================
Random Numbers
================

-------------------------------------------------
Differences Between Discrete and Floating Point
-------------------------------------------------

* Two packages for random number generation

  * :ada:`Ada.Numerics.Discrete_Random`

    * Generic package that needs to be instantiated with a discrete type (typically integer-based, but could be an enumeral)
    * Function :ada:`Random` returns a value within the discrete type

  * :ada:`Ada.Numerics.Float_Random`

    * Not generic
    * Function :ada:`Random` returns a value between 0.0 and 1.0
    * Treat return value as a fraction of the range

---------------------
Randon Number Usage
---------------------

.. code:: Ada

  with Ada.Numerics.Discrete_Random;
  with Ada.Numerics.Float_Random;
  with Ada.Text_IO;
  use Ada.Text_IO;
  procedure Main is
    type Count_T is range 1 .. 10;
    type Value_T is digits 6 range 0.0 .. 1_000.0;
    -- Create instance for an integer-based random number
    package I_Random is new Ada.Numerics.Discrete_Random (Count_T);
    -- Use a rename to simplify floating point random number
    package F_Random renames Ada.Numerics.Float_Random;
    -- Generators keep track for pseudo-random algorithm
    I_Generator : I_Random.Generator;
    F_Generator : F_Random.Generator;
    V           : Value_T;
  begin
    -- Intialize generators
    I_Random.Reset (I_Generator);
    F_Random.Reset (F_Generator);
    -- Loop a random number of times
    for I in 1 .. I_Random.Random (I_Generator) loop
      -- Print a random floating point number
      V := Value_T (F_Random.Random (F_Generator)) * Value_T'Last;
      Ada.Text_IO.Put_Line (Count_T'Image (I) & " => " & Value_T'Image (V));
    end loop;
  end Main;

