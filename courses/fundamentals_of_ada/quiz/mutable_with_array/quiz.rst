.. code:: Ada

    type R (Size : Integer := 0) is record
       S : String (1 .. Size);
    end record;

Which proposition(s) will compile and run without error?

A. ``V1 : R := (6, "Hello")``
B. ``V1 : R := (5, "Hello")``
C. :answermono:`V1 : R (5) := (5, S => "Hello")`
D. ``V1 : R (6) := (6, S => "Hello")``
