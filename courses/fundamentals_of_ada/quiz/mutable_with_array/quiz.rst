..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    type R (Size : Integer := 0) is record
       S : String (1 .. Size);
    end record;

Which proposition(s) will compile and run without error?

A. ``V : R := (6, "Hello")``
B. ``V : R := (5, "Hello")``
C. :answermono:`V : R (5) := (5, S => "Hello")`
D. ``V : R (6) := (6, S => "Hello")``

.. container:: animate

    Choices **A** and **B** are mutable: the runtime assumes :ada:`Size`
    can be :ada:`Positive'Last`, so field :ada:`S` will cause a runtime error.
    Choice **D** tries to copy a 5-character string into a 6-character string,
    also generating a runtime error.
