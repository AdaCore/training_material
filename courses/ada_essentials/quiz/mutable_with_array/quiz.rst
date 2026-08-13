..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    type R (Size : Integer := 0) is record
       S : String (1 .. Size);
    end record;

Which of the following propositions will compile and run without error? (Select all that apply)

A. ``V : R := (6, "Hello")``
B. ``V : R := (5, "Hello")``
C. :answermono:`V : R (5) := (5, S => "Hello")`
D. ``V : R (6) := (6, S => "Hello")``

.. container:: animate

    Choices **A** and **B** are mutable: the runtime assumes :ada:`Size`
    can be :ada:`Positive'Last`, so component :ada:`S` will cause a run-time error.
    Choice **D** tries to copy a 5-character string into a 6-character string,
    also generating a run-time error.
