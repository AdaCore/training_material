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

    When V is declared without specifying its size, it becomes mutable,
    at this point the :ada:`S'Length = Positive'Last`, causing a Runtime_Error.
    Furthermore the length of "Hello" is 5, it cannot be stored in a String of Length 6.
