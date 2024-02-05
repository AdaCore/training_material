..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    package P is
       Object_One : Integer;
       procedure One (V : out Integer);
    end P;

Which completion(s) is(are) correct for :ada:`package P`?

A. ``No completion is needed``
B. | :answermono:`package body P is`
   |    :answermono:`procedure One (V : out Integer) is null;`
   | :answermono:`end P;`
C. | ``package body P is``
   |    ``Object_One : Integer;``
   |    ``procedure One (V : out Integer) is``
   |    ``begin``
   |       ``V := Object_One;``
   |    ``end One;``
   | ``end P;``
D. | :answermono:`package body P is`
   |    :answermono:`procedure One (V : out Integer) is`
   |    :answermono:`begin`
   |       :answermono:`V := Object_One;`
   |    :answermono:`end One;`
   | :answermono:`end P;`

.. container:: animate

    A. Procedure One must have a body
    B. Parameter V is :ada:`out` but not assigned (legal but not a good idea)
    C. Redeclaration of Object_One
    D. Correct
