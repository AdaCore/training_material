..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

   generic
      type Element_T is (<>);
      Last : in out Element_T;
   procedure Write (P : Element_T);

   Numeric        : Integer;
   Enumerated     : Boolean;
   Floating_Point : Float;

Which of the following piece(s) of code is(are) legal?

A. :answermono:`procedure P is new G_P (I, X)`
B. :answermono:`procedure P is new G_P (E, Y)`
C. ``procedure P is new G_P (I, E'Pos (Y))``
D. ``procedure P is new G_P (F, Z)``

.. container:: animate

  A. Legal
  B. Legal
  C. The second generic parameter has to be a variable
  D. The first generic parameter has to be discrete
