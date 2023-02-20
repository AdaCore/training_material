..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

       generic
          type T is tagged;
          type T2;
       procedure G_P;
    
       type Tag is tagged null record;
       type Arr is array (Positive range <>) of Tag;

Which declaration(s) is(are) legal?

A. :answermono:`procedure P is new G_P (Tag, Arr)`
B. ``procedure P is new G_P (Arr, Tag)``
C. :answermono:`procedure P is new G_P (Tag, Tag)`
D. ``procedure P is new G_P (Arr, Arr)``
