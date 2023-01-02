..

    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

       generic
          type T is (<>);
          G_A : in out T;
       procedure G_P;
    
       type I is new Integer;
       type E is (OK, NOK);
       type F is new Float;
       X : I;
       Y : E;
       Z : F;

Which of the following piece(s) of code is(are) legal?

A. :answermono:`procedure P is new G_P (I, X)`
B. :answermono:`procedure P is new G_P (E, Y)`
C. ``procedure P is new G_P (I, E'Pos (Y))``
D. ``procedure P is new G_P (F, Z)``
