..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

       generic
          type L is limited private;
          type P is private;
       procedure G_P;
       
       type Lim is limited null record;
       type Int is new Integer;
    
       type Rec is record
          L : Lim;
          I : Int;
       end record;

Which declaration(s) is(are) legal?

A. :answermono:`procedure P is new G_P (Lim, Int)`
B. ``procedure P is new G_P (Int, Rec)``
C. ``procedure P is new G_P (Rec, Rec)``
D. :answermono:`procedure P is new G_P (Int, Int)`
