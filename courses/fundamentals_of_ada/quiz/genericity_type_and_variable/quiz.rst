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

A. :answermono:`procedure P is new G_P (I, X)`
B. :answermono:`procedure P is new G_P (E, Y)`
C. ``procedure P is new G_P (I, E'Pos (Y))``
D. ``procedure P is new G_P (F, Z)``
