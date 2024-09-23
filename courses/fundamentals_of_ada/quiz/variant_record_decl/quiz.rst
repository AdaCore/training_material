..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

       type Shape_Kind is (Circle, Line);
    
       type Shape (Kind : Shape_Kind) is record
          case Kind is
             when Line =>
                X, Y : Float;
                X2, Y2 : Float;

Which declaration(s) is (are) legal?

A. | ``when Circle =>``
   |    ``Cord : Shape (Line);``
B. | ``when Circle =>``
   |    ``Center : array (1 .. 2) of Float;``
   |    ``Radius : Float;``
C. | :answermono:`when Circle =>`
   |    :answermono:`Center_X, Center_Y : Float;`
   |    :answermono:`Radius : Float;`
D. | ``when Circle =>``
   |    ``X, Y, Radius : Float;``

.. container:: animate

    A. Referencing itself
    B. anonymous array in record declaration
    C. OK
    D. X, Y are duplicated with the Line variant
