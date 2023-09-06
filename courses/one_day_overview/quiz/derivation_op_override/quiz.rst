..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

       type T1 is new Integer;
       function "+" (A : T1) return T1 is (0);
       type T2 is new T1;
       type T3 is new T1;
       overriding function "+" (A : T3) return T3 is (1);
    
       O1 : T1;
       O2 : T2;
       O3 : T3;

Which proposition(s) is(are) legal and running without error?

A. :answermono:`pragma Assert (+O1 = 0)`
B. :answermono:`pragma Assert (+O2 = 0)`
C. ``pragma Assert ((+O2) + (+O3) = 1)``
D. :answermono:`pragma Assert (+(T3 (O1) + O3) = 1)`

.. container:: animate

    C. ``+O2`` returns a ``T2``, ``+O3`` a ``T3``
