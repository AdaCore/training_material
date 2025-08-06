..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

       type T is limited record
          I : Integer;
       end record;
    
       L1, L2 : T;
       B : Boolean;

Which statement(s) is (are) legal?

A. :answermono:`L1.I := 1`
B. ``L1 := L2``
C. ``B := (L1 = L2)``
D. :answermono:`B := (L1.I = L2.I)`
