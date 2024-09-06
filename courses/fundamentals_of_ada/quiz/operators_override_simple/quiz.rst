..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    type T is new Integer;

Which operator(s) definition(s) is (are) legal?

A. :answermono:`function "+" (V : T) return Boolean is (V /= 0)`
B. ``function "+" (A, B : T) return T is (A + B)``
C. :answermono:`function "=" (A, B : T) return T is (A - B)`
D. ``function ":=" (A : T) return T is (A)``

.. container:: animate

    B. Infinite recursion
    D. Unlike some languages, there is no assignment operator
