.. code:: Ada

    type T is new Integer;

Which operator(s) definition(s) is legal?

A. ``function "+" (V : T) return Boolean is (T /= 0)``
B. :answermono:`function "+" (A, B : T) return T is (A + B)`
C. :answermono:`function "=" (A, B : T) return T is (A - B)`
D. ``function ":=" (A : T) return T is (A)``

.. container:: animate

    B. Infinite recursion
    D. Unlike some languages, there is no assignment operator
