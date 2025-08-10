..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    type T is limited record
       I : Integer;
    end record;

Which  of the following declaration(s) is (are) legal?

A. ``function "+" (A : T) return T is (A)``
B. :answermono:`function "-" (A : T) return T is (I => -A.I)`
C. :answermono:`function "=" (A, B : T) return Boolean is (True)`
D. :answermono:`function "=" (A, B : T) return Boolean is (A.I = T'(I => B.I).I)`

.. container:: animate

    A. Returning a copy of a limited object is not allowed
    B. Creating a new object
    C. No actual comparison happening
    D. Comparing components is allowed
